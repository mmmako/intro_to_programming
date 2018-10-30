(*Autor: Michal Makowski*)
(*Code reviewer: Artur Matyjasek*)

(*Pusty = pusty, Pomiedzy (a, b) = [a, b], Poza (a,b) = (-oo, a] u [b, oo) *)
type wartosc = Pomiedzy of float*float | Poza of float*float | Pusty;;

exception Blad_laczenia of wartosc * wartosc;;

let wartosc_dokladnosc x p =
  Pomiedzy (x -. abs_float (x *. p /. 100.), x +. abs_float (x *. p /. 100.));;

let wartosc_od_do x y =
  Pomiedzy (x, y);;

let wartosc_dokladna x =
  Pomiedzy (x, x);;

let in_wartosc x y =
  match x with
  | Pusty -> false
  | Pomiedzy (a, b) -> (a <= y && y <= b)
  | Poza (a, b) -> (y <= a || y >= b);;

let min_wartosc x =
  match x with
  | Pusty -> nan
  | Pomiedzy (a, _) -> a
  | Poza (_, _) -> neg_infinity;;

let max_wartosc x =
  match x with
  | Pusty -> nan
  | Pomiedzy (_, b) -> b
  | Poza (_, _) -> infinity;;

let sr_wartosc x =
  match x with
  | Pomiedzy (a, b) -> (a +. b) /. 2.
  | _ -> nan;;

(* mnozenie dajace 0 zamiast nan*)
let m x y = if classify_float (x *. y) <> FP_nan then x *. y else 0.;;

let min_lista l = List.fold_left min infinity l;;

let max_lista l = List.fold_left max neg_infinity l;;

(* dzieli na przedzialy skladowe *)
let rozbij a =
  match a with
  | Poza (a1, a2) -> (Pomiedzy (neg_infinity, a1), Pomiedzy (a2, infinity))
  | _ -> (a, Pusty);;

let rec polacz a b =
  match a, b with
  | Pusty, x | x, Pusty -> x
  | Pomiedzy (a1, a2), Pomiedzy (b1, b2) ->
      if (a1 = neg_infinity && a2 = infinity) ||
         (b1 = neg_infinity && b2 = infinity) then Pomiedzy (neg_infinity, infinity)
      else if (a1 = neg_infinity && b2 = infinity) ||
         (b1 = neg_infinity && a2 = infinity) then Poza (min a2 b2, max a1 b1) 
      else if max a1 a2 > min b1 b2 then raise (Blad_laczenia (a, b))
      else Pomiedzy (min a1 b1, max a2 b2)
  | Poza (a1, a2), Poza (b1, b2) -> if max a1 b1 >= min a2 b2 then
      Pomiedzy (neg_infinity, infinity)
    else Poza (max a1 b1, min a2 b2)
  | _, _ ->
      let (l1, p1) = rozbij a in
      let (l2, p2) = rozbij b in
      polacz (polacz l1 p2) (polacz l2 p1);; 

let odwroc a =
  match a with
  | Pusty -> Pusty
  | Pomiedzy (0., 0.) -> Pusty
  | Pomiedzy (a1, a2) ->
      if m a1 a2 < 0. then Poza (1. /. a1, 1. /. a2)
      else
        if a1 = 0. then Pomiedzy (1. /. a2, infinity) else
        if a2 = 0. then Pomiedzy (neg_infinity, 1. /. a1) else
        Pomiedzy (1. /. a2, 1. /. a1)
  | Poza (a1, a2) ->
      if m a1 a2 < 0. then Pomiedzy (1. /. a1, 1. /. a2)
      else 
        if a1 = 0. then 
          if a2 = 0. then Pomiedzy (neg_infinity, infinity)
          else Pomiedzy (1. /. a2, infinity)
        else if a2 = 0. then Pomiedzy (neg_infinity, 1. /. a1)
        else Poza (1. /. a2, 1. /. a1);;

let plus a b =
  match a, b with
  | Pusty, x | x, Pusty -> Pusty (*czy na pewno?*)
  | Pomiedzy (a1, a2), Pomiedzy (b1, b2) -> Pomiedzy (a1 +. b1, a2 +. b2)
  | Poza (_, _), Poza (_, _) -> Pomiedzy (neg_infinity, infinity)
  | Poza (a1, a2), Pomiedzy (b1, b2) | Pomiedzy (b1, b2), Poza(a1, a2) ->
      if a2 -. a1 <= b2 -. b1 then Pomiedzy (neg_infinity, infinity)
      else Poza (a1 +. b2, a2 +. b1);;

let minus a b =
  let minus_b =
    match b with
    | Pusty -> Pusty
    | Pomiedzy (b1, b2) -> Pomiedzy (-.b2, -.b1)
    | Poza (b1, b2) -> Poza (-.b2, -.b1)
  in plus a minus_b;;

let rec razy a b =
  match a, b with
  | Pusty, _ | _, Pusty -> Pusty
  | Pomiedzy (a1, a2), Pomiedzy (b1, b2) -> Pomiedzy (
      min_lista [m a1 b1; m a1 b2; m a2 b1; m a2 b2],
      max_lista [m a1 b1; m a1 b2; m a2 b1; m a2 b2])
  | Poza (a1, a2), x| x, Poza (a1, a2) ->
      let (l, p) = rozbij (Poza (a1, a2)) in
      (*Printf.printf "ddd";*)
      polacz (razy l x) (razy p x);;

let podzielic a b = razy a (odwroc b);;