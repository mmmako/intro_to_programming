(* Autor: Michał Makowski
Reviewer: Wojciech Kłopotek *)

type point = float * float

type kartka = point -> int

(* Funkcja sprawdzająca, czy x i y są równe z dokładnością co do epsilona *)
let (=?) x y =
  let ax = abs_float x in
  let ay = abs_float y in
  let d = abs_float (x -. y) in
  if x = y then true
  else if x = 0. || y = 0. || d < min_float then d < (10. *. epsilon_float)
  else d /. (min (ax +. ay) max_float) < epsilon_float 

(* Porównania co do epsilona *)
let (>=?) x y = x > y || x =? y
let (<=?) x y = x < y || x =? y

(* Iloczyn skalarny (x1, y1) i (x2, y2) *)
let dp (x1, y1) (x2, y2) = x1*.x2 +. y1*.y2

(* Iloczyn wektorowy (x1, y1) i (x2, y2) *)
let cp (x1, y1) (x2, y2) = x1*.y2 -. x2*.y1

(* Kwadrat odległości (x1, y1) i (x2, y2) *)
let dist_sq (x1, y1) (x2, y2) = let d = (x2 -. x1, y2 -. y1) in dp d d

let prostokat (x1, y1) (x2, y2) =
  function (x, y) -> if x1 <=? x && x <=? x2 && y1 <=? y && y <=? y2 then 1
                                                                     else 0

let kolko (x1, y1) r =
  function (x, y) ->
    if dist_sq (x, y) (x1, y1) = infinity then
      if dist_sq (1., 1.) (x1 /. x, y1 /. x) <=? 1. then 1 else 0
    else if dist_sq (x, y) (x1, y1) <=? r *. r then 1 else 0

let zloz (x1, y1) (x2, y2) k =
  function (x, y) ->
    let rel = cp (x2 -. x1, y2 -. y1) (x -. x1, y -. y1) in
    if rel =? 0. then
      k (x, y)
    else if rel < 0. then 0
    else (* rel > 0 *)
      (* odbicie symetryczne (x, y) względem wektora od (x1, y1) do (x2, y2) *)
      let sa = dp (x -. x1, y -. y1) (x2 -. x1, y2 -. y1) in
      let s = sa /. dist_sq (x1, y1) (x2, y2) in
      k (-.x +. 2.*.x1 +. 2.*.(x2 -. x1)*.s, -.y +. 2.*.y1 +. 2.*.(y2 -. y1)*.s) + k (x, y)

let skladaj l k = List.fold_left (fun a el -> zloz (fst el) (snd el) a) k l