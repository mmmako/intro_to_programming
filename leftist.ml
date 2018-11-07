(* Autor: Michal Makowski *)
(* Code reviewer: Jakub Zarzycki *)

type 'a queue = Node of 'a queue * 'a * 'a queue * int | Leaf;;

exception Empty;;

let empty = Leaf;;

let rec join q1 q2 =
  match (q1, q2) with
    | Leaf, x | x, Leaf -> x
    | Node (l1, v1, r1, h1), Node (l2, v2, r2, h2) ->
      if v1 > v2 then join q2 q1 else
        let new_r = join r1 q2 in
        if l1 = Leaf then Node (new_r, v1, Leaf, h1) else
          let Node (_, _, _, h_l), Node (_, _, _, h_r) = l1, new_r in
          if h_l < h_r then Node (new_r, v1, l1, h1 + 1) else
            Node (l1, v1, new_r, h1 + 1);;

let add e q = join q (Node (Leaf, e, Leaf, 1));;

let delete_min q =
  match q with
  | Node (l, v, r, h) -> (v, join l r)
  | Leaf -> raise Empty;;

let min_val q =
  match q with
  | Node (_, v, _, _) -> v
  | Leaf -> raise Empty;;

let is_empty q = (q = Leaf);; 