type 'a queue = Node of 'a queue * 'a * 'a queue * int | Leaf;;

exception Empty;;

let empty = Leaf;;

let rec join q1 q2 =
  match q1, q2 with
  | Leaf, x | x, Leaf -> x
  | Node (l1, v1, r1, h1), Node (l2, v2, r2, h2) ->
    if v1 < v2 then join q2 q1 else
    let new_r = join r1 q2 in
    if l1 = Leaf then Node (new_r, v1, Leaf, h1) else
    let Node (_, _, _, h_l), Node (_, _, _, h_r) = l1, new_r in
    if h_l < h_r then Node (new_r, v1, l1, h1 + 1) else
    Node (l1, v1, new_r, h1 + 1)

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

let print_q q =
  let rec h q1 a =
    if q1 = empty then a
    else let (v, t) = delete_min q1 in
    h t (v::a)
  in h q [];;

(* check if nonincreasing and count uniques *)
(* let check_sorted l =
  let rec temp l c last nonde =
    match l with
    | [] -> (nonde, c)
    | h :: t ->
        if last = None then temp t 1 (Some h) nonde else
        if Some h > last then temp t c last false else
        if Some h = last then temp t c last nonde else
                         temp t (c + 1) (Some h) nonde
  in temp l 0 None true;;
 *)

let random_list n bnd =
  let rec aux n a =
    if n = 0 then a else
    aux (n - 1) ((Random.int bnd)::a)
  in aux n [];;

let print_list l =
  List.iter (Printf.printf "%d ") l;
  Printf.printf "\n";;

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f seconds\n"
                (Unix.gettimeofday () -. t);
  res;;


let test_add_delete n bnd =
  Printf.printf "Generating random list\n";
  let l = time (fun () -> random_list n bnd) in
  let rec make_queue l a =
    match l with
    | [] -> a
    | h::t -> make_queue t (add h a)
  in
  Printf.printf "Converting to queue\n";
  let q = time (fun () -> make_queue l empty) in
  Printf.printf "Converting back to list\n";
  let res = time (fun () -> print_q q) in
  Printf.printf "Sorting list\n";
  let sorted = time (fun () -> List.sort compare l) in
(*   print_list l;
  print_list res;
  print_list sorted;
 *)  assert (res = sorted);
  Printf.printf "Test OK\n";;

let test_join n bnd =
  Printf.printf "Generating random lists\n";
  let l1 = time (fun () -> random_list n bnd) in
  let l2 = time (fun () -> random_list n bnd) in
  let rec make_queue l a =
    match l with
    | [] -> a
    | h::t -> make_queue t (add h a)
  in
  Printf.printf "Converting to queues\n";
  let q1 = time (fun () -> make_queue l1 empty) in
  let q2 = time (fun () -> make_queue l2 empty) in
  Printf.printf "Joining queues\n";
  let q = time (fun () -> join q1 q2) in
  Printf.printf "Converting back to list\n";
  let res = time (fun () -> print_q q) in
  Printf.printf "Sorting lists\n";
  let sorted1 = time (fun () -> List.sort compare l1) in
  let sorted2 = time (fun () -> List.sort compare l2) in
  Printf.printf "Merging lists\n";
  let sorted = time (fun () -> List.merge compare sorted1 sorted2) in
(*   print_list l;
  print_list res;
  print_list sorted;
 *)  assert (res = sorted);
  Printf.printf "Test OK\n";;

(* let () =
  Random.self_init();
  test_add_delete 1000000 100000000;;
  test_join 80000 100000000;; *)