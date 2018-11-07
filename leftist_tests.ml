open Leftist;;

let print_q q =
  let rec h q1 a =
    if q1 = empty then a
    else let (v, t) = delete_min q1 in
    h t (v::a)
  in h q [];;

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
  let sorted = time (fun () -> List.sort (fun x y -> compare y x) l) in
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
  let sorted1 = time (fun () -> List.sort (fun x y -> compare y x) l1) in
  let sorted2 = time (fun () -> List.sort (fun x y -> compare y x) l2) in
  Printf.printf "Merging lists\n";
  let sorted = time (fun () -> List.merge (fun x y -> compare y x) sorted1 sorted2) in
(*   print_list l;
  print_list res;
  print_list sorted;
 *)  assert (res = sorted);
  Printf.printf "Test OK\n";;

let () =
  Random.self_init();
  test_add_delete 1000000 100000000;;
  test_join 80000 100000000;;