open Topol

let info =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "-info" then true else false

let rec usun_dupl = function
  |h1::(h2::_ as t) -> if h1 = h2 then usun_dupl t else h1::usun_dupl t
  |l -> l

(* laczy dwie listy posortowane ściśle rosnąco w jedną posortowaną ściśle rosnąco *)
let polacz_posortowane l1 l2 =
  let rec pom l1 l2 a =
    match l1, l2 with
    |h1::t1, h2::t2 ->
      if h1 > h2 then pom l1 t2 (h2::a)
      else if h2 > h1 then pom t1 l2 (h1::a)
      else pom t1 t2 (h1::a)
    |h::t, [] |[], h::t -> pom t [] (h::a)
    |[], [] -> a
  in List.rev (pom l1 l2 []);;

(* sprawdza, czy lista jest poprawnym sortowaniem topologicznym grafu *)
let sprawdz_wynik lista top =
  let wierzch = List.fold_left (fun a (w, l) -> w::l@a) [] lista in
  let wierzch = usun_dupl (List.sort compare wierzch) in
  let wstaw (g, o) w = (PMap.add w [] g, PMap.add w 0 o) in
  let (graf, ozn) = List.fold_left wstaw (PMap.empty, PMap.empty) wierzch in
  let pom s (w, l) =
    let obecni = try PMap.find w s with Not_found -> [] in
    let nowi = usun_dupl (List.sort compare l) in
    PMap.add w (polacz_posortowane obecni nowi) s in 
  let graf = List.fold_left pom graf lista in
  if (List.sort compare wierzch) <> (List.sort compare top) then false
  else
    let sprawdz l_prz w =
      let pom a el = a || (List.mem el l_prz) in
      List.fold_left pom false (PMap.find w graf) in
    snd (List.fold_left (fun (l_prz, popr) w -> (w::l_prz, popr && not (sprawdz l_prz w))) ([], true) top)

let sprawdz_topol lista = sprawdz_wynik lista (topol lista)

let testuj nazwa gr czyCykliczny =
  let t = Sys.time() in
  if info then Printf.printf "Test: \"%s\"" nazwa;
  let wynik = (try if (sprawdz_topol gr) then 1 else 0 with Cykliczne -> 2) in
  if info then Printf.printf ", czas wykonania: %fs\n" (Sys.time() -. t);
  if not ((wynik = 2 && czyCykliczny) || (wynik = 1 && not czyCykliczny)) then begin
    Printf.printf "Nieudany test \"%s\"\n" nazwa;
    assert false
  end;;

let drzewo_binarne h =
  let wynik = ref [] in
  let n = 1 lsl h in
  let i = ref 0 in
  while !i < n - 1 do
    let _ = if !i < (n - 2) / 2 then
      wynik := (!i, [!i * 2 + 1; !i * 2 + 2])::!wynik
    else wynik := (!i, [])::!wynik in
    i := !i + 1
  done;
  !wynik;;

testuj "pusty" [] false;;
testuj "jeden element" [(1, [])] false;;
testuj "jeden element, cykliczny" [(1, [1])] true;;
testuj "mały" [(5, [11]); (11, [2;9;10]); (2, []); (7, [11;8]); (8, [9]); (3, [8; 10]); (9, []); (10, [])] false;;
testuj "mały, string" [("pięć", ["jedenaście"]); ("jedenaście", ["dwa";"dziewięć";"dziesięć"]); ("dwa", []); ("siedem", ["jedenaście";"osiem"]); ("osiem", ["dziewięć"]); ("trzy", ["osiem"; "dziesięć"]); ("dziewięć", []); ("dziesięć", [])] false;;
testuj "trzy, cylkiczny" [("a", ["b"]); ("b", ["c"]); ("c", ["a"])] true;;
testuj "prosta" [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", ["e"]); ("e", ["f"]); ("f", [])] false;;
testuj "prosta z pętlą na końcu" [("a", ["b"]); ("b", ["c"]); ("c", ["d"]); ("d", ["e"]); ("e", ["f"]); ("f", ["d"])] true;;
testuj "małe drzewo binarne" [(0, [1; 2]); (1, [3; 4]); (2, [5; 6]); (3, [7; 8]); (4, [9; 10]); (5, [11; 12]); (6, [13; 14]); (7, []); (8, []); (9, []); (10, []); (11, []); (12, []); (13, []); (14, [])] false;;
testuj "małe drzewo binarne - cykl" [(0, [1; 2]); (1, [3; 4]); (2, [5; 6]); (3, [7; 8]); (4, [9; 10]); (5, [11; 12]); (6, [13; 14]); (7, []); (8, []); (9, []); (10, []); (11, []); (12, []); (13, []); (14, [2])] true;;
testuj "nie-cykl" [(0, [1; 2]); (1, [3]); (2, [3]); (3, [])] false;;
let drz = drzewo_binarne 12;;
testuj "większe drzewo binarne" drz false;;
testuj "niejawne wierzchołki" [(1, [2]); (1, [3])] false;;