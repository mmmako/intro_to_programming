(* Autor: Michał Makowski *)
(* Reviewer: Artur Matyjasek *)
open Przelewanka

type rodzaj = Dokladnie | CoNajwyzej | Nie;;

let string_of_rodzaj = function
  | Dokladnie  -> "dokładnie"
  | CoNajwyzej -> "co najwyżej"
  | Nie        -> "nie"

let info =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "-info" then true else false

let testuj nazwa tabl ?r:(r = Dokladnie) wynik =
  let t = Sys.time() in
  if info then Printf.printf "Test: \"%s\"" nazwa;
  let wyn = przelewanka tabl in
  if info then print_endline (", czas wykonania: " ^ string_of_float (Sys.time() -. t) ^ "s");
  let popr = match r with
    | Dokladnie  -> wyn = wynik
    | CoNajwyzej -> wyn <= wynik
    | Nie        -> wyn <> wynik in
  if not popr then begin
    Printf.printf "Nieudany test \"%s\". Oczekiwano %s %d, otrzymano: %d\n" nazwa (string_of_rodzaj r) wynik wyn;
    assert false
  end;;

testuj "pusty" [||] 0;;
testuj "jeden element, gotowy" [|(42, 0)|] 0;;
testuj "jeden element" [|(42, 42)|] 1;;
testuj "jedno zero" [|(0, 0)|] 0;;
testuj "dużo samych zer" (Array.create 1000000 (0, 0)) 0;;

testuj "bez pełnych, z pustym" [|(4, 0); (5, 4); (10, 9)|] ~r:Nie (-1);;
testuj "bez pustych, z pełnym" [|(4, 2); (5, 5); (10, 10)|] ~r:Nie (-1);;
testuj "bez pustych i pełnych" [|(4, 2); (5, 3); (100, 10)|] (-1);;

testuj "złe nwd" [|(10, 3); (100, 5); (1000, 1000)|] (-1);;
testuj "złe nwd, duży" (Array.init 10000 (fun n -> (10*n, 3*n))) (-1);;

testuj "\"dużo\" możliwych stanów" (Array.append [|(100, 0); (200, 0); (1111, 300)|] (Array.init 10 (fun n -> (10*n, 0)))) 4;;
testuj "bardzo możliwych stanów, bardzo łatwe" (Array.append [|(1, 1)|] (Array.create 60 (1, 0))) 1;;

testuj "ręcznie sprawdzony 1" [|(5, 3); (9, 0)|] ~r:CoNajwyzej 10;;
testuj "ręcznie sprawdzony 2" [|(5, 4); (7, 7)|] ~r:CoNajwyzej 16;;

Printf.printf "Testy OK\n";;