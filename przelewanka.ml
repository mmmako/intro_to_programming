(* Autor: Michał Makowski
Reviewer: Artur Matyjasek *)

(* sprawdza warunki konieczne i trywialne przypadki *)
let latwe_przypadki tabl =
  let rec nwd a b =
    if b = 0 then a
    else nwd b (a mod b) in
  let nwd2 (a, b) (c,d) = (nwd a c, nwd b d) in
  if tabl = [||] then Some 0 else
  let (nwd_poj, nwd_wyn) = Array.fold_left nwd2 (0, 0) tabl in
  if nwd_wyn = 0 then Some 0 else
  if nwd_wyn < nwd_poj || nwd_wyn mod nwd_poj <> 0 then Some (-1)
  else
    let kon = Array.fold_left (fun a (e1, e2) -> a || e1 = e2 || e2 = 0) false tabl in
    if not kon then Some (-1) else None;;

(* koduje stan, tzn. listę par (pojemność, objętość) w unikatową liczbę *)
let nr_stanu = Array.fold_left (fun a (e1, e2) -> a*(e1 + 1) + e2) 0;;

(* tworzy kopię stanu i przelewa w nim z i-tej do j-tej szklanki *)
let przelej tab i j =
  let tabl = Array.copy tab in
  let (poj_i, stan_i) = tabl.(i) in
  let (poj_j, stan_j) = tabl.(j) in
  let prz = min stan_i (poj_j - stan_j) in
  tabl.(i) <- (poj_i, stan_i - prz);
  tabl.(j) <- (poj_j, stan_j + prz);
  tabl;;

(* tworzy kopię stanu i wylewa w nim z i-tej szklanki *)
let wylej tab i =
  let tabl = Array.copy tab in
  tabl.(i) <- (fst tabl.(i), 0);
  tabl;;

(* tworzy kopię stanu i wlewa w nim do i-tej szklanki do pełna *)
let wlej tab i =
  let tabl = Array.copy tab in
  tabl.(i) <- (fst tabl.(i), fst tabl.(i));
  tabl;;

(* wyjątek używany do wyjścia z pętli w przypadku znalezienia wyniku *)
exception Wynik of int;;

let przelewanka tabl =
  match latwe_przypadki tabl with
  | Some wyn -> wyn
  | None ->
    let n = Array.length tabl in
    (* kolejka do bfs *)
    let kol = Queue.create () in
    (* tablica hashująca do przechowywania stanów już odwiedzonych *)
    let odw = Hashtbl.create 1000 in
    (* stan początkowy *)
    let pocz = Array.map (fun (x, _) -> (x, 0)) tabl in
    (* sprawdza, czy stan już odwiedzony - jeśli nie, to dodaje do kolejki *)
    let dodaj st gleb =
      if not (Hashtbl.mem odw (nr_stanu st)) then begin
        Hashtbl.add odw (nr_stanu st) true;
        Queue.push (st, gleb) kol;
      end in
    dodaj pocz 0;
    try (while not (Queue.is_empty kol) do
      let (stan, gleb) = Queue.pop kol in
      if stan = tabl then raise (Wynik gleb) else
      for i = 0 to n - 1 do
        dodaj (wlej stan i) (gleb + 1);
        dodaj (wylej stan i) (gleb + 1);
        for j = 0 to n - 1 do
          if j <> i then begin
            dodaj (przelej stan i j) (gleb + 1);
          end
        done;
      done;
    done; (-1)) with Wynik wynik -> wynik;;