exception Cykliczne

(* usuwa duplikaty z posortowanej listy *)
let rec usun_dupl = function
  |h1::(h2::_ as t) -> if h1 = h2 then usun_dupl t else h1::usun_dupl t
  |l -> l

(* łączy dwie listy posortowane rosnąco w jedną posortowaną rosnąco *)
let polacz_posortowane l1 l2 =
  let rec pom l1 l2 a =
    match l1, l2 with
    |h1::t1, h2::t2 ->
      if h1 > h2 then pom l1 t2 (h2::a)
      else if h2 > h1 then pom t1 l2 (h1::a)
      else pom t1 t2 (h1::a)
    |h::t, [] |[], h::t -> pom t [] (h::a)
    |[], [] -> a
  in List.rev (pom l1 l2 [])

(* "spłaszcza" listę reprezentującą graf w celu otrzymania listy wierzchołków *)
let splaszcz l = List.fold_left (fun a (w, s) -> w::s@a) [] l

(* wstawia "pusty" wierzchołek o etykiecie w do mapy sąsiadów i oznaczenia *)
let wstaw (sas, ozn) w = (PMap.add w [] sas, PMap.add w 0 ozn)

(* przetwarza jeden wierzchołek z listy reprezentującej graf *)
let dodaj_sasiadow sas (w, l) =
  let obecni = try PMap.find w sas with Not_found -> [] in
  let nowi = usun_dupl (List.sort compare l) in
  PMap.add w (polacz_posortowane obecni nowi) sas

let topol lista =
  let wierzch = lista |> splaszcz |> (List.sort compare) |> usun_dupl in
  let (graf, ozn) = List.fold_left wstaw (PMap.empty, PMap.empty) wierzch in
  let graf = List.fold_left dodaj_sasiadow graf lista in
  let rec dfs (odw, l) w =
    match PMap.find w odw with
    |1 -> raise Cykliczne
    |2 -> (odw, l)
    |0 ->
      let odw = PMap.add w 1 odw in
      let (odw, l) = List.fold_left dfs (odw, l) (PMap.find w graf) in
      let odw = PMap.add w 2 odw in
      let l = w::l in
      (odw, l)
    |_ -> assert false in
  let (_, wynik) = PMap.foldi (fun k _ a -> dfs a k) graf (ozn, []) in
  wynik