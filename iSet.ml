(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(*
* TODO:
* -node count in tree
* -overlap
* -max_int, min_int
*)

type interval = int * int

type t =
  | Empty
  | Node of t * interval * t * int * int

let cmp (s1, e1) (s2, e2) =
  if s2 = min_int || e1 < s2 - 1 then (-1)
  else if s1 = min_int || e2 < s1 - 1 then 1
  else assert false 

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let cnt = function
  | Node (_, _, _, _, c) -> c
  | Empty -> 0

let (+?) x y =
  if (y >= 0 && x + y < 0) || (y < 0 && x + y > 0) then max_int
  else x + y
(*   if x >= 0 && y > max_int - x then max_int
  else if x < 0 && y < min_int - x then max_int
  else x + y
 *)

let len (b, e) = e +? (-b) +? 1 

let make l (b, e) r =
  Node (l, (b, e), r, max (height l) (height r) + 1, cnt l +? cnt r +? len (b, e))

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec max_elt = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let rec remove_max_elt = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_max_elt"

let rec add_disjoint (b, e) = function
  | Node (l, k, r, h, c) ->
      let cm = cmp (b, e) k in
      if cm = 0 then Node (l, (b, e), r, h, c)
      else if cm < 0 then
        let nl = add_disjoint (b, e) l in
        bal nl k r
      else
        let nr = add_disjoint (b, e) r in
        bal l k nr
  | Empty -> Node (Empty, (b, e), Empty, 1, len (b, e))

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_disjoint v r
  | (_, Empty) -> add_disjoint v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

let concat t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
    let k = min_elt t2 in
    join t1 k (remove_min_elt t2)

let empty = Empty

let is_empty x = x = Empty

let split x s  =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, (b, e), r, _, _) ->
        if x < b then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl (b, e) r)
        else if x > e then
          let (lr, pres, rr) = loop x r in (join l (b, e) lr, pres, rr)
        else (* b <= x <= e *)
          if x = b then
            if x = e then
              (l, true, r)
            else
              (l, true, add_disjoint (b + 1, e) r)                           (* CHECK *)
          else if x = e then
            if e > min_int then (add_disjoint (b, e - 1) l, true, r)
            else (l, true, r)
          else
            let nl =
              if x > min_int then add_disjoint (b, x - 1) l
              else l in
            let nr =
              if x < max_int then add_disjoint (x + 1, e) r
              else r in
            (nl, true, nr)
  in
    loop x s

let remove (b, e) s =
  let (l, _, _) = split b s in
  let (_, _, r) = split e s in
  concat l r

let add (b, e) s =
  let (l, _, _) = split b s in
  let (_, _, r) = split e s in
(*   match l, r with
    | Empty, Empty -> join l (b, e) r
    | Empty, _ ->
        let (rmb, rme) = min_elt r in
        let nr =
          if rmb = e + 1 then
            add_disjoint (b, rme) (remove_min_elt r) in
          else r
        in concat l nr
    | _, Empty ->
        let (lmb, lme) = max_elt l in
        let nl =
          if lme + 1 = b then
            let nl = add_disjoint (lmb, e) (remove_max_elt k) in
          else l
        in concat nl r
    | _ ->
        let (rmb, rme) = min_elt r in
        let nr =
          if rmb = e + 1 then
            add_disjoint (b, rme) (remove_min_elt r) in
          else r
        in
        let (lmb, lme) = max_elt l in
        let nl =
          if lme + 1 = b then
            let nl = add_disjoint (lmb, e) (remove_max_elt k) in
          else l
        in concat nl r
 *)  
  let ((b, e), nl) =
    if l = Empty then ((b, e), l)
    else
      let (lmb, lme) = max_elt l in
        if lme < max_int && lme + 1 = b then                     (* CHECK *)
          ((lmb, e), remove_max_elt l)
        else ((b, e), l)
  in
  let ((b, e), nr) =
    if r = Empty then ((b, e), r)
    else
      let (rmb, rme) = min_elt r in
        if rmb > min_int && rmb - 1 = e then                     (* CHECK *)
          ((b, rme), remove_min_elt r)
        else ((b, e), r)
  in join nl (b, e) nr

let mem x s =
  let rec loop = function
    | Node (l, (b, e), r, _, _) ->
        let c = (b <= x && x <= e) in
        c = true || loop (if x < b then l else r)
    | Empty -> false in
  loop s

let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, (b, e), r, _, _) -> loop l; f (b, e); loop r in
  loop s

let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc s

let elements s = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, (b, e), r, _, _) -> loop ((b, e) :: loop acc r) l in
  loop [] s

let rec below x s =
  match s with
  | Empty -> 0
  | Node (l, (b, e), r, _, c) ->
      if x < b then below x l
      else if x <= e then cnt l +? len (b, x)
      else len (b, e) +? cnt l +? below x r