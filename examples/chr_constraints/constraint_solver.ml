(*
SWIPL-OCaml

Copyright (C) 2021  Kiran Gopinathan

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

type t = Salt | Water | SaltWater [@@deriving show { with_path = false }]
type store = t list [@@deriving show]

let consult file = Swipl.Syntax.(app ("consult" /@ 1) [!file])
let reducesTo goal result = Swipl.Syntax.(app ("reducesTo" /@ 2) [goal; result])
let print_term t = print_endline @@ Swipl.show t

let consult file =
  Swipl.(with_ctx @@ fun ctx -> call ctx (consult file))

let salt = lazy Swipl.Syntax.(!"salt")
let water = lazy Swipl.Syntax.(!"water")
let salt_water = lazy Swipl.Syntax.(!"salt_water")


let encode v = Lazy.force @@ match v with Salt -> salt | Water -> water | SaltWater -> salt_water
let encode ls =
  let hd,tl = List.hd ls, List.tl ls in
  List.fold_left Swipl.Syntax.(fun acc vl -> acc && encode vl) (encode hd) tl 

let decode =
  let salt = lazy (Swipl.atom "salt") and water = lazy (Swipl.atom "water")
  and salt_water = lazy (Swipl.atom "salt_water") in
  fun ctx t ->
  match Swipl.extract_atom ctx t with
  | v when Swipl.equal_atom v (Lazy.force salt) -> Salt
  | v when Swipl.equal_atom v (Lazy.force water) -> Water
  | v when Swipl.equal_atom v (Lazy.force salt_water) -> SaltWater
  | v -> failwith ("unknown atom " ^ Swipl.show_atom v)

let decode ctx t =
  let ls = Swipl.extract_list ctx t in
  List.map (decode ctx) ls

let solve_constraints ls =
  Swipl.with_ctx (fun ctx ->
    let goal = encode ls in
    let result = Swipl.fresh ctx in    
    Swipl.call ctx (reducesTo goal result);
    let result = decode ctx result in
    result
  )
  
let () =
  let () = Swipl.initialise () in
  consult "./constraints.pl";
  let terms = [Salt; Water; Salt; Salt; Salt; Salt; Water; Water] in
  let term = solve_constraints terms in
  print_endline (show_store term);

