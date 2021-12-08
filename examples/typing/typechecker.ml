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

let consult file =
  Swipl.(with_ctx @@ fun ctx -> call ctx Syntax.(app ("consult" /@ 1) [! file]))

type t =
  | Lam of string * t
  | Var of string
  | App of t * t

type ty =
  | TyVar of string
  | Fun of ty * ty

let rec show_ty_ ?(needs_parens=false) =
  function TyVar v -> v
         | Fun (frt,tot) ->
           (if needs_parens then "(" else "") ^
           show_ty_ ~needs_parens:true frt ^ " -> " ^
           show_ty_ tot ^
           (if needs_parens then ")" else "")
let show_ty ty = show_ty_ ty

let lam var body = Swipl.Syntax.(app ("lam" /@ 2) [var; body])
let apply fn arg = Swipl.Syntax.(app ("app" /@ 2) [fn; arg])
let typeof gamma term ty = Swipl.Syntax.(app ("typeof" /@ 3) [gamma; term; ty])

let rec encode =
  let open Swipl.Syntax in
  function
  | Lam (var, body) ->
    lam (!var) (encode body)
  | Var v -> !v
  | App (fn,arg) -> apply (encode fn) (encode arg)

module TermMap = Map.Make(struct type t = Swipl.t let compare = Swipl.compare end)

let lookup (map,id) t =
  match TermMap.find_opt t map with
  | Some name -> TyVar name, (map,id)
  | None ->
    let name = "'" ^ String.init 1 (fun _ -> Char.chr (97 + id)) in
    let map = TermMap.add t name map in
    TyVar name, (map, id+1)

let decode ctx =
  let (let+) x f= Option.bind x f in
  let rec loop map t =
    match Swipl.typeof t with
    | `Variable -> Some (lookup map t)
    | `Term ->
      let[@warning "-8"] (_, [froty;toty]) = Swipl.extract_functor ctx t in
      let+ froty, map = loop map froty in
      let+ toty, map = loop map toty in
      Some (Fun (froty, toty), map)
    | _ -> None in
  loop

let typecheck term =
  Swipl.with_ctx (fun ctx ->
    let result = Swipl.fresh ctx in
    let query =
      Swipl.eval ctx
        (typeof Swipl.(encode_list ctx []) (encode term) result) in
    if Swipl.first_solution query
    then decode ctx (TermMap.empty, 0) result |> Option.map fst
    else None
  )

let () =
  let () = Swipl.initialise () in
  consult "./type_checker.pl";
  let term = Lam("x", Lam("y", App(Var "x", App(Var "y", Var "x")))) in
  match typecheck term with
  | None -> print_endline @@ "Term does not type check"
  | Some ty -> print_endline @@ "Term has type " ^ (show_ty ty)
