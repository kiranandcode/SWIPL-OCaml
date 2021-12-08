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

module Raw = Raw

let module_sep = lazy (Raw.Functor.functor_ (Raw.Atom.atom ":") 2)
let conjunction = lazy (Raw.Functor.functor_ (Raw.Atom.atom ",") 2)
let disjunction = lazy (Raw.Functor.functor_ (Raw.Atom.atom ";") 2)
let apply = lazy (Raw.Functor.functor_ (Raw.Atom.atom "apply") 2)
let call1 = lazy (Raw.Functor.functor_ (Raw.Atom.atom "call") 1)

type ty  =
  [ `Atom | `Blob | `Bool | `Dict | `Float | `Integer
  | `ListPair | `Nil | `Rational | `String | `Term | `Variable ]

type ctx = Raw.ForeignFrame.t option
type query = Raw.Query.qid
type module_ = Raw.Module.t [@@deriving eq, ord]
let module_ str = Raw.Module.new_module (Raw.Atom.atom str)
type fn = Raw.Functor.t [@@deriving eq, ord]

type atom = Raw.Atom.t [@@deriving eq, ord]
type t =
  | Term of Raw.Term.t
  | Atom of Raw.Atom.t
  | App of Raw.Module.t option * Raw.Functor.t * t list
  | Conjunction of t * t
  | Disjunction of t * t
[@@deriving eq, ord]

let create_term (_ctx: ctx) =
  Raw.Term.new_ref ()

let create_terms (_ctx: ctx) n =
  Raw.Term.new_refs n


let of_term term = Term term

let encode_string ctx str =
  let t = create_term ctx in
  assert(Raw.Term.put_string_chars t str);
  t

let encode_list ctx ls =
  let rec loop result = function
    | [] -> assert (Raw.Term.put_nil result); result
    | h :: t ->
      let result = loop result t in
      assert (Raw.Term.cons_list result h result);
      result in
  loop (create_term ctx) ls

let array_of_list ctx args =
  let length = List.length args in
  if length = 0
  then Raw.Term.Array.empty
  else begin
    let terms = create_terms ctx length in
    List.iteri (fun ind term ->
      assert (Raw.Term.put_term (Raw.Term.Array.get terms ind) term)) args;
    terms
  end

let atom ctx v =
  let result = create_term ctx in
  Raw.Term.put_atom result v;
  result

let app2 ctx fn arg1 arg2 =
  let result = create_term ctx in
  assert (Raw.Term.cons_functor2 result fn arg1 arg2);
  result

let app ctx fn args =
  let result = create_term ctx in
  let args = array_of_list ctx args in
  assert (Raw.Term.cons_functor result fn args);
  result

let rec to_term ctx = function
  | Term t -> t
  | App (None, fn, args) -> app ctx fn (List.map (to_term ctx) args)
  | App (Some module_, fn, args) ->
    let fn = 
      let lazy sep =  module_sep in
      let module_ = Raw.Module.module_name module_ in
      let fn = Raw.Functor.name fn in
      app2 ctx sep (atom ctx module_) (atom ctx fn) in
    let args = List.map (to_term ctx) args in
    let args = encode_list ctx args in
    let lazy apply = apply in
    app2 ctx apply fn args
  | Conjunction (l,r) ->
    let lazy conjunction = conjunction in
    let l = to_term ctx l in
    let r = to_term ctx r in
    app2 ctx conjunction l r
  | Disjunction (l,r) ->
    let lazy disjunction = disjunction in
    let l = to_term ctx l in
    let r = to_term ctx r in
    app2 ctx disjunction l r
  | Atom a -> atom ctx a

let eval ctx =
  let flags = Raw.Q.(normal + ext_status + pass_exception) in
  function
  | Term t ->
    let lazy call = call1 in
    let pred = Raw.Predicate.pred call in
    Raw.Query.open_query ~flags pred (array_of_list ctx [t])
  | App (module_, fn, args) ->
    let pred = Raw.Predicate.pred ?module_ fn in
    let args = array_of_list ctx (List.map (to_term ctx) args) in
    Raw.Query.open_query ?module_ ~flags pred args
  | Atom a ->
    let fn = Raw.Functor.functor_ a 0 in
    let pred = Raw.Predicate.pred fn in
    Raw.Query.open_query ~flags pred Raw.Term.Array.empty
  | Conjunction (l,r) ->
    let lazy conjunction = conjunction in
    let conjunction = Raw.Predicate.pred conjunction in
    Raw.Query.open_query ~flags conjunction (array_of_list ctx [to_term ctx l; to_term ctx r])
  | Disjunction (l,r) ->
    let lazy disjunction = disjunction in
    let disjunction = Raw.Predicate.pred disjunction in
    Raw.Query.open_query ~flags disjunction (array_of_list ctx [to_term ctx l; to_term ctx r])

let initialise () = Raw.initialise ()

let with_ctx f =
  let frame = Raw.ForeignFrame.open_frame () in
  let res = f (Some frame) in
  Raw.ForeignFrame.close_frame frame;
  res

let show t =
  with_ctx (fun ctx -> to_term ctx t |> Raw.Term.get_chars) |> Option.value ~default:"None"
let pp fmt t = Format.pp_print_string fmt (show t)

let fresh ctx = of_term (create_term ctx)

module Syntax = struct
  let (/@) fn x = Raw.Functor.functor_ (Raw.Atom.atom fn) x
  let (!) x = Atom (Raw.Atom.atom x)
  let app ?module_ fn args = App (module_, fn, args)
  let (&&) l r = Conjunction (l,r)
  let (||) l r = Disjunction (l,r)
end

let fold_solutions fn query =
  let any_seen = ref false in
  let rec loop () =
    match Raw.Query.next_solution query with
    | Raw.Query.Result.Bool true ->
      any_seen := true;
      begin match fn `Solution with
      | Some `Close -> assert (Raw.Query.close_query query)
      | Some `Cut -> assert (Raw.Query.cut_query query)
      | None -> loop ()
      end
    | Raw.Query.Result.Bool false -> assert (Raw.Query.close_query query)
    | Raw.Query.Result.Last ->
      any_seen := true;
      begin match fn `Last with
      | Some `Close -> assert (Raw.Query.close_query query)
      | Some `Cut -> assert (Raw.Query.cut_query query)
      | None -> assert (Raw.Query.close_query query)
      end
    | Raw.Query.Result.Exception  ->
      match Raw.Exception.exn query with
      | None -> assert (Raw.Query.close_query query)
      | Some exn ->
        ignore @@ begin
          try
            fn (`Exception (Term exn))
          with
            e -> (Raw.Exception.clear_exn (); raise e)
        end;
        Raw.Exception.clear_exn ();
        assert (Raw.Query.close_query query) in
  begin
    try
      loop ()
    with
    | e -> assert (Raw.Query.close_query query); raise e
  end;
  !any_seen

let iter_solutions ?(on_error=fun _ -> ()) query fn =
  fold_solutions (function
    | `Exception e -> on_error e; None
    | _ -> fn (); None
  ) query

let first_solution query =
  fold_solutions (function
    | `Exception e -> failwith ("Prolog query raised exception: " ^ show e)
    | `Last -> Some `Cut
    | `Solution -> Some `Cut
  ) query

let last_solution query =
  fold_solutions (function
    | `Exception e -> failwith ("Prolog query raised exception" ^ show e)
    | `Last -> Some `Cut
    | `Solution -> None
  ) query

let call ctx t =
  let query = eval ctx t in
  ignore @@ first_solution query

let extract_list ctx term =
  let term = to_term ctx term in
  let rec loop term = 
    if Raw.Term.get_nil term
    then []
    else
      let hd = create_term ctx in
      let tl = create_term ctx in
      assert (Raw.Term.get_list term hd tl);
      (of_term hd) :: (loop tl) in
  loop term

let atom t = Raw.Atom.atom t

let extract_atom ctx term = Raw.Term.get_atom (to_term ctx term) |> Option.get
let show_atom atom = Raw.Atom.chars atom
let pp_atom fmt atom = Format.pp_print_string fmt (show_atom atom)
let extract_bool ctx term = Raw.Term.get_bool (to_term ctx term) |> Option.get
let extract_int ctx term = Raw.Term.get_integer (to_term ctx term) |> Option.get
let extract_float ctx term = Raw.Term.get_float (to_term ctx term) |> Option.get
let extract_string ctx term = Raw.Term.get_string_chars (to_term ctx term) |> Option.get
let extract_functor ctx term =
  let (name, arity) = Raw.Term.get_name_arity (to_term ctx term) |> Option.get in
  let rec loop acc ind =
    if ind < arity
    then
      let arg = create_term ctx in
      assert (Raw.Term.get_arg (ind + 1) (to_term ctx term) arg);
      loop ((of_term arg) :: acc) (ind + 1)
    else List.rev acc in
  (name, loop [] 0)

let typeof = function
  | Term t -> Raw.Term.term_type t
  | Atom _ -> `Atom
  | App (_, _, _)
  | Conjunction (_, _)
  | Disjunction (_, _) -> `Term

let encode_list ctx args = of_term (List.map (to_term ctx) args |> encode_list ctx)
let encode_string ctx str = of_term (encode_string ctx str)

let load_source txt =
  let user = lazy Syntax.(! "user") in
  let open_string = lazy Syntax.("open_string" /@ 2) in
  let load_files = lazy Syntax.("load_files" /@ 2) in
  let stream = lazy Syntax.("stream" /@ 1) in
  let open_string txt out = Syntax.(app (Lazy.force open_string) [txt; out]) in
  let load_files int opts = Syntax.(app (Lazy.force load_files) [int; opts]) in
  let stream out = Syntax.(app (Lazy.force stream) [out]) in
  let open_source ctx src =
    let out = fresh ctx in
    Syntax.(open_string (encode_string ctx src) out &&
            load_files (Lazy.force user) (encode_list ctx [stream out])) in
  with_ctx (fun ctx ->
    call ctx (open_source ctx txt)
  )
