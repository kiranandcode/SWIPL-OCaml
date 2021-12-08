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

module Bindings = Bindings.Stubs(Bindings_stubs)
open Ctypes


type atom = unit ptr
type functor_ = unit ptr
type term = Unsigned.ulong
type module_  = unit ptr
type predicate = unit ptr


module CVT = struct
  type t = int
  let[@warning "-32"] t  = int 
  external ( + ) : t -> t -> t = "%orint"
  external equal : t -> t -> bool = "%equal"
  include Bindings.CVT
end

module Q = struct
  type t = int
  let t = int
  external ( + ) : t -> t -> t = "%orint"
  external equal : t -> t -> bool = "%equal"
  include Bindings.Q
end

module DB = struct
  type t = int
  let t = int
  external equal : t -> t -> bool = "%equal"
  include Bindings.Database
end

module File = struct
  type t = int
  let t = int
  external ( + ) : t -> t -> t = "%orint"
  external equal : t -> t -> bool = "%equal"
  include Bindings.File
end

module Action = struct
  type[@warning "-34"] t = int
  let t = int
  (* external ( + ) : t -> t -> t = "%orint"
   * external equal : t -> t -> bool = "%equal" *)
  include Bindings.Action
end

module[@warning "-32"] VersionInfo = struct
  type t = int
  let t = int
  external equal : t -> t -> bool = "%equal"
  include Bindings.VersionInfo
end

module Atom = struct

  type t = atom
  let compare l r = ptr_compare l r
  let equal l r = compare l r = 0

  let t : t typ = ptr void

  let atom =
    Foreign.foreign "PL_new_atom" (string @-> returning t)

  let chars =
    Foreign.foreign "PL_atom_chars" (t @-> returning string)

  let register =
    Foreign.foreign "PL_register_atom" (t @-> returning void)    

  let unregister =
    Foreign.foreign "PL_unregister_atom" (t @-> returning void)    

end

module Functor = struct

  type t = functor_

  let compare l r = ptr_compare l r
  let equal l r = compare l r = 0

  let t : t typ = ptr void

  let functor_ =
    Foreign.foreign "PL_new_functor" (Atom.t @-> int @-> returning t)

  let name =
    Foreign.foreign "PL_functor_name" (t @-> returning Atom.t)

  let arity =
    Foreign.foreign "PL_functor_name" (t @-> returning int)

end

module Term = struct
  type t = term

  let t : t typ = ulong

  type repr =
    | Variable of t
    | Atom of Atom.t
    | Bool of bool
    | Nil
    | Blob of t
    | String of string
    | Integer of int
    | Rational of t
    | Float of float
    | Compound of Atom.t * repr list
    | List of repr * repr
    | Dict of t * (atom -> t option)

  module Array = struct
    type nonrec t = term * int
    let t = t

    let empty : t = (Unsigned.ULong.zero, 0)

    let singleton t = (t, 1)

    let to_array (start, n) =
      let refs = Array.make n start in
      for i = 0 to n - 1 do
        refs.(i) <- Unsigned.ULong.(Infix.(refs.(i) + of_int i))
      done;
      refs

    let get_unsafe (start,_) i = Unsigned.ULong.(Infix.(start + of_int i))

    let get (start,n) i = assert (0 <= i && i < n); Unsigned.ULong.(Infix.(start + of_int i))

  end

  let compare  = Foreign.foreign "PL_compare" (t @-> t @-> returning int)
  let equal l r = compare l r = 0
  let (==)  = Foreign.foreign "PL_same_compound" (t @-> t @-> returning bool)

  let new_ref =
    Foreign.foreign "PL_new_term_ref" (void @-> returning t)

  let new_refs =
    Foreign.foreign "PL_new_term_refs" (int @-> returning t)

  let new_refs n : Array.t = new_refs n, n

  let copy =
    Foreign.foreign "PL_copy_term_ref" (t @-> returning t)

  let reset =
    Foreign.foreign "PL_reset_term_refs" (t @-> returning void)

  let get_atom =
    Foreign.foreign "PL_get_atom" (t @-> ptr Atom.t @-> returning bool)

  let deref_if_safe fn vl =
    if fn
    then Some (!@ vl)
    else None

  let deref_if_safe2 fn vl1 vl2 =
    if fn
    then Some (!@ vl1, !@ vl2)
    else None

  let get_chars =
    Foreign.foreign "PL_get_chars" (t @-> ptr string @-> int @-> returning bool)
  let get_chars ?(flags=CVT.all) t =
    let str = allocate string "" in
    deref_if_safe (get_chars t str flags) str

  let get_atom t =
    let atom = allocate Atom.t null in
    deref_if_safe (get_atom t atom) atom

  let get_atom_chars =
    Foreign.foreign "PL_get_atom_chars" (t @-> ptr string @-> returning bool)

  let get_atom_chars t =
    let chars = allocate string "" in
    deref_if_safe (get_atom_chars t chars) chars

  let get_string_chars =
    Foreign.foreign "PL_get_string" (t @-> ptr string @-> ptr int @-> returning bool)

  let get_string_chars t =
    let chars = allocate string "" in
    let len = allocate int 0 in
    deref_if_safe (get_string_chars t chars len) chars

  let get_integer =
    Foreign.foreign "PL_get_integer" (t @-> ptr int @-> returning bool)

  let get_integer t =
    let vl = allocate int 0 in
    deref_if_safe (get_integer t vl) vl

  let get_long =
    Foreign.foreign "PL_get_long" (t @-> ptr long @-> returning bool)

  let get_long t =
    let vl = allocate long Signed.Long.zero in
    deref_if_safe (get_long t vl) vl

  let get_int64 =
    Foreign.foreign "PL_get_int64" (t @-> ptr int64_t @-> returning bool)

  let get_int64 t =
    let vl = allocate int64_t 0L in
    deref_if_safe (get_int64 t vl) vl

  let get_bool =
    Foreign.foreign "PL_get_bool" (t @-> ptr bool @-> returning bool)

  let get_bool t =
    let vl = allocate bool false in
    deref_if_safe (get_bool t vl) vl

  let get_float =
    Foreign.foreign "PL_get_float" (t @-> ptr float @-> returning bool)

  let get_float t =
    let vl = allocate float 0. in
    deref_if_safe (get_float t vl) vl

  let get_functor =
    Foreign.foreign "PL_get_functor" (t @-> ptr Functor.t @-> returning bool)

  let get_functor t =
    let vl = allocate Functor.t null in
    deref_if_safe (get_functor t vl) vl

  let get_name_arity =
    Foreign.foreign "PL_get_name_arity" (t @-> ptr Atom.t @-> ptr int @-> returning bool)

  let get_name_arity t =
    let name = allocate Atom.t null in
    let arity = allocate int 0 in
    deref_if_safe2 (get_name_arity t name arity) name arity

  let get_compound_name_arity =
    Foreign.foreign "PL_get_compound_name_arity" (t @-> ptr Atom.t @-> ptr int @-> returning bool)

  let get_compound_name_arity t =
    let name = allocate Atom.t null in
    let arity = allocate int 0 in
    deref_if_safe2 (get_compound_name_arity t name arity) name arity

  let get_arg =
    Foreign.foreign "PL_get_arg" (int @-> t @-> t @-> returning bool)

  let get_dict_key =
    Foreign.foreign "PL_get_dict_key" (Atom.t @-> t @-> t @-> returning bool)

  let get_list =
    Foreign.foreign "PL_get_list" (t @-> t @-> t @-> returning bool)

  let get_head =
    Foreign.foreign "PL_get_head" (t @-> t @-> returning bool)

  let get_tail =
    Foreign.foreign "PL_get_tail" (t @-> t @-> returning bool)

  let get_nil = Foreign.foreign "PL_get_nil" (t @-> returning bool)

  let term_type =
    Foreign.foreign "PL_term_type" (t @-> returning int)
      
  let term_type t =
    match term_type t with
    | v when v = Bindings.Term.pl_variable -> `Variable
    | v when v = Bindings.Term.pl_atom -> `Atom
    | v when v = Bindings.Term.pl_bool -> `Bool
    | v when v = Bindings.Term.pl_nil -> `Nil
    | v when v = Bindings.Term.pl_blob -> `Blob
    | v when v = Bindings.Term.pl_string -> `String
    | v when v = Bindings.Term.pl_integer -> `Integer
    | v when v = Bindings.Term.pl_rational -> `Rational
    | v when v = Bindings.Term.pl_float -> `Float
    | v when v = Bindings.Term.pl_term -> `Term
    | v when v = Bindings.Term.pl_list_pair -> `ListPair
    | v when v = Bindings.Term.pl_dict -> `Dict
    | d -> failwith ("Unknown term type: " ^ string_of_int d)

  let rec get t =
    match term_type t with
    | `Variable -> Variable t
    | `Atom -> Atom (get_atom t |> Option.get)
    | `Bool -> Bool (get_bool t |> Option.get)
    | `Nil -> Nil
    | `Blob -> Blob t
    | `String -> String (get_string_chars t |> Option.get)
    | `Integer -> Integer (get_integer t |> Option.get)
    | `Rational -> Rational t
    | `Float -> Float (get_float t |> Option.get)
    | `Term ->
      let name, arity = get_name_arity t |> Option.get in
      let arr = new_refs arity in
      let args =
        List.init arity (fun ind ->
          let arg = Array.get_unsafe arr ind in
          assert (get_arg ind t arg);
          get arg
        ) in
      Compound (name, args)
    | `ListPair ->
      let ts = new_refs 2 in
      let hd = Array.get_unsafe ts 0 in
      let tl = Array.get_unsafe ts 1 in
      assert (get_list t hd tl);
      List (get hd, get tl)
    | `Dict ->
      let lookup key =
        let out = new_ref () in
        if get_dict_key key t out
        then Some out
        else (reset out; None) in
      Dict (t, lookup)

  (** Returns non-zero if term is a variable.  *)
  let is_variable = Foreign.foreign "PL_is_variable" (t @-> returning bool)

  (** Returns non-zero if term is a ground term. See also ground/1. This function is cycle-safe.  *)
  let is_ground = Foreign.foreign "PL_is_ground" (t @-> returning bool)

  (** Returns non-zero if term is an atom. *)
  let is_atom = Foreign.foreign "PL_is_atom" (t @-> returning bool)

  (** Returns non-zero if term is a string. *)
  let is_string = Foreign.foreign "PL_is_string" (t @-> returning bool)

  (** Returns non-zero if term is an integer. *)
  let is_integer = Foreign.foreign "PL_is_integer" (t @-> returning bool)

  (** Returns non-zero if term is a rational number (P/Q). Note that all integers are considered rational and this test thus succeeds for any term for which PL_is_integer() succeeds. See also PL_get_mpq() and PL_unify_mpq(). *)
  let is_rational = Foreign.foreign "PL_is_rational" (t @-> returning bool)         

  (** Returns non-zero if term is a float. Note that the corresponding PL_get_float() converts rationals (and thus integers). *)
  let is_float = Foreign.foreign "PL_is_float" (t @-> returning bool)         

  (** Returns non-zero if term is a callable term. See callable/1 for details. *)
  let is_callable = Foreign.foreign "PL_is_callable" (t @-> returning bool)         

  (** Returns non-zero if term is a compound term. *)
  let is_compound = Foreign.foreign "PL_is_compound" (t @-> returning bool)

  (** Returns non-zero if term is compound and its functor is functor. This test is equivalent to PL_get_functor(), followed by testing the functor, but easier to write and faster. *)
  let is_functor = Foreign.foreign "PL_is_functor" (t @-> returning bool)         

  (** Returns non-zero if term is a compound term using the list constructor or the list terminator. See also PL_is_pair() and PL_skip_list(). *)
  let is_list = Foreign.foreign "PL_is_list" (t @-> returning bool)         

  (** Returns non-zero if term is a compound term using the list constructor. See also PL_is_list() and PL_skip_list(). *)
  let is_pair = Foreign.foreign "PL_is_pair" (t @-> returning bool)         

  (** Returns non-zero if term is atomic (not a variable or compound). *)
  let is_atomic = Foreign.foreign "PL_is_atomic" (t @-> returning bool)         

  (** Returns non-zero if term is an rational (including integers) or float. *)
  let is_number = Foreign.foreign "PL_is_number" (t @-> returning bool)         

  (** Returns non-zero if term is acyclic (i.e. a finite tree).   *)
  let is_acyclic = Foreign.foreign "PL_is_acyclic" (t @-> returning bool)

  let put_variable = Foreign.foreign "PL_put_variable" (t @-> returning void)

  let put_atom = Foreign.foreign "PL_put_atom" (t @-> Atom.t @-> returning void)

  let put_bool = Foreign.foreign "PL_put_bool" (t @-> bool @-> returning void)

  let put_atom_chars = Foreign.foreign "PL_put_atom_chars" (t @-> string @-> returning bool)

  let put_string_chars = Foreign.foreign "PL_put_string_chars" (t @-> string @-> returning bool)

  let put_integer = Foreign.foreign "PL_put_integer" (t @-> int @-> returning bool)

  let put_int64 = Foreign.foreign "PL_put_int64" (t @-> int64_t @-> returning bool)

  let put_uint64 = Foreign.foreign "PL_put_uint64" (t @-> uint64_t @-> returning bool)

  let put_float = Foreign.foreign "PL_put_float" (t @-> float @-> returning bool)

  let put_list = Foreign.foreign "PL_put_list" (t @-> returning bool)

  let put_nil = Foreign.foreign "PL_put_nil" (t @-> returning bool)

  let put_term = Foreign.foreign "PL_put_term" (t @-> t @-> returning bool)

  let cons_functor =
    Foreign.foreign "PL_cons_functor_v" (t @-> Functor.t @-> Array.t @-> returning bool)

  let cons_functor result fn (args, _) = cons_functor result fn args

  let cons_functor1 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> returning bool)
  let cons_functor2 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> returning bool)
  let cons_functor3 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> returning bool)
  let cons_functor4 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> t @-> returning bool)
  let cons_functor5 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> t @-> t @-> returning bool)
  let cons_functor6 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> t @-> t @-> t @-> returning bool)
  let cons_functor7 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> returning bool)
  let cons_functor8 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> returning bool)
  let cons_functor9 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> returning bool)
  let cons_functor10 = Foreign.foreign "PL_cons_functor" (t @-> Functor.t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> t @-> returning bool)

  let cons_list = Foreign.foreign "PL_cons_list" (t @-> t @-> t @-> returning bool)

  let put_dict =
    Foreign.foreign "PL_put_dict" (t @-> Atom.t @-> int @-> ptr Atom.t @-> t @-> returning bool)

  let put_dict ?(tag=null) t keys (values, len) =
    let keys = CArray.of_list Atom.t keys in
    assert (CArray.length keys <= len);
    put_dict t tag (CArray.length keys) (CArray.start keys) values

  let unify = Foreign.foreign "PL_unify" (t @-> t @-> returning bool)

  let unify_atom = Foreign.foreign "PL_unify_atom" (t @-> Atom.t @-> returning bool)

  let unify_bool = Foreign.foreign "PL_unify_bool" (t @-> bool @-> returning bool)

  let unify_atom_chars = Foreign.foreign "PL_unify_atom_chars" (t @-> string @-> returning bool)

  let unify_string_chars = Foreign.foreign "PL_unify_string_chars" (t @-> string @-> returning bool)

  let unify_integer = Foreign.foreign "PL_unify_integer" (t @-> int @-> returning bool)

  let unify_int64 = Foreign.foreign "PL_unify_int64" (t @-> int64_t @-> returning bool)

  let unify_uint64 = Foreign.foreign "PL_unify_uint64" (t @-> uint64_t @-> returning bool)

  let unify_float = Foreign.foreign "PL_unify_float" (t @-> float @-> returning bool)

  let unify_functor = Foreign.foreign "PL_unify_functor" (t @-> Functor.t @-> returning bool)

  let unify_compound = Foreign.foreign "PL_unify_compound" (t @-> Functor.t @-> returning bool)

  let unify_list = Foreign.foreign "PL_unify_list" (t @-> t @-> t @-> returning bool)

  let unify_nil = Foreign.foreign "PL_unify_nil" (t @-> returning bool)

  let unify_arg = Foreign.foreign "PL_unify_arg" (int @-> t @-> t @-> returning bool)

  let chars_to_term = Foreign.foreign "PL_chars_to_term" (string @-> t @-> returning bool)

end

module Module = struct
  type t = module_
  let compare l r = ptr_compare l r
  let equal l r = compare l r = 0

  let t : t typ = ptr void

  let context = Foreign.foreign "PL_context" (void @-> returning t)

  let strip_module = Foreign.foreign "PL_strip_module" (Term.t @-> ptr t @-> Term.t @-> returning bool)

  let strip_module ?(module_: t option) raw plain =
    let module_ = match module_ with None -> null | Some ptr -> ptr in
    let module_ = allocate t module_ in
    let result = strip_module raw module_ plain in
    let data = if result then Some (!@ module_) else None in
    result, data

  let module_name = Foreign.foreign "PL_module_name" (t @-> returning Atom.t)

  let new_module = Foreign.foreign "PL_new_module" (Atom.t @-> returning t)

end

module Predicate = struct
  type t = predicate
  let t : t typ = ptr void

  let pred = Foreign.foreign "PL_pred" (Functor.t @-> Module.t @-> returning t)

  let pred ?(module_=null) fn = pred fn module_

  let predicate = Foreign.foreign "PL_predicate" (string @-> int @-> string_opt @-> returning t)

  let predicate ?module_ name arity =
    predicate name arity module_

  let predicate_info = Foreign.foreign "PL_predicate_info" (t @-> ptr Atom.t @-> ptr int @-> ptr Module.t @-> returning void)

  let predicate_info pred =
    let name = allocate Atom.t null in
    let arity = allocate int 0 in
    let modl = allocate Module.t null in
    predicate_info pred name arity modl;
    !@ name, !@ arity, !@ modl

end

module Query = struct

  module Result = struct
    type t =
      | Bool of bool
      | Last | Exception

    let to_bool = function Bool b -> b | Last -> true | Exception -> false

    let t =
      view
        ~read:(function[@warning "-8"]
          | v when v = Bindings.Result.s_exception -> Exception
          | v when v = Bindings.Result.s_last -> Last
          | v when v = Bindings.Result.s_true -> Bool true
          | v when v = Bindings.Result.s_false -> Bool false
          | 0 -> Bool false
          | 1 -> Bool true
        )
        ~write:(function
          | Bool true -> Bindings.Result.s_true
          | Bool false -> Bindings.Result.s_true
          | Last -> Bindings.Result.s_last
          | Exception -> Bindings.Result.s_exception
        ) int

  end


  type qid = unit ptr
  let qid = ptr void

  let open_query =
    Foreign.foreign "PL_open_query" (Module.t @-> Q.t @-> Predicate.t @-> Term.Array.t @-> returning qid)

  let open_query ?(module_=null) ?(flags=Q.normal) pred (arg, _) =
    open_query module_ flags pred arg

  let next_solution =
    Foreign.foreign "PL_next_solution" (qid @-> returning Result.t)

  let cut_query =
    Foreign.foreign "PL_cut_query" (qid @-> returning bool)

  let close_query =
    Foreign.foreign "PL_close_query" (qid @-> returning bool)

  let current_query =
    Foreign.foreign "PL_current_query" (void @-> returning qid)

  let call_predicate =
    Foreign.foreign "PL_call_predicate" (Module.t @-> Q.t @-> Predicate.t @-> Term.t @-> returning bool)

  let call_predicate ?(module_=null) ?(flags=Q.normal) pred ((arg, _): Term.Array.t) =
    call_predicate module_ flags pred arg

  let call =
    Foreign.foreign "PL_call" (Term.t @-> Module.t @-> returning bool)

  let call ?(module_=null) term = call term module_

  let yielded =
    Foreign.foreign "PL_yielded" (qid @-> returning Term.t)

  let yielded qid =
    let result = yielded qid in
    if Unsigned.ULong.(equal zero result)
    then None
    else Some result

end

module ForeignFrame = struct
  type t = unit ptr
  let t : t typ = ptr void

  let open_frame =
    Foreign.foreign "PL_open_foreign_frame" (void @-> returning t)

  let close_frame =
    Foreign.foreign "PL_close_foreign_frame" (t @-> returning void)

  let discard_frame =
    Foreign.foreign "PL_discard_foreign_frame" (t @-> returning void)

  let rewind_frame =
    Foreign.foreign "PL_rewind_foreign_frame" (t @-> returning void)

end

module Exception = struct

  let raise = Foreign.foreign "PL_raise_exception" (Term.t @-> returning bool)

  let exn = Foreign.foreign "PL_exception" (Query.qid @-> returning Term.t)

  let exn qid =
    let result = exn qid in
    if Unsigned.ULong.(equal zero result)
    then None
    else Some result

  let clear_exn = Foreign.foreign "PL_clear_exception" (void @-> returning void)

end

module Database = struct

  let assert_ = Foreign.foreign "PL_assert" (Term.t @-> Module.t @-> DB.t @-> returning bool)

  let assert_ ?(flags=DB.assertz) ?(module_=null) t =
    assert_ t module_ flags

end

module Filename = struct

  let get_file_name =
    Foreign.foreign "PL_get_file_name" (Term.t @-> ptr string @-> File.t @-> returning bool)

  let get_file_name ?(flags=0) t =
    let res = allocate string "" in
    if get_file_name t res flags
    then Some (!@ res)
    else None

end

module Env = struct
  module Flags = struct
    let get_flag = Foreign.foreign "PL_current_prolog_flag" (Atom.t @-> int @-> ptr void @-> returning bool)

    let set_flagb = Foreign.foreign "PL_set_prolog_flag" (string @-> int @-> bool @-> returning bool)
    let set_flagb flag vl = set_flagb flag Bindings.Term.pl_bool vl

    let get_flagb flag =
      let vl = allocate bool false in
      if get_flag flag Bindings.Term.pl_bool (vl |> to_voidp)
      then Some (!@ vl)
      else None

    let set_flaga = Foreign.foreign "PL_set_prolog_flag" (string @-> int @-> Atom.t @-> returning bool)
    let set_flaga flag vl = set_flaga flag Bindings.Term.pl_atom vl

    let get_flaga flag =
      let vl = allocate Atom.t null in
      if get_flag flag Bindings.Term.pl_atom (vl |> to_voidp)
      then Some (!@ vl)
      else None

    let set_flagi = Foreign.foreign "PL_set_prolog_flag" (string @-> int @-> int @-> returning bool)
    let set_flagi flag vl = set_flagi flag Bindings.Term.pl_integer vl

    let get_flagi flag =
      let vl = allocate int 0 in
      if get_flag flag Bindings.Term.pl_integer (vl |> to_voidp)
      then Some (!@ vl)
      else None

    let get_flagf flag =
      let vl = allocate float 0. in
      if get_flag flag Bindings.Term.pl_float (vl |> to_voidp)
      then Some (!@ vl)
      else None

    let get_flagt flag =
      let vl = allocate ulong Unsigned.ULong.zero in
      if get_flag flag Bindings.Term.pl_term (vl |> to_voidp)
      then Some (!@ vl)
      else None

  end

  module Action = struct
    let action0 = Foreign.foreign "PL_action" (Action.t @-> returning void)
    let actioni = Foreign.foreign "PL_action" (Action.t @-> int @-> returning void)
    let actionb = Foreign.foreign "PL_action" (Action.t @-> bool @-> returning void)
    let actions = Foreign.foreign "PL_action" (Action.t @-> string @-> returning void)
    let trace () = action0 Action.action_trace
    let debug () = action0 Action.action_debug
    let backtrace n = actioni Action.action_debug n
    let halt ext = actioni Action.action_halt ext
    let abort () = action0 Action.action_abort
    let break () = action0 Action.action_break
    let guiapp b = actionb Action.action_guiapp b
    let traditional () = action0 Action.action_traditional
    let write s = actions Action.action_write s
    let flush () = action0 Action.action_flush
    let attach_console () = action0 Action.action_attach_console
  end

  (* let info = Foreign.foreign "PL_version_info" (VersionInfo.t @-> returning uint) *)

end

let license =
  Foreign.foreign "PL_license" (string @-> Module.t @-> returning void)

let initialise =
  Foreign.foreign "PL_initialise" (int @-> ptr string @-> returning int)

let initialise ?args () =
  let args = match args with
    | None -> [Sys.argv.(0); "-q"]
    | Some args -> Sys.argv.(0) :: args in
  let length = List.length args in
  let args = CArray.of_list string args in
  assert (initialise length (CArray.start args) <> 0)

let cleanup =
  Foreign.foreign "PL_cleanup" (int @-> returning void)

let cleanup () =
  cleanup 0
