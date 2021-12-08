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

type atom
type functor_
type term

type module_
type predicate

module CVT : sig
  type t = private int
  external ( + ) : t -> t -> t = "%orint"
  external equal : t -> t -> bool = "%equal"
  val atom : t
  val string : t
  val list : t
  val integer : t
  val float : t
  val number : t
  val atomic : t
  val variable : t
  val write : t
  val write_canonical : t
  val writeq : t
  val all : t
  val exception_ : t
  val discardable : t
  val stack : t
  val malloc : t
  val utf8 : t
  val mb : t
end

module Q : sig
  type t = private int
  external ( + ) : t -> t -> t = "%orint"
  external equal : t -> t -> bool = "%equal"
  val normal : t
  val nodebug : t
  val catch_exception : t
  val pass_exception : t
  val allow_yield : t
  val ext_status : t
end

module DB : sig
  type t = private int
  external equal : t -> t -> bool = "%equal"
  val assertz : t
  val asserta : t
  val create_thread_local : t
  val create_incremental : t
end

module File : sig
  type t = private int
  external ( + ) : t -> t -> t = "%orint"
  external equal : t -> t -> bool = "%equal"
  val file_absolute : t
  val file_ospath : t
  val file_search : t
  val file_exist : t
  val file_read : t
  val file_write : t
  val file_execute : t
  val file_noerrors : t
end


module Atom : sig
  type t = atom
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val atom : string -> t
  val chars : t -> string
  val register : t -> unit
  val unregister : t -> unit
end

module Functor : sig
  type t = functor_
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val functor_ : atom -> int -> t
  val name : t -> atom
  val arity : t -> int
end

module Term : sig
  type t = term
  type repr =
      Variable of t
    | Atom of atom
    | Bool of bool
    | Nil
    | Blob of t
    | String of string
    | Integer of int
    | Rational of t
    | Float of float
    | Compound of atom * repr list
    | List of repr * repr
    | Dict of t * (atom -> t option)
  module Array : sig
    type t
    val empty: t
    val singleton : term -> t
    val to_array : t -> term array
    val get_unsafe : t -> int -> term
    val get : t -> int -> term
  end

  val equal: t -> t -> bool
  val compare : t -> t -> int
  val ( == ) : t -> t -> bool
  val new_ref : unit -> t
  val new_refs : int -> Array.t
  val copy : t -> t
  val reset : t -> unit
  val get_chars : ?flags:CVT.t -> t -> string option
  val get_atom : t -> atom option
  val get_atom_chars : t -> string option
  val get_string_chars : t -> string option
  val get_integer : t -> int option
  val get_long : t -> Signed.long option
  val get_int64 : t -> int64 option
  val get_bool : t -> bool option
  val get_float : t -> float option
  val get_functor : t -> functor_ option
  val get_name_arity : t -> (atom * int) option
  val get_compound_name_arity : t -> (atom * int) option
  val get_arg : int -> t -> t -> bool
  val get_dict_key : atom -> t -> t -> bool
  val get_list : t -> t -> t -> bool
  val get_head : t -> t -> bool
  val get_tail : t -> t -> bool
  val get_nil : t -> bool
  val term_type : t -> [> `Atom | `Blob | `Bool | `Dict | `Float
                       | `Integer | `ListPair | `Nil | `Rational
                       | `String | `Term | `Variable ]

  val get : t -> repr
  val is_variable : t -> bool
  val is_ground : t -> bool
  val is_atom : t -> bool
  val is_string : t -> bool
  val is_integer : t -> bool
  val is_rational : t -> bool
  val is_float : t -> bool
  val is_callable : t -> bool
  val is_compound : t -> bool
  val is_functor : t -> bool
  val is_list : t -> bool
  val is_pair : t -> bool
  val is_atomic : t -> bool
  val is_number : t -> bool
  val is_acyclic : t -> bool
  val put_variable : t -> unit
  val put_atom : t -> atom -> unit
  val put_bool : t -> bool -> unit
  val put_atom_chars : t -> string -> bool
  val put_string_chars : t -> string -> bool
  val put_integer : t -> int -> bool
  val put_int64 : t -> int64 -> bool
  val put_uint64 : t -> Unsigned.uint64 -> bool
  val put_float : t -> float -> bool
  val put_list : t -> bool
  val put_nil : t -> bool
  val put_term : t -> t -> bool
  val cons_functor : t -> functor_ -> Array.t -> bool
  val cons_functor1 : t -> functor_ -> t -> bool
  val cons_functor2 : t -> functor_ -> t -> t -> bool
  val cons_functor3 : t -> functor_ -> t -> t -> t -> bool
  val cons_functor4 : t -> functor_ -> t -> t -> t -> t -> bool
  val cons_functor5 : t -> functor_ -> t -> t -> t -> t -> t -> bool
  val cons_functor6 : t -> functor_ -> t -> t -> t -> t -> t -> t -> bool
  val cons_functor7 : t -> functor_ -> t -> t -> t -> t -> t -> t -> t -> bool
  val cons_functor8 : t -> functor_ -> t -> t -> t -> t -> t -> t -> t -> t -> bool
  val cons_functor9 : t -> functor_ -> t -> t -> t -> t -> t -> t -> t -> t -> t -> bool
  val cons_functor10 : t -> functor_ -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> bool
  val cons_list : t -> t -> t -> bool
  val put_dict : ?tag:Atom.t -> t -> atom list -> t * int -> bool
  val unify : t -> t -> bool
  val unify_atom : t -> atom -> bool
  val unify_bool : t -> bool -> bool
  val unify_atom_chars : t -> string -> bool
  val unify_string_chars : t -> string -> bool
  val unify_integer : t -> int -> bool
  val unify_int64 : t -> int64 -> bool
  val unify_uint64 : t -> Unsigned.uint64 -> bool
  val unify_float : t -> float -> bool
  val unify_functor : t -> functor_ -> bool
  val unify_compound : t -> functor_ -> bool
  val unify_list : t -> t -> t -> bool
  val unify_nil : t -> bool
  val unify_arg : int -> t -> t -> bool
  val chars_to_term : string -> t -> bool
end
module Module : sig
  type t = module_
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val context : unit -> t
  val strip_module : ?module_:t -> term -> term -> bool * t option
  val module_name : t -> atom
  val new_module : atom -> t
end

module Predicate : sig
  type t = predicate
  val pred : ?module_:Module.t -> functor_ -> t
  val predicate : ?module_:string -> string -> int -> t
  val predicate_info : t -> atom * int * module_
end

module Query : sig
  module Result : sig
    type t = Bool of bool | Last | Exception
    val to_bool : t -> bool
  end
  type qid
  val open_query : ?module_:Module.t -> ?flags:Q.t -> predicate -> Term.Array.t -> qid
  val next_solution : qid -> Result.t
  val cut_query : qid -> bool
  val close_query : qid -> bool
  val current_query : unit -> qid
  val call_predicate : ?module_:Module.t -> ?flags:Q.t -> predicate -> Term.Array.t -> bool
  val call : ?module_:Module.t -> term -> bool
  val yielded: qid -> term option
end

module ForeignFrame : sig
  type t
  val open_frame : unit -> t
  val close_frame : t -> unit
  val discard_frame : t -> unit
  val rewind_frame : t -> unit
end

module Exception : sig
  val raise : term -> bool
  val exn : Query.qid -> term option
  val clear_exn : unit -> unit
end

module Database : sig
  val assert_ : ?flags:DB.t -> ?module_:Module.t -> term -> bool
end

module Filename : sig
  val get_file_name : ?flags:File.t -> term -> string option
end

module Env : sig
  module Flags : sig
    val set_flagb : string -> bool -> bool
    val get_flagb : atom -> bool option
    val set_flaga : string -> atom -> bool
    val get_flaga : atom -> atom option
    val set_flagi : string -> int -> bool
    val get_flagi : atom -> int option
    val get_flagf : atom -> float option
    val get_flagt : atom -> term option
  end
  module Action : sig
    val trace : unit -> unit
    val debug : unit -> unit
    val backtrace : int -> unit
    val halt : int -> unit
    val abort : unit -> unit
    val break : unit -> unit
    val guiapp : bool -> unit
    val traditional : unit -> unit
    val write : string -> unit
    val flush : unit -> unit
    val attach_console : unit -> unit
  end
end

val license: string -> Module.t -> unit
val initialise : ?args:string list -> unit -> unit
val cleanup : unit -> unit

