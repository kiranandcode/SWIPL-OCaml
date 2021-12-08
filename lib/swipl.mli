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


val initialise : unit -> unit
(** [initialise ()] initialises the SWI-prolog engine. It must be
    called at the start of the program before creating any terms. *)

type t
(** [t [@@deriving eq, ord, show]] represents prolog terms. *)
(**/**)
val equal: t -> t -> bool
val compare: t -> t -> int
val show: t -> string
val pp: Format.formatter -> t -> unit
(**/**)


type ty =
  [ `Atom | `Blob | `Bool | `Dict | `Float | `Integer
  | `ListPair | `Nil | `Rational | `String | `Term | `Variable ]
(** Represents types of prolog terms  *)

type query
(** Represents an in-progress prolog query.

    {b Note}: It is an error to run another query before the result
    of a prior query has been consumed.  *)

type ctx
(** Represents a term context - all terms are tied to a given context,
    and are discarded when the context ends. It is undefined behaviour
    to manipulate a term that has been freed, so make sure to extract
    any relevant terms to OCaml before the context is discarded. *)

type fn
(** [fn [@@deriving eq, ord]] represents function symbols. *)
(**/**)
val equal_fn: fn -> fn -> bool
val compare_fn: fn -> fn -> int
(**/**)

type atom
(** Represents atoms in prolog.  This is an internal type that is
    primarily exposed for performance reasons - in particular,
    comparing whether two atoms [: atom] are equal will be faster than comparing
    if two terms [: t] are equal. *)
(**/**)
val equal_atom: atom -> atom -> bool
val compare_atom: atom -> atom -> int
val show_atom: atom -> string
val pp_atom: Format.formatter -> atom -> unit
(**/**)

val atom: string -> atom
(** [atom name] constructs an atom with characters [name].

    {b Note}: If you call this function before {!initialise} you will segfault. *)

type module_
(** [module_ [@@deriving eq, ord]] represents SWI-prolog modules.  *)
(**/**)
val equal_module_: module_ -> module_ -> bool
val compare_module_: module_ -> module_ -> int
(**/**)


val module_: string -> module_
(** [module_ name] returns a reference to the module with name [name]. *)

val fold_solutions:
  ([> `Exception of t | `Last | `Solution ] -> [< `Close | `Cut ] option) -> query -> bool
(** [fold_solutions fn qid] is the most general combinator for
    consuming the output of Prolog queries. It calls [fn] once for each
    solution to the query (or exception raised by the query), and
    allows the function to terminate early by either [`Close]ing the
    query (dropping all bindings), or [`Cut]ting the query (and keeping
    the bindings introduced by the current result). The function
    returns a boolean representing whether there were any results or
    not.

    For many cases you probably don't need the generality of this
    combinator, and can get away with using one of the simpler wrappers
    we provide (see below).  *)

val iter_solutions: ?on_error:(t -> unit) -> query -> (unit -> unit) -> bool
(** [iter_solutions ?on_error qid fn] calls [fn] once for each
    solution returned by the query [qid], and returns true if there
    were any solutions at all. [on_error] is called with any exception
    terms that are raised by the query (if any). *)

val first_solution: query -> bool
(** [first_solution qid] consumes the query [qid] and preserves the
    bindings from the first solution to the query. The function returns
    a boolean representing whether there were any solutions or not.

    {b raises} Failure if the prolog query raises an exception.
*)

val last_solution: query -> bool
(** [last_solution qid] consumes the query [qid] and preserves the
    bindings from the last solution to the query. The function returns
    a boolean representing whether there were any solutions or not.

    {b raises} Failure if the prolog query raises an exception.
*)

val call: ctx -> t -> unit
(** [call ctx term] runs the query represented by [term], preserving
    the bindings produced by the first solution if the query succeeds
    at all (does not check if the query succeeds at all).

    {b raises} Failure if the prolog query raises an exception. *)

(** The {!Syntax} module provides a useful set of combinators for
   constructing prolog terms using idiomatic OCaml syntax.

    As the operators in this module shadow common OCaml ones ([&&, ||]),
    typically you want to locally open this module:

    {[ Swipl.Syntax.(app ("consult" /@ 1) [! file]) ]} *)
module Syntax : sig
  val ( /@ ) : string -> int -> fn
  (** [fn /@ arity] mirrors the prolog syntax {!pred/arity} and declares
      a prolog function named [fn] with arity [arity].

      {b Note}: If you call this function before {!initialise} you will segfault. *)

  val ( ! ) : string -> t
  (** [! atom] creates a prolog term representing the atom [atom].

      {b Note}: If you call this function before {!initialise} you will segfault. *)

  val app : ?module_:module_ -> fn -> t list -> t
  (** [app ?module_ fn args] creates a prolog term that represents the
      symbolic expression [module_:fn(args...)]. *)

  val ( && ) : t -> t -> t
  (** [t1 && t2] creates a prolog term that represents the conjunction
      of two prolog queries - i.e the prolog expression [t1,t2]. *)

  val ( || ) : t -> t -> t
  (** [t1 || t2] creates a prolog term that represents the disjunction
      of two prolog queries - i.e the prolog expression [t1; t2]. *)

end

val with_ctx : (ctx -> 'a) -> 'a
(** [with_ctx fn] creates a new term context [ctx] and calls [fn] with
    that context.

    {b Note}: Any terms created within the context will be dropped at
    the end of this function - it is undefined behaviour to try and
    escape prolog terms out of [fn]. (You have been warned, nasal
    demons at the ready).

    {b Note}: If you call this function before {!initialise} you will
    segfault. *)

val eval : ctx -> t -> query
(** [eval ctx term] send the prolog term [term] to the prolog engine
    and returns a handle to the query. *)

val encode_list: ctx -> t list -> t
(** [encode_list ctx ls] returns a prolog term representing the list
    [ls]. *)

val encode_string: ctx -> string -> t
(** [encode_string ctx str] returns a prolog term representing the string
    [str]. *)

val fresh : ctx -> t
(** [fresh ctx] creates a new prolog variable that lasts for the scope
    of [ctx]. *)

val extract_list: ctx -> t -> t list
(** [extract_list ctx t] extracts a list of prolog terms from [t].

    {b Note}: It is undefined behaviour to call this function on a term
    that is not a list. If in doubt, check the type of the term with
    {!typeof} first.  *)

val extract_atom: ctx -> t -> atom
(** [extract_atom ctx t] extracts an atom from [t].

    {b Note}: It is undefined behaviour to call this function on a term
    that is not an atom. If in doubt, check the type of the term with
    {!typeof} first.  *)

val extract_bool: ctx -> t -> bool
(** [extract_bool ctx t] extracts a bool from [t].

    {b Note}: It is undefined behaviour to call this function on a term
    that is not a bool. If in doubt, check the type of the term with
    {!typeof} first.  *)

val extract_int: ctx -> t -> int
(** [extract_int ctx t] extracts an int from [t].

    {b Note}: It is undefined behaviour to call this function on a term
    that is not an int. If in doubt, check the type of the term with
    {!typeof} first.  *)

val extract_float: ctx -> t -> float
(** [extract_float ctx t] extracts a float from [t].

    {b Note}: It is undefined behaviour to call this function on a term
    that is not a float. If in doubt, check the type of the term with
    {!typeof} first.  *)

val extract_string: ctx -> t -> string
(** [extract_string ctx t] extracts a string from [t].

    {b Note}: It is undefined behaviour to call this function on a term
    that is not a string. If in doubt, check the type of the term with
    {!typeof} first.  *)


val extract_functor: ctx -> t -> (atom * t list)
(** [extract_int ctx t] extracts a compound term or atom from [t].

    {b Note}: It is undefined behaviour to call this function on a term
    that is not a functor or atom. If in doubt, check the type of the
    term with {!typeof} first.  *)

val typeof: t -> ty
(** [typeof t] returns the type of term t.

    {b Note}: If you call this function before {!initialise} you will
    segfault. *)

val load_source: string -> unit
(** [load_source src] loads [src] as prolog source code (i.e not a
   file) into the prolog engine.

    {b Note}: If you call this function before {!initialise} you will
   segfault. *)
