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
[@@@warning "-50"]

module Stubs = functor (T: Cstubs_structs.TYPE) -> struct
  module Term = struct
    let pl_variable = T.constant "PL_VARIABLE" T.int
    let pl_bool     = T.constant "PL_BOOL" T.int
    let pl_atom     = T.constant "PL_ATOM" T.int
    let pl_nil      = T.constant "PL_NIL" T.int
    let pl_blob     = T.constant "PL_BLOB" T.int
    let pl_string   = T.constant "PL_STRING" T.int
    let pl_integer  = T.constant "PL_INTEGER" T.int
    let pl_rational = T.constant "PL_RATIONAL" T.int
    let pl_float    = T.constant "PL_FLOAT" T.int
    let pl_term     = T.constant "PL_TERM" T.int
    let pl_list_pair = T.constant "PL_LIST_PAIR" T.int
    let pl_dict     = T.constant "PL_DICT" T.int
  end

  module CVT = struct

    (* Convert if term is an atom.  *)
    let atom = T.constant "CVT_ATOM" T.int

    (* Convert if term is a string. *)
    let string = T.constant "CVT_STRING" T.int

    (* Convert if term is a list of of character codes. *)
    let list = T.constant "CVT_LIST" T.int

    (* Convert if term is an integer. *)
    let integer = T.constant "CVT_INTEGER" T.int

    (* Convert if term is a float. The characters returned are the same as write/1 would write for the floating point number. *)
    let float = T.constant "CVT_FLOAT" T.int

    (* Convert if term is an integer or float. *)
    let number = T.constant "CVT_NUMBER" T.int

    (* Convert if term is atomic. *)
    let atomic = T.constant "CVT_ATOMIC" T.int

    (* Convert variable to print-name *)
    let variable = T.constant "CVT_VARIABLE" T.int

    (* Convert any term that is not converted by any of the other flags using write/1. If no BUF_* is provided, BUF_STACK is implied. *)
    let write = T.constant "CVT_WRITE" T.int

    (* As CVT_WRITE, but using write_canonical/2. *)
    let write_canonical = T.constant "CVT_WRITE_CANONICAL" T.int

    (* As CVT_WRITE, but using writeq/2. *)
    let writeq = T.constant "CVT_WRITEQ" T.int

    (* Convert if term is any of the above, except for CVT_VARIABLE and CVT_WRITE*. *)
    let all = T.constant "CVT_ALL" T.int

    (* If conversion fails due to a type error, raise a Prolog type error exception in addition to failure *)
    let exception_ = T.constant "CVT_EXCEPTION" T.int

    (* Data must copied immediately *)
    let discardable = T.constant "BUF_DISCARDABLE" T.int

    (* Data is stored on a stack. The older BUF_RING is an alias for BUF_STACK. See section 12.4.12. *)
    let stack = T.constant "BUF_STACK" T.int

    (* Data is copied to a new buffer returned by PL_malloc(3). When no longer needed the user must call PL_free() on the data. *)
    let malloc = T.constant "BUF_MALLOC" T.int

    (* Convert the text to a UTF-8 string. This works for all text. *)
    let utf8 = T.constant "REP_UTF8" T.int

    (* Convert to default locale-defined 8-bit string. Success depends on the locale. Conversion is done using the wcrtomb() C library function.  *)
    let mb = T.constant "REP_MB" T.int

  end

  module Q = struct
    (* Normal operation. The debugger inherits its settings from the environment. If an exception occurs that is not handled in Prolog, a message is printed and the tracer is started to debug the error.205 *)
    let normal = T.constant "PL_Q_NORMAL" T.int

    (* Switch off the debugger while executing the goal. This option is used by many calls to hook-predicates to avoid tracing the hooks. An example is print/1 calling portray/1 from foreign code. *)
    let nodebug = T.constant "PL_Q_NODEBUG" T.int

    (* If an exception is raised while executing the goal, do not report it, but make it available for PL_exception(). *)
    let catch_exception = T.constant "PL_Q_CATCH_EXCEPTION" T.int

    (* As PL_Q_CATCH_EXCEPTION, but do not invalidate the exception-term while calling PL_close_query(). This option is experimental. *)
    let pass_exception = T.constant "PL_Q_PASS_EXCEPTION" T.int

    (* Support the I_YIELD instruction for engine-based coroutining. See $engine_yield/2 in boot/init.pl for details. *)
    let allow_yield = T.constant "PL_Q_ALLOW_YIELD" T.int

    (* Make PL_next_solution() return extended status. Instead of only TRUE or FALSE extended status as illustrated in the following table *)
    let ext_status = T.constant "PL_Q_EXT_STATUS" T.int

  end

  module Result = struct
    (* Exception available through PL_exception() *)
    let s_exception = T.constant "PL_S_EXCEPTION" T.int
    (* Query failed *)
    let s_false = T.constant "PL_S_FALSE" T.int
    (* Query succeeded with choicepoint *)
    let s_true = T.constant "PL_S_TRUE" T.int
    (* Query succeeded without choicepoint  *)
    let s_last = T.constant "PL_S_LAST" T.int
  end

  module Database = struct

    (* Add the new clause as last. Calls assertz/1. This macros is defined
       as 0 and thus the default. *)
    let assertz = T.constant "PL_ASSERTZ" T.int

    (* Add the new clause as first. Calls asserta/1. *)
    let asserta = T.constant "PL_ASSERTA" T.int

    (* If the predicate is not defined, create it as thread-local. See thread_local/1. *)
    let create_thread_local = T.constant "PL_CREATE_THREAD_LOCAL" T.int

    (* If the predicate is not defined, create it as incremental see table/1 and section 7.7. *)
    let create_incremental = T.constant "PL_CREATE_INCREMENTAL" T.int

  end

  module File = struct

    (* Return an absolute path to the requested file. *)
    let file_absolute = T.constant "PL_FILE_ABSOLUTE" T.int

    (* Return the name using the hosting OS conventions. On MS-Windows, \ is used to separate directories rather than the canonical /. *)
    let file_ospath = T.constant "PL_FILE_OSPATH" T.int

    (* Invoke absolute_file_name/3. This implies rules from file_search_path/2 are used. *)
    let file_search = T.constant "PL_FILE_SEARCH" T.int

    (* Demand the path to refer to an existing entity. *)
    let file_exist = T.constant "PL_FILE_EXIST" T.int

    (* Demand read-access on the result. *)
    let file_read = T.constant "PL_FILE_READ" T.int

    (* Demand write-access on the result. *)
    let file_write = T.constant "PL_FILE_WRITE" T.int

    (* Demand execute-access on the result. *)
    let file_execute = T.constant "PL_FILE_EXECUTE" T.int

    (* Do not raise any exceptions.  *)
    let file_noerrors = T.constant "PL_FILE_NOERRORS" T.int

  end

  module Action = struct

    (* Start Prolog tracer (trace/0). Requires no arguments. *)
    let action_trace = T.constant "PL_ACTION_TRACE" T.int

    (* Switch on Prolog debug mode (debug/0). Requires no arguments. *)
    let action_debug = T.constant "PL_ACTION_DEBUG" T.int

    (* Print backtrace on current output stream. The argument (an int) is the number of frames printed. *)
    let action_backtrace = T.constant "PL_ACTION_BACKTRACE" T.int

    (* Halt Prolog execution. This action should be called rather than Unix exit() to give Prolog the opportunity to clean up. This call does not return. The argument (an int) is the exit code. See halt/1. *)
    let action_halt = T.constant "PL_ACTION_HALT" T.int

    (* Generate a Prolog abort (abort/0). This call does not return. Requires no arguments. *)
    let action_abort = T.constant "PL_ACTION_ABORT" T.int

    (* Create a standard Prolog break environment (break/0). Returns after the user types the end-of-file character. Requires no arguments. *)
    let action_break = T.constant "PL_ACTION_BREAK" T.int

    (* Windows: Used to indicate to the kernel that the application is a GUI application if the argument is not 0, and a console application if the argument is 0. If a fatal error occurs, the system uses a windows messagebox to report this on a GUI application, and otherwise simply prints the error and exits. *)
    let action_guiapp = T.constant "PL_ACTION_GUIAPP" T.int

    (* Same effect as using --traditional. Must be called before PL_initialise(). *)
    let action_traditional = T.constant "PL_ACTION_TRADITIONAL" T.int

    (* Write the argument, a char * to the current output stream. *)
    let action_write = T.constant "PL_ACTION_WRITE" T.int

    (* Flush the current output stream. Requires no arguments. *)
    let action_flush = T.constant "PL_ACTION_FLUSH" T.int

    (* Attach a console to a thread if it does not have one. See attach_console/0. *)
    let action_attach_console = T.constant "PL_ACTION_ATTACH_CONSOLE" T.int

    (* Takes an integer argument. If TRUE, the GMP allocations are immediately bound to the Prolog functions. If FALSE, SWI-Prolog will never rebind the GMP allocation functions. See mp_set_memory_functions() in the GMP documentation. The action returns FALSE if there is no GMP support or GMP is already initialised.  *)
    let gmp_set_alloc_functions = T.constant "PL_GMP_SET_ALLOC_FUNCTIONS" T.int

  end

  module VersionInfo = struct

    (* SWI-Prolog version as 10,000 × major + 100 × minor + patch. *)
    let version_system = T.constant "PL_VERSION_SYSTEM" T.int

    (* Incremented if the foreign interface defined in this chapter changes in a way that breaks backward compatibility. *)
    let version_fli = T.constant "PL_VERSION_FLI" T.int

    (* Incremented if the binary representation of terms as used by PL_record_external() and fast_write/2 changes. *)
    let version_rec = T.constant "PL_VERSION_REC" T.int

    (* Incremented if the QLF file format changes. *)
    let version_qlf = T.constant "PL_VERSION_QLF" T.int

    (* Represents the oldest loadable QLF file format version. *)
    let version_qlf_load = T.constant "PL_VERSION_QLF_LOAD" T.int

    (* A hash that represents the VM instructions and their arguments. *)
    let version_vm = T.constant "PL_VERSION_VM" T.int

    (* A hash that represents the names, arities and properties of all built-in predicates defined in C. If this function is called before PL_initialise() it returns 0.  *)
    let version_built_in = T.constant "PL_VERSION_BUILT_IN" T.int

  end

end

