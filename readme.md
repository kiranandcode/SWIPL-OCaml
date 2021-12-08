# SWIPL-OCaml 

SWIProlog bindings for OCaml (for SWIProlog version 8.5).

```ocaml
(* initialise SWIProlog *)
let () = Swipl.initialise ()
(* setup the prolog database with some facts *)
let () = Swipl.load_source "hello :- writeln('hello world')."
(* construct a Swipl term in OCaml *)
let hello = Swipl.Syntax.(!"hello")
(* send the term to the Prolog engine *)
let () = Swipl.with_ctx @@ fun ctx -> Swipl.call ctx hello
```

Check out the [Documentation page](https://gopiandcode.github.io/SWIPL-OCaml/swipl/index.html) for a detailed introduction and quick start guide! 

If you know what you're doing already, then maybe check out the
example uses under `examples/`.
