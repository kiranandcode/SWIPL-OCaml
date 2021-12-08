(lang dune 2.9)
(package
 (name swipl)
 (synopsis "Bindings to SWI-Prolog for OCaml")
 (description
   "Swipl are Free Software high-level OCaml bindings to SWI Prolog 8.5, supporting easy interop between OCaml and Prolog.")
 (depends
   (ocaml (>= 4.08.0))
   (conf-swi-prolog (>= 1))
   (ctypes (>= 0.18.0))
   (ppx_deriving (>= 2.0))))
(generate_opam_files true)
(license GPL-3.0+)
(source (uri git+https://gitlab.com/gopiandcode/swipl-ocaml.git))
(bug_reports https://gitlab.com/gopiandcode/swipl-ocaml/issues)
(homepage https://gitlab.com/gopiandcode/swipl-ocaml)
(name swipl)
(authors "Kiran Gopinathan")
(maintainers "kirang@comp.nus.edu.sg")
