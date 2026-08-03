(* A type error in a file that also uses a sibling module *)
(* Checkers: ocaml-dune *)
let greeting = Helper.greet "world"
let bad : int = "not an int"
