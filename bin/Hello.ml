open Migrate_parsetree

type typed_code = Typedtree.structure

type typed_signature = Typedtree.signature

(* base: parsetree -> typedtree -> lambda *)
(* bytecode: lambda -> bytegen *)
(* native: lambda -> closure / flambda -> cmm -> asmcomp *)

let code =
  [%str
    let x =
      let a = 1 in
      let b = 2 in
      a + b]

let foo = 1

let x =
  let a = 3 in
  print_endline "hello world";
  foo + a

let env =
  Compmisc.init_path ();
  Compmisc.initial_env ()

let typed_code =
  Typemod.type_implementation "Tuturu.ml" "Tuturu" "Tuturu" env code

let program = Translmod.transl_implementation "Tuturu" typed_code

let () = Format.printf "%a" Printtyped.implementation_with_coercion typed_code

let () = Format.printf "%a" Printlambda.program program

(* -int -float +string *)
(* (a : b (c : d b))' *)
(* (a (c b)) *)
