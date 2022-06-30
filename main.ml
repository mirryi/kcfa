open Kcfa
open Kcfa.Ast

let () =
  let ast =
    LLet
      ( 8,
        "f",
        LFun (7, "z", LVar (6, "z")),
        LAp (5, LFun (2, "x", LVar (1, "x")), LFun (4, "y", LVar (3, "y"))) )
  in

  let ccs = Zcfa.constraints ast in

  let ac, ae = Zcfa.solve ccs in
  print_endline ("cache:\n" ^ Abstract.show_cache ac);
  print_newline ();
  print_endline ("env:\n" ^ Abstract.show_env ae)
