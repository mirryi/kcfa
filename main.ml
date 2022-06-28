open Kcfa
open Kcfa.Ast

let () =
  (* Example 3.23 *)
  let ast =
    LAp (5, LFun (2, "x", LVar (1, "x")), LFun (4, "y", LVar (3, "y")))
  in

  let ac, ae = Zcfa.analyse_labeled ast in
  print_endline ("cache:\n" ^ Abstract.show_cache ac);
  print_newline ();
  print_endline ("env:\n" ^ Abstract.show_env ae)
