open Kcfa
open Kcfa.Ast

let () =
  (* Example 3.23 *)
  let ast =
    LLet
      ( 14,
        "f",
        LAp (13, LFun (12, "a", LVar (11, "a")), LFun (10, "b", LVar (9, "b"))),
        LCase
          ( 8,
            LCtor (7, "Cons", [ LVar (6, "f"); LCtor (5, "Nil", []) ]),
            [
              LRule (4, "Cons", [ LBind "x"; LBind "xs" ], LVar (3, "x"));
              LRule (2, "Nil", [], LFun (1, "z", LVar (0, "z")));
            ] ) )
  in

  let ac, ae = Zcfa.analyse_labeled ast in
  print_endline ("cache:\n" ^ Abstract.show_cache ac);
  print_newline ();
  print_endline ("env:\n" ^ Abstract.show_env ae)
