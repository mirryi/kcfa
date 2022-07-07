open Kcfa
open Kcfa.Ast

let () =
  (* Example 3.23 *)
  let ast =
    Let'
      ( 14,
        "f",
        Ap' (13, Fun' (12, "a", Var' (11, "a")), Fun' (10, "b", Var' (9, "b"))),
        Case'
          ( 8,
            Ctor' (7, "Cons", [ Var' (6, "f"); Ctor' (5, "Nil", []) ]),
            [
              Rule' (4, "Cons", [ Bind' "x"; Bind' "xs" ], Var' (3, "x"));
              Rule' (2, "Nil", [], Fun' (1, "z", Var' (0, "z")));
            ] ) )
  in

  let ac, ae = Zcfa.analyse_labeled ast in
  print_endline ("cache:\n" ^ Abstract.show_cache ac);
  print_newline ();
  print_endline ("env:\n" ^ Abstract.show_env ae)
