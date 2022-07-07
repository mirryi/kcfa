open Kcfa
open Kcfa.Ast

let () =
  (* Example 3.23 *)
  let ast =
    Let'
      ( 18,
        "f",
        Fun' (17, "x", Var' (16, "x")),
        Let'
          ( 15,
            "g",
            Fun' (14, "y", Var' (13, "y")),
            Case'
              ( 12,
                Ctor'
                  ( 11,
                    "Cons",
                    [
                      Var' (10, "f");
                      Ctor' (9, "Cons", [ Var' (8, "g"); Ctor' (7, "Nil", []) ]);
                    ] ),
                [
                  Rule'
                    ( 6,
                      PCtor'
                        ( "Cons",
                          [
                            PVar' (Bind' "a");
                            PCtor'
                              ("Cons", [ PVar' (Bind' "b"); PCtor' ("Nil", []) ]);
                          ] ),
                      Var' (5, "b") );
                  Rule'
                    ( 4,
                      PCtor' ("Cons", [ PVar' (Bind' "c"); PVar' (Bind' "d") ]),
                      Var' (3, "c") );
                  Rule' (2, PCtor' ("Nil", []), Fun' (1, "z", Var' (0, "z")));
                ] ) ) )
  in

  let ac, ae = Zcfa.analyse_labeled ast in
  print_endline ("cache:\n" ^ Abstract.show_cache ac);
  print_newline ();
  print_endline ("env:\n" ^ Abstract.show_env ae)
