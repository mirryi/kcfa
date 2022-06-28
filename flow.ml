module AstSet = Set.Make (struct
  type t = Ast.labeled

  let compare = Ast.compare_labeled
end)
