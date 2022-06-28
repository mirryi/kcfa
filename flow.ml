type abstract = Cache of Label.t | Env of Var.t [@@deriving show, eq, ord]

module Constraint = struct
  type t =
    | Concrete of Ast.labeled * abstract
    | Subset of abstract * abstract
    | Conditional of Ast.labeled * abstract * abstract * abstract
  [@@deriving show, eq, ord]
end

module Constraints = struct
  include Set.Make (Constraint)

  (** [constraints astl] computes the set of constraints used to perform
      constraint-based 0-CFA analysis. *)
  let constraints astl =
    let ( +++ ) = union in

    let functions = Ast.functions astl in
    let conditionals f =
      functions |> Ast.Functions.to_seq |> Seq.filter_map f |> of_seq
    in

    let rec constraints (astl : Ast.labeled) =
      match astl with
      | Var (l, x) -> singleton (Subset (Env x, Cache l))
      | Fun (l, _, body) as f ->
          singleton (Concrete (f, Cache l)) +++ constraints body
      | Ap (l, f, arg) ->
          let l1 = Ast.label_of f in
          let l2 = Ast.label_of arg in
          constraints f +++ constraints arg
          +++ conditionals (fun f ->
                  match f with
                  | Fun (_, x, _) ->
                      Some (Conditional (f, Cache l1, Cache l2, Env x))
                  | _ -> None)
          +++ conditionals (fun f ->
                  match f with
                  | Fun (_, _, body) ->
                      Some
                        (Conditional
                           (f, Cache l1, Cache (Ast.label_of body), Cache l))
                  | _ -> None)
      | Let (l, x, e1, e2) ->
          let l1 = Ast.label_of e1 in
          let l2 = Ast.label_of e2 in
          constraints e1 +++ constraints e2
          +++ singleton (Subset (Cache l1, Env x))
          +++ singleton (Subset (Cache l2, Cache l))
      | Int (_, _) -> empty
      | Bin (_, _, e1, e2) -> constraints e1 +++ constraints e2
    in
    constraints astl
end

module Solver = struct
  module AstSet = Set.Make (struct
    type t = Ast.labeled

    let compare = Ast.compare_labeled
  end)

  module Node = struct
    type t = abstract

    let compare = compare_abstract
  end

  module NodeMap = Map.Make (Node)

  module Data = struct
    type t = AstSet.t NodeMap.t

    let mk constrs =
      constrs |> Constraints.to_seq
      |> Seq.filter_map (fun (constr : Constraint.t) ->
             match constr with Concrete (t, p) -> Some (p, t) | _ -> None)
      |> NodeMap.of_seq
  end

  let solve constrs =
    let w = [] in
    let d = Data.mk constrs in
    ()
end

let analyse ast =
  let _, astl = Ast.label ast in
  let constrs = Constraints.constraints astl in
  constrs
