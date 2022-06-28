module AstSet = Set.Make (struct
  type t = Ast.labeled

  let compare = Ast.compare_labeled
end)

module Abstract = struct
  type cache = Label.t [@@deriving show, eq, ord]
  type env = Var.t [@@deriving show, eq, ord]
  type t = Cache of cache | Env of env [@@deriving show, eq, ord]
end

module AbstractCacheMap = Map.Make (struct
  type t = Abstract.cache

  let compare = Abstract.compare_cache
end)

module AbstractEnvMap = Map.Make (struct
  type t = Abstract.env

  let compare = Abstract.compare_env
end)

type abstract_cache = AstSet.t AbstractCacheMap.t
type abstract_env = AstSet.t AbstractEnvMap.t
type t = abstract_cache * abstract_env

module Constraint = struct
  type t =
    | Concrete of Ast.labeled * Abstract.t
    | Subset of Abstract.t * Abstract.t
    | Conditional of Ast.labeled * Abstract.t * Abstract.t * Abstract.t
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
  module Node = Abstract
  module NodeMap = Map.Make (Node)

  module State = struct
    type work = Node.t list
    type data = AstSet.t NodeMap.t
    type edges = Constraint.t list NodeMap.t
    type t = work * data * edges

    let init = ([], NodeMap.empty, NodeMap.empty)

    let find_data p (_, d, _) =
      NodeMap.find_opt p d |> Option.value ~default:AstSet.empty

    let find_edge p (_, _, e) = NodeMap.find_opt p e |> Option.value ~default:[]

    let extend_edge p cc (w, d, e) =
      let e = NodeMap.add p (cc :: find_edge p (w, d, e)) e in
      (w, d, e)

    let add q dq' ((w, d, e) as s) =
      let dq = find_data q s in
      match AstSet.subset dq' dq with
      | true | false ->
          let d = NodeMap.add q (AstSet.union dq dq') d in
          let w = q :: w in
          (w, d, e)
  end

  let init ccs =
    let open State in
    ccs |> Constraints.to_seq
    |> Seq.fold_left
         (fun s (cc : Constraint.t) ->
           match cc with
           | Concrete (t, p) -> s |> add p (AstSet.singleton t)
           | Subset (p1, _p2) -> s |> extend_edge p1 cc
           | Conditional (_t, p, p1, _p2) ->
               s |> extend_edge p1 cc |> extend_edge p cc)
         State.init

  let iterate = function
    | ([], _, _) as s -> s
    | (q :: w, d, e) as s ->
        let open State in
        let ccs = find_edge q s in

        ccs
        |> List.fold_left
             (fun s (cc : Constraint.t) ->
               match cc with
               | Concrete (_, _) -> s
               | Subset (p1, p2) -> s |> add p2 (find_data p1 s)
               | Conditional (t, p, p1, p2) ->
                   if AstSet.subset (AstSet.singleton t) (find_data p s) then
                     s |> add p2 (find_data p1 s)
                   else s)
             (w, d, e)

  let solve ccs =
    let _, d, _ = init ccs |> iterate in
    NodeMap.fold
      (fun p ts (ac, ae) ->
        match p with
        | Cache l -> (AbstractCacheMap.add l ts ac, ae)
        | Env x -> (ac, AbstractEnvMap.add x ts ae))
      d
      (AbstractCacheMap.empty, AbstractEnvMap.empty)
end

let analyse ast =
  let _, astl = Ast.label ast in
  Constraints.constraints astl |> Solver.solve
