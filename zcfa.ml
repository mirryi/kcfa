open Containers
open Abstract

module Constraint = struct
  type t =
    | Concrete of Ast.t' * Kind.t
    | Subset of Kind.t * Kind.t
    | Conditional of Ast.t' * Kind.t * Kind.t * Kind.t
  [@@deriving show, eq, ord]
end

module Constraints = struct
  module M = Set.Make (Constraint) [@@deriving show]
  include M

  let pp = M.pp Constraint.pp

  let constraints astl =
    let ( +++ ) = union in

    let functions = Ast.functions astl in
    let conditionals_of_functions f =
      functions |> Ast.Functions.to_seq |> Seq.map f |> Seq.flat_map Seq.of_list
      |> of_seq
    in
    let constructors = Ast.constructors astl in
    let conditionals_of_constructors f =
      constructors |> Ast.Constructors.to_seq |> Seq.map f
      |> Seq.flat_map Seq.of_list |> of_seq
    in

    let rec constraints (astl : Ast.t') =
      match astl with
      | Var' (l, x) -> singleton (Subset (Env x, Cache l))
      | Fun' (l, _, body) as astl ->
          singleton (Concrete (astl, Cache l)) +++ constraints body
      | Fix' (l, f, _, body) as astl ->
          singleton (Concrete (astl, Cache l))
          +++ constraints body
          +++ singleton (Concrete (astl, Env f))
      | Ap' (l, f, arg) ->
          let l1 = Ast.label_of f in
          let l2 = Ast.label_of arg in
          constraints f +++ constraints arg
          +++ conditionals_of_functions (fun f ->
                  match f with
                  | Fun' (_, x, body) | Fix' (_, _, x, body) ->
                      [
                        Conditional (f, Cache l1, Cache l2, Env x);
                        Conditional
                          (f, Cache l1, Cache (Ast.label_of body), Cache l);
                      ]
                  | _ -> [])
      | Let' (l, x, e1, e2) ->
          let l1 = Ast.label_of e1 in
          let l2 = Ast.label_of e2 in
          constraints e1 +++ constraints e2
          +++ singleton (Subset (Cache l1, Env x))
          +++ singleton (Subset (Cache l2, Cache l))
      | Int' (_, _) -> empty
      | Bin' (_, _, e1, e2) -> constraints e1 +++ constraints e2
      | Ctor' (l, _, args) ->
          singleton (Concrete (astl, Cache l))
          +++ (args |> List.map constraints |> List.fold_left ( +++ ) empty)
      | Case' (l, scrut, rs) ->
          let scrut_l = Ast.label_of scrut in
          let rule_constraints =
            rs
            |> List.map (function Ast.Rule' (_, name, binds, body) ->
                   singleton (Subset (Cache (Ast.label_of body), Cache l))
                   +++ constraints body
                   +++ conditionals_of_constructors (function
                         | Ctor' (_, name', args) as e
                           when String.equal name name' ->
                             List.combine binds args
                             |> List.map (fun (b, arg) ->
                                    let arg_l = Ast.label_of arg in
                                    match b with
                                    | Ast.Bind' x ->
                                        Constraint.Conditional
                                          (e, Cache scrut_l, Cache arg_l, Env x))
                         | _ -> []))
            |> List.fold_left ( +++ ) empty
          in
          constraints scrut +++ rule_constraints
    in
    constraints astl
end

module Solver = struct
  module Node = Kind

  module NodeMap = struct
    module M = Map.Make (Node)
    include M

    let pp pp_v = M.pp Node.pp pp_v
  end

  module State = struct
    type work = Node.t list [@@deriving show]
    type data = Values.t NodeMap.t [@@deriving show]
    type edges = Constraint.t list NodeMap.t [@@deriving show]
    type t = work * data * edges [@@deriving show]

    let _ = pp
    let init : t = ([], NodeMap.empty, NodeMap.empty)

    let find_data (p : Node.t) ((_, d, _) : t) : Values.t =
      NodeMap.find_opt p d |> Option.value ~default:Values.empty

    let find_edge (p : Node.t) ((_, _, e) : t) : Constraint.t list =
      NodeMap.find_opt p e |> Option.value ~default:[]

    let extend_edge (p : Node.t) (cc : Constraint.t) ((w, d, e) : t) : t =
      let e = NodeMap.add p (cc :: find_edge p (w, d, e)) e in
      (w, d, e)

    let add (q : Node.t) (dq' : Values.t) (s : t) : t =
      let dq = find_data q s in
      match Values.subset dq' dq with
      | true -> s
      | false ->
          let w, d, e = s in
          let d = NodeMap.add q (Values.union dq dq') d in
          let w = q :: w in
          (w, d, e)
  end

  let init (ccs : Constraints.t) : State.t =
    let open State in
    ccs |> Constraints.to_seq
    |> Seq.fold_left
         (fun s (cc : Constraint.t) ->
           match cc with
           | Concrete (t, p) -> s |> add p (Values.singleton t)
           | Subset (p1, _p2) -> s |> extend_edge p1 cc
           | Conditional (_t, p, p1, _p2) ->
               s |> extend_edge p1 cc |> extend_edge p cc)
         State.init

  let rec iterate : State.t -> State.t = function
    | ([], _, _) as s -> s
    | (q :: w, d, e) as s ->
        let open State in
        let ccs = find_edge q s in

        let s =
          ccs
          |> List.fold_left
               (fun s (cc : Constraint.t) ->
                 match cc with
                 | Concrete (_, _) -> s
                 | Subset (p1, p2) -> s |> add p2 (find_data p1 s)
                 | Conditional (t, p, p1, p2) ->
                     if Values.mem t (find_data p s) then
                       s |> add p2 (find_data p1 s)
                     else s)
               (w, d, e)
        in
        iterate s

  let solve (ccs : Constraints.t) : Abstract.analysis =
    let _, d, _ = ccs |> init |> iterate in
    NodeMap.fold
      (fun p ts (ac, ae) ->
        match p with
        | Cache l -> (CacheMap.add l ts ac, ae)
        | Env x -> (ac, EnvMap.add x ts ae))
      d
      (CacheMap.empty, EnvMap.empty)
end

let label ast =
  let _, astl = Ast.label ast in
  astl

let constraints = Constraints.constraints
let solve = Solver.solve
let analyse_labeled astl = astl |> constraints |> solve
let analyse ast = ast |> label |> analyse_labeled
