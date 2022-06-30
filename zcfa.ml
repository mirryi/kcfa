open Containers
open Abstract

module Constraint = struct
  type t =
    | Concrete of Ast.labeled * Kind.t
    | Subset of Kind.t * Kind.t
    | Conditional of Ast.labeled * Kind.t * Kind.t * Kind.t
    | Called of Kind.t
    | CalledConditional of Kind.t * t list
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

    let rec constraints (astl : Ast.labeled) =
      match astl with
      | LVar (l, x) ->
          (* { r(x) ⊆ C(l) } *)
          singleton (Subset (Env x, Cache l))
      | LFun (l, _, body) ->
          of_list
            [
              (* { {fn x => e₀} ⊆ C(l) } *)
              Concrete (astl, Cache l);
              CalledConditional (Cache l, to_list (constraints body));
            ]
      | LFix (l, f, _, body) ->
          of_list
            [
              (* { {fun f x => e₀} ⊆ C(l) } *)
              Concrete (astl, Cache l);
              CalledConditional
                (Cache l, Concrete (astl, Env f) :: to_list (constraints body));
            ]
      | LAp (l, f, arg) ->
          let l1 = Ast.label_of f in
          let l2 = Ast.label_of arg in

          (* { C(l₁) called } *)
          singleton (Called (Cache l1))
          (* C[t₁] *)
          +++ constraints f
          (* C[t₂] *)
          +++ constraints arg
          +++ conditionals_of_functions (fun f ->
                  match f with
                  | LFun (_, x, body) | LFix (_, _, x, body) ->
                      [
                        (* { {t} ⊆ C(l₁) ⇒ C(l₂) ⊆ r(x) | t = (fn x => t₀) ∈ Term } *)
                        Conditional (f, Cache l1, Cache l2, Env x);
                        (* { {t} ⊆ C(l₁) ⇒ C(l₀) ⊆ C(l) | t = (fn x => t₀) ∈ Term } *)
                        Conditional
                          (f, Cache l1, Cache (Ast.label_of body), Cache l);
                      ]
                  | _ -> [])
      | LLet (l, x, e1, e2) ->
          let l1 = Ast.label_of e1 in
          let l2 = Ast.label_of e2 in
          (* C[t₁] *)
          constraints e1
          (* C[t₂] *)
          +++ constraints e2
          (* { C(l₁) ⊆ r(x) } *)
          +++ singleton (Subset (Cache l1, Env x))
          (* { C(l₂) ⊆ C(l) } *)
          +++ singleton (Subset (Cache l2, Cache l))
      | LInt (_, _) -> empty
      | LBin (_, _, e1, e2) -> constraints e1 +++ constraints e2
    in
    constraints astl
end

module Solver = struct
  module Node = Kind

  module NodeSet = struct
    module M = Set.Make (Node)
    include M

    let pp = M.pp Node.pp
  end

  module NodeMap = struct
    module M = Map.Make (Node)
    include M

    let pp pp_v = M.pp Node.pp pp_v
  end

  module State = struct
    type work = Node.t list [@@deriving show]
    (** The worklist. *)

    type data = Values.t NodeMap.t [@@deriving show]
    type edges = Constraint.t list NodeMap.t [@@deriving show]

    type called = Values.t [@@deriving show]
    (** The set of called values. *)

    type used_called = NodeSet.t [@@deriving show]

    type t = work * data * edges * called * used_called [@@deriving show]
    (** The type of the state. *)

    let _ = pp

    let init : t =
      ([], NodeMap.empty, NodeMap.empty, Values.empty, NodeSet.empty)

    let find_data (p : Node.t) ((_, d, _, _, _) : t) : Values.t =
      NodeMap.find_opt p d |> Option.value ~default:Values.empty

    let find_edge (p : Node.t) ((_, _, e, _, _) : t) : Constraint.t list =
      NodeMap.find_opt p e |> Option.value ~default:[]

    (** [is_called p] returns [true] if any t ∈ C(l) / t ∈ r(x) is in the
        set of called terms. *)
    let is_called (p : Node.t) ((_, _, _, r, _) as s : t) : bool =
      not (Values.disjoint (find_data p s) r)

    let unused_called_conditional (p : Node.t) ((_, _, _, _, u) : t) : bool =
      not (u |> NodeSet.mem p)

    let extend_work (p : Node.t) ((w, d, e, r, u) : t) : t =
      let w = p :: w in
      (w, d, e, r, u)

    let extend_edge (p : Node.t) (cc : Constraint.t) ((w, d, e, r, u) : t) : t =
      let e = NodeMap.add p (cc :: find_edge p (w, d, e, r, u)) e in
      (w, d, e, r, u)

    let extend_used_called (p : Node.t) ((w, d, e, r, u) : t) : t =
      let u = NodeSet.add p u in
      (w, d, e, r, u)

    let add (q : Node.t) (dq' : Values.t) ((w, d, e, r, u) as s : t) : t =
      let dq = find_data q s in
      match Values.subset dq' dq with
      | true | false ->
          let d = NodeMap.add q (Values.union dq dq') d in
          let w = q :: w in
          (w, d, e, r, u)

    let add_called (q : Node.t) ((w, d, e, r, u) : t) : t =
      let values = find_data q (w, d, e, r, u) in
      let cardinal = Values.cardinal r in
      let r = Values.union values r in
      let w = if cardinal <> Values.cardinal r then q :: w else w in
      (w, d, e, r, u)
  end

  let init (ccs : Constraints.t) : State.t =
    let open State in
    let s =
      ccs |> Constraints.to_seq
      |> Seq.fold_left
           (fun s (cc : Constraint.t) ->
             match cc with
             | Concrete (t, p) -> s |> add p (Values.singleton t)
             | Subset (p1, _p2) -> s |> extend_edge p1 cc
             | Conditional (_t, p, p1, _p2) ->
                 s |> extend_edge p1 cc |> extend_edge p cc
             | Called p -> s |> extend_edge p cc
             | CalledConditional (p, _ccs') -> s |> extend_edge p cc)
           State.init
    in
    s

  let rec iterate : State.t -> State.t = function
    | ([], _, _, _, _) as s -> s
    | (q :: w, d, e, r, u) as s ->
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
                     else s
                 | Called p -> s |> add_called p
                 | CalledConditional (p, ccs') ->
                     if unused_called_conditional p s && is_called p s then
                       ccs'
                       |> List.fold_left (fun s cc -> s |> extend_edge p cc) s
                       |> extend_work p |> extend_used_called p
                     else s)
               (w, d, e, r, u)
        in
        iterate s

  let solve (ccs : Constraints.t) : Abstract.analysis =
    let _, d, _, _, _ = ccs |> init |> iterate in
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
