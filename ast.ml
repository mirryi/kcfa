open Containers

type op = Add | Sub | Mult | Div [@@deriving show, eq, ord]

type t =
  | Var of Var.t
  | Fun of Var.t * t
  | Fix of Var.t * Var.t * t
  | Ap of t * t
  | Let of Var.t * t * t
  | Int of int
  | Bin of op * t * t
  | Ctor of Var.t * t list
  | Case of t * rule list

and rule = Rule of pat * t
and pat = PCtor of Var.t * pat list | PVar of bind
and bind = Bind of Var.t [@@deriving show, eq, ord]

type t' =
  | Var' of Label.t * Var.t
  | Fun' of Label.t * Var.t * t'
  | Fix' of Label.t * Var.t * Var.t * t'
  | Ap' of Label.t * t' * t'
  | Let' of Label.t * Var.t * t' * t'
  | Int' of Label.t * int
  | Bin' of Label.t * op * t' * t'
  | Ctor' of Label.t * Var.t * t' list
  | Case' of Label.t * t' * rule' list

and rule' = Rule' of Label.t * pat' * t'
and pat' = PCtor' of Var.t * pat' list | PVar' of bind'
and bind' = Bind' of Var.t [@@deriving show, eq, ord]

module LabelMonad = struct
  include Monad.State (Label)
  open Syntax

  let next =
    let* _ = update Label.next in
    get
end

let label ast =
  let open LabelMonad in
  let open LabelMonad.Syntax in
  let rec label ast =
    let* l = next in
    match ast with
    | Var x -> Var' (l, x) |> return
    | Fun (x, body) ->
        let* body = label body in
        Fun' (l, x, body) |> return
    | Fix (f, x, body) ->
        let* body = label body in
        Fix' (l, f, x, body) |> return
    | Ap (f, arg) ->
        let* f = label f in
        let* arg = label arg in
        Ap' (l, f, arg) |> return
    | Let (x, e1, e2) ->
        let* e1 = label e1 in
        let* e2 = label e2 in
        Let' (l, x, e1, e2) |> return
    | Int n -> Int' (l, n) |> return
    | Bin (op, e1, e2) ->
        let* e1 = label e1 in
        let* e2 = label e2 in
        Bin' (l, op, e1, e2) |> return
    | Ctor (name, args) ->
        let* args = label_list args in
        Ctor' (l, name, args) |> return
    | Case (scrut, rs) ->
        let* scrut = label scrut in
        let* rs = label_rules rs in
        Case' (l, scrut, rs) |> return
  and label_list = function
    | [] -> [] |> return
    | e :: es ->
        let* e = label e in
        label_list es >>| fun es -> e :: es
  and label_rules = function
    | [] -> [] |> return
    | r :: rs ->
        let* r = label_rule r in
        label_rules rs >>| fun rs -> r :: rs
  and label_rule r =
    let* l = next in
    match r with
    | Rule (p, body) ->
        let* p = label_pat p in
        let* body = label body in
        Rule' (l, p, body) |> return
  and label_pats = function
    | [] -> [] |> return
    | p :: pats ->
        let* p = label_pat p in
        label_pats pats >>| fun pats -> p :: pats
  and label_pat = function
    | PCtor (name, pats) ->
        let* pats = label_pats pats in
        PCtor' (name, pats) |> return
    | PVar b ->
        let* b = label_bind b in
        PVar' b |> return
  and label_bind b = match b with Bind x -> Bind' x |> return in
  label ast Label.init

let label_of = function
  | Var' (l, _)
  | Fun' (l, _, _)
  | Fix' (l, _, _, _)
  | Ap' (l, _, _)
  | Let' (l, _, _, _)
  | Int' (l, _)
  | Bin' (l, _, _, _)
  | Ctor' (l, _, _)
  | Case' (l, _, _) ->
      l

module Functions = struct
  module M = Set.Make (struct
    type nonrec t = t'

    let compare = compare_t'
  end)

  include M

  let pp = M.pp pp_t'
end

let functions astl =
  let open Functions in
  let rec functions = function
    | Var' (_, _) -> empty
    | Fun' (_, _, body) as astl -> union (singleton astl) (functions body)
    | Fix' (_, _, _, body) as astl -> union (singleton astl) (functions body)
    | Ap' (_, f, arg) -> union (functions f) (functions arg)
    | Let' (_, _, e1, e2) -> union (functions e1) (functions e2)
    | Int' (_, _) -> empty
    | Bin' (_, _, e1, e2) -> union (functions e1) (functions e2)
    | Ctor' (_, _, args) ->
        args |> List.map functions |> List.fold_left union empty
    | Case' (_, scrut, rs) -> union (functions scrut) (functions_rules rs)
  and functions_rules rs =
    rs |> List.map functions_rule |> List.fold_left union empty
  and functions_rule = function Rule' (_, _, body) -> functions body in

  functions astl

module Constructors = struct
  module M = Set.Make (struct
    type nonrec t = t'

    let compare = compare_t'
  end)

  include M

  let pp = M.pp pp_t'
end

let constructors astl =
  let open Constructors in
  let rec constructors = function
    | Var' (_, _) -> empty
    | Fun' (_, _, body) -> constructors body
    | Fix' (_, _, _, body) -> constructors body
    | Ap' (_, f, arg) -> union (constructors f) (constructors arg)
    | Let' (_, _, e1, e2) -> union (constructors e1) (constructors e2)
    | Int' (_, _) -> empty
    | Bin' (_, _, e1, e2) -> union (constructors e1) (constructors e2)
    | Ctor' (_, _, args) as astl ->
        union (singleton astl)
          (args |> List.map constructors |> List.fold_left union empty)
    | Case' (_, scrut, rs) -> union (constructors scrut) (constructors_rules rs)
  and constructors_rules rs =
    rs |> List.map constructors_rule |> List.fold_left union empty
  and constructors_rule = function Rule' (_, _, body) -> constructors body in

  constructors astl
