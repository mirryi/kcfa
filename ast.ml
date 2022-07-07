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

and rule = Rule of Var.t * bind list * t [@@deriving show, eq, ord]
and bind = Bind of Var.t

type labeled =
  | LVar of Label.t * Var.t
  | LFun of Label.t * Var.t * labeled
  | LFix of Label.t * Var.t * Var.t * labeled
  | LAp of Label.t * labeled * labeled
  | LLet of Label.t * Var.t * labeled * labeled
  | LInt of Label.t * int
  | LBin of Label.t * op * labeled * labeled
  | LCtor of Label.t * Var.t * labeled list
  | LCase of Label.t * labeled * labeled_rule list

and labeled_rule = LRule of Label.t * Var.t * labeled_bind list * labeled
and labeled_bind = LBind of Var.t [@@deriving show, eq, ord]

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
    | Var x -> LVar (l, x) |> return
    | Fun (x, body) ->
        let* body = label body in
        LFun (l, x, body) |> return
    | Fix (f, x, body) ->
        let* body = label body in
        LFix (l, f, x, body) |> return
    | Ap (f, arg) ->
        let* f = label f in
        let* arg = label arg in
        LAp (l, f, arg) |> return
    | Let (x, e1, e2) ->
        let* e1 = label e1 in
        let* e2 = label e2 in
        LLet (l, x, e1, e2) |> return
    | Int n -> LInt (l, n) |> return
    | Bin (op, e1, e2) ->
        let* e1 = label e1 in
        let* e2 = label e2 in
        LBin (l, op, e1, e2) |> return
    | Ctor (name, args) ->
        let* args = label_list args in
        LCtor (l, name, args) |> return
    | Case (scrut, rs) ->
        let* scrut = label scrut in
        let* rs = label_rules rs in
        LCase (l, scrut, rs) |> return
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
    | Rule (name, binds, body) ->
        let* binds = label_binds binds in
        let* body = label body in
        LRule (l, name, binds, body) |> return
  and label_binds = function
    | [] -> [] |> return
    | b :: binds ->
        let* b = label_bind b in
        label_binds binds >>| fun binds -> b :: binds
  and label_bind b = match b with Bind x -> LBind x |> return in
  label ast Label.init

let label_of = function
  | LVar (l, _)
  | LFun (l, _, _)
  | LFix (l, _, _, _)
  | LAp (l, _, _)
  | LLet (l, _, _, _)
  | LInt (l, _)
  | LBin (l, _, _, _)
  | LCtor (l, _, _)
  | LCase (l, _, _) ->
      l

module Functions = struct
  module M = Set.Make (struct
    type nonrec t = labeled

    let compare = compare_labeled
  end)

  include M

  let pp = M.pp pp_labeled
end

let functions astl =
  let open Functions in
  let rec functions = function
    | LVar (_, _) -> empty
    | LFun (_, _, body) as astl -> union (singleton astl) (functions body)
    | LFix (_, _, _, body) as astl -> union (singleton astl) (functions body)
    | LAp (_, f, arg) -> union (functions f) (functions arg)
    | LLet (_, _, e1, e2) -> union (functions e1) (functions e2)
    | LInt (_, _) -> empty
    | LBin (_, _, e1, e2) -> union (functions e1) (functions e2)
    | LCtor (_, _, args) ->
        args |> List.map functions |> List.fold_left union empty
    | LCase (_, scrut, rs) -> union (functions scrut) (functions_rules rs)
  and functions_rules rs =
    rs |> List.map functions_rule |> List.fold_left union empty
  and functions_rule = function LRule (_, _, _, body) -> functions body in

  functions astl

module Constructors = struct
  module M = Set.Make (struct
    type nonrec t = labeled

    let compare = compare_labeled
  end)

  include M

  let pp = M.pp pp_labeled
end

let constructors astl =
  let open Constructors in
  let rec constructors = function
    | LVar (_, _) -> empty
    | LFun (_, _, body) -> constructors body
    | LFix (_, _, _, body) -> constructors body
    | LAp (_, f, arg) -> union (constructors f) (constructors arg)
    | LLet (_, _, e1, e2) -> union (constructors e1) (constructors e2)
    | LInt (_, _) -> empty
    | LBin (_, _, e1, e2) -> union (constructors e1) (constructors e2)
    | LCtor (_, _, args) as astl ->
        union (singleton astl)
          (args |> List.map constructors |> List.fold_left union empty)
    | LCase (_, scrut, rs) -> union (constructors scrut) (constructors_rules rs)
  and constructors_rules rs =
    rs |> List.map constructors_rule |> List.fold_left union empty
  and constructors_rule = function
    | LRule (_, _, _, body) -> constructors body
  in

  constructors astl
