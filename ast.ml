type op = Add | Sub | Mult | Div [@@deriving show, eq, ord]

type t =
  | Var of Var.t
  | Fun of Var.t * t
  | Ap of t * t
  | Let of Var.t * t * t
  | Int of int
  | Bin of op * t * t
[@@deriving show, eq, ord]

type labeled =
  | LVar of Label.t * Var.t
  | LFun of Label.t * Var.t * labeled
  | LAp of Label.t * labeled * labeled
  | LLet of Label.t * Var.t * labeled * labeled
  | LInt of Label.t * int
  | LBin of Label.t * op * labeled * labeled
[@@deriving show, eq, ord]

module LabelMonad = struct
  include Monad.State (Label)
  open Syntax

  let next =
    let* _ = update Label.next in
    get
end

(** [label ast] returns [astl] where [astl] is [ast] with all expressions
    labeled. *)
let label ast =
  let rec label (ast : t) =
    let open LabelMonad in
    let open LabelMonad.Syntax in
    let* l = next in

    match ast with
    | Var x -> LVar (l, x) |> return
    | Fun (x, body) ->
        let* body = label body in
        LFun (l, x, body) |> return
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
  in
  label ast Label.init

let label_of = function
  | LVar (l, _)
  | LFun (l, _, _)
  | LAp (l, _, _)
  | LLet (l, _, _, _)
  | LInt (l, _)
  | LBin (l, _, _, _) ->
      l

module Functions = Set.Make (struct
  type nonrec t = labeled

  let compare = compare_labeled
end)

(** [functions astl] accumulates the set of functions in [astl]. *)
let functions astl =
  let rec functions (astl : labeled) =
    let open Functions in
    match astl with
    | LVar (_, _) -> empty
    | LFun (_, _, body) -> union (singleton astl) (functions body)
    | LAp (_, f, arg) -> union (functions f) (functions arg)
    | LLet (_, _, e1, e2) -> union (functions e1) (functions e2)
    | LInt (_, _) -> empty
    | LBin (_, _, e1, e2) -> union (functions e1) (functions e2)
  in

  functions astl
