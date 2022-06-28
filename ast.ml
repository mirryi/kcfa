type op = Add | Sub | Mult | Div [@@deriving show, eq, ord]

type t =
  | Var of Var.t
  | Fun of Var.t * t
  | Ap of t * t
  | Int of int
  | Bin of op * t * t
[@@deriving show, eq, ord]

type labeled =
  | Var of Label.t * Var.t
  | Fun of Label.t * Var.t * labeled
  | Ap of Label.t * labeled * labeled
  | Int of Label.t * int
  | Bin of Label.t * op * labeled * labeled
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
    | Var x -> Var (l, x) |> return
    | Fun (x, body) ->
        let* body = label body in
        Fun (l, x, body) |> return
    | Ap (f, arg) ->
        let* f = label f in
        let* arg = label arg in
        Ap (l, f, arg) |> return
    | Int n -> Int (l, n) |> return
    | Bin (op, e1, e2) ->
        let* e1 = label e1 in
        let* e2 = label e2 in
        Bin (l, op, e1, e2) |> return
  in
  label ast Label.init

let label_of = function
  | Var (l, _) | Fun (l, _, _) | Ap (l, _, _) | Int (l, _) | Bin (l, _, _, _) ->
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
    | Var (_, _) -> empty
    | Fun (_, _, body) -> union (singleton astl) (functions body)
    | Ap (_, f, arg) -> union (functions f) (functions arg)
    | Int (_, _) -> empty
    | Bin (_, _, e1, e2) -> union (functions e1) (functions e2)
  in

  functions astl
