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

val label : t -> Label.t * labeled
val label_of : labeled -> Label.t

module Functions : Set.S with type elt = labeled

val functions : labeled -> Functions.t
