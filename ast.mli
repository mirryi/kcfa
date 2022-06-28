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
  | Var of Label.t * Var.t
  | Fun of Label.t * Var.t * labeled
  | Ap of Label.t * labeled * labeled
  | Let of Label.t * Var.t * labeled * labeled
  | Int of Label.t * int
  | Bin of Label.t * op * labeled * labeled
[@@deriving show, eq, ord]

val label : t -> Label.t * labeled
val label_of : labeled -> Label.t

module Functions : Set.S with type elt = labeled

val functions : labeled -> Functions.t
