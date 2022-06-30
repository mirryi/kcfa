(** The type for binary operations. *)
type op = Add | Sub | Mult | Div [@@deriving show, eq, ord]

(** The type for syntax trees. *)
type t =
  | Var of Var.t
  | Fun of Var.t * t
  | Ap of t * t
  | Let of Var.t * t * t
  | Int of int
  | Bin of op * t * t
[@@deriving show, eq, ord]

(** The type for labeled syntax trees. *)
type labeled =
  | LVar of Label.t * Var.t
  | LFun of Label.t * Var.t * labeled
  | LAp of Label.t * labeled * labeled
  | LLet of Label.t * Var.t * labeled * labeled
  | LInt of Label.t * int
  | LBin of Label.t * op * labeled * labeled
[@@deriving show, eq, ord]

val label : t -> Label.t * labeled
(** [label ast] returns [astl] where [astl] is [ast] with all expressions
    labeled. *)

val label_of : labeled -> Label.t
(** [label_of astl] returns the label of [astl]. *)

module Functions : sig
  include Set.S with type elt = labeled

  val pp : t CCSet.printer
end

val functions : labeled -> Functions.t
(** [functions astl] accumulates the set of functions in [astl]. *)
