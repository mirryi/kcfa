(** The type for binary operations. *)
type op = Add | Sub | Mult | Div [@@deriving show, eq, ord]

(** The type for syntax trees. *)
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

(** The type for labeled syntax trees. *)
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

and rule' = Rule' of Label.t * Var.t * bind' list * t'
and bind' = Bind' of Var.t [@@deriving show, eq, ord]

val label : t -> Label.t * t'
(** [label ast] returns [astl] where [astl] is [ast] with all expressions
    labeled. *)

val label_of : t' -> Label.t
(** [label_of astl] returns the label of [astl]. *)

module Functions : sig
  include Set.S with type elt = t'

  val pp : t CCSet.printer
end

val functions : t' -> Functions.t
(** [functions astl] accumulates the set of functions in [astl]. *)

module Constructors : sig
  include Set.S with type elt = t'

  val pp : t CCSet.printer
end

val constructors : t' -> Constructors.t
(** [constructors astl] accumulates the set of data constructions in [astl]. *)
