open Abstract
(**
   0-CFA analysis.
 *)

(** Constraints for constraint-based analysis. *)
module Constraint : sig
  (** The type for constraints. *)
  type t =
    | Concrete of Ast.labeled * Kind.t
    | Subset of Kind.t * Kind.t
    | Conditional of Ast.labeled * Kind.t * Kind.t * Kind.t
    | Called of Kind.t
    | CalledConditional of Kind.t * t list
  [@@deriving show, eq, ord]
end

(** Set of constraints for constraint-based analysis. *)
module Constraints : sig
  include Set.S with type elt = Constraint.t

  val pp : t CCSet.printer
end

val label : Ast.t -> Ast.labeled
(** [label ast] returns [astl] where [astl] is [ast] with unique labels. *)

val constraints : Ast.labeled -> Constraints.t
(** [constraints astl] computes the set of constraints for [astl]. *)

val solve : Constraints.t -> Abstract.analysis
(** [solve ccs] solves the constraints [ccs] and returns the analysis result. *)

val analyse : Ast.t -> Abstract.analysis
(** [analyse ast] performs 0-CFA analysis on [ast]. *)

val analyse_labeled : Ast.labeled -> Abstract.analysis
(** [analyse_labeled astl] is [analyse] but for [!type:Ast.labeled] *)
