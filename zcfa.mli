open Abstract

module Constraint : sig
  type t =
    | Concrete of Ast.labeled * Kind.t
    | Subset of Kind.t * Kind.t
    | Conditional of Ast.labeled * Kind.t * Kind.t * Kind.t
  [@@deriving show, eq, ord]
end

module Constraints : sig
  include Set.S with type elt = Constraint.t

  val pp : t CCSet.printer
end

val label : Ast.t -> Ast.labeled
val constraints : Ast.labeled -> Constraints.t
val solve : Constraints.t -> Abstract.analysis
val analyse : Ast.t -> Abstract.analysis
val analyse_labeled : Ast.labeled -> Abstract.analysis
