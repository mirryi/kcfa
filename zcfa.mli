open Abstract

module Constraint : sig
  type t =
    | Concrete of Ast.labeled * Kind.t
    | Subset of Kind.t * Kind.t
    | Conditional of Ast.labeled * Kind.t * Kind.t * Kind.t
  [@@deriving show, eq, ord]
end

module Constraints : Set.S with type elt = Constraint.t

val constraints : Ast.t -> Constraints.t
val solve : Constraints.t -> Abstract.analysis
val analyse : Ast.t -> Abstract.analysis
