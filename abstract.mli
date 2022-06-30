open Containers

(** Sets of abstract values. *)
module Values : sig
  include Set.S with type elt = Ast.labeled

  val pp : t CCSet.printer
end

module Kind : sig
  type cache = Label.t [@@deriving show, eq, ord]
  type env = Var.t [@@deriving show, eq, ord]
  type t = Cache of cache | Env of env [@@deriving show, eq, ord]
end

module CacheMap : sig
  include Map.S with type key = Kind.cache

  val pp : 'a CCMap.printer -> 'a t CCMap.printer
end

module EnvMap : sig
  include Map.S with type key = Kind.env

  val pp : 'a CCMap.printer -> 'a t CCMap.printer
end

type cache = Values.t CacheMap.t [@@deriving show]
(** The type for abstract cache. *)

type env = Values.t EnvMap.t [@@deriving show]
(** The type for abstract environment. *)

type analysis = cache * env [@@deriving show]
(** The type for flow analysis result. *)
