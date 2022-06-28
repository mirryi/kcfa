module Values : Set.S with type elt = Ast.labeled

module Kind : sig
  type cache = Label.t [@@deriving show, eq, ord]
  type env = Var.t [@@deriving show, eq, ord]
  type t = Cache of cache | Env of env [@@deriving show, eq, ord]
end

module CacheMap : Map.S with type key = Kind.cache
module EnvMap : Map.S with type key = Kind.env

type cache = Values.t CacheMap.t
type env = Values.t EnvMap.t
type analysis = cache * env
