open Flow

module Kind = struct
  type cache = Label.t [@@deriving show, eq, ord]
  type env = Var.t [@@deriving show, eq, ord]
  type t = Cache of cache | Env of env [@@deriving show, eq, ord]
end

module CacheMap = Map.Make (struct
  type t = Kind.cache

  let compare = Kind.compare_cache
end)

module EnvMap = Map.Make (struct
  type t = Kind.env

  let compare = Kind.compare_env
end)

type cache = AstSet.t CacheMap.t
type env = AstSet.t EnvMap.t
type analysis = cache * env
