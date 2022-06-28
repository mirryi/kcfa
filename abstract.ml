module Values = Set.Make (struct
  type t = Ast.labeled

  let compare = Ast.compare_labeled
end)

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

type cache = Values.t CacheMap.t
type env = Values.t EnvMap.t
type analysis = cache * env
