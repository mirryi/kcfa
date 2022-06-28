open Containers

module Values = struct
  module M = Set.Make (struct
    type t = Ast.labeled

    let compare = Ast.compare_labeled
  end)

  include M

  let pp = M.pp Ast.pp_labeled
end

module Kind = struct
  type cache = Label.t [@@deriving show, eq, ord]
  type env = Var.t [@@deriving show, eq, ord]
  type t = Cache of cache | Env of env [@@deriving show, eq, ord]
end

module CacheMap = struct
  module M = Map.Make (struct
    type t = Kind.cache

    let compare = Kind.compare_cache
  end)

  include M

  let pp pp_v = M.pp Kind.pp_cache pp_v
end

module EnvMap = struct
  module M = Map.Make (struct
    type t = Kind.env

    let compare = Kind.compare_env
  end)

  include M

  let pp pp_v = M.pp Kind.pp_env pp_v
end

type cache = Values.t CacheMap.t [@@deriving show]
type env = Values.t EnvMap.t [@@deriving show]
type analysis = cache * env [@@deriving show]
