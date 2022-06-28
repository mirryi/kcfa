module type MONAD_BASIC = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD = sig
  include MONAD_BASIC

  val map : 'a t -> ('a -> 'b) -> 'b t
  val zip : 'a t -> 'b t -> ('a * 'b) t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Make (M : MONAD_BASIC) : MONAD with type 'a t = 'a M.t = struct
  include M

  let map m k = bind m (fun a -> return (k a))
  let zip m m' = bind m (fun a -> bind m' (fun a' -> return (a, a')))

  module Syntax = struct
    let ( let* ) = bind
    let ( let+ ) = map
    let ( and+ ) = zip
    let ( >>= ) = bind
    let ( >>| ) = map
  end
end

module State (ST : sig
  type t
end) =
struct
  module M = struct
    type 'a t = ST.t -> ST.t * 'a

    let return x s = (s, x)

    let bind m k s =
      let s, a = m s in
      k a s

    let get s = (s, s)
    let put s _ = (s, ())
    let update f = bind get (fun s -> put (f s))
  end

  include M
  include Make (M)
end
