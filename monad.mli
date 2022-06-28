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

module Make (M : MONAD_BASIC) : MONAD with type 'a t = 'a M.t

module State (ST : sig
  type t
end) : sig
  include MONAD with type 'a t = ST.t -> ST.t * 'a

  val get : ST.t t
  val put : ST.t -> unit t
  val update : (ST.t -> ST.t) -> unit t
end
