type t = int [@@deriving show, eq, ord]

val init : t
val next : t -> t
