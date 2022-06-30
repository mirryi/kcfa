type t = int [@@deriving show, eq, ord]
(** The type of labels. *)

val init : t
(** [init] is the initial label. *)

val next : t -> t
(** [next l] is the next label. *)
