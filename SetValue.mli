open Core.Std

(* type t *)
type t

val create : int -> string -> t
val create_unnamed : int -> t

val to_string : t -> string
