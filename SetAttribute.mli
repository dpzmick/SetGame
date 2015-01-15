open Core.Std

type t
type name_type

val create : string -> SetValue.t list -> t

val number_of_values : t -> int
val list_of_values : t -> SetValue.t list

val to_string : t -> string
val name_type_of_t : t -> name_type
