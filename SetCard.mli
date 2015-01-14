open Core.Std

type t
val empty : unit -> t

val of_tuple_list : (SetAttribute.t * SetValue.t) list -> t

val attr_value_exn : t -> SetAttribute.t -> SetValue.t

val to_string : t -> string
