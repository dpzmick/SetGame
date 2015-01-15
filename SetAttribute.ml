open Core.Std

type t = { name: string; values: SetValue.t list }
type name_type = string

let create name values = {name; values}

let number_of_values {values;_} = List.length values
let list_of_values t = t.values

let to_string {name; _} = "SA: " ^ name
let name_type_of_t {name;_} = name
