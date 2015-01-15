open Core.Std

type t = { name: string; values: SetValue.t list }

let create name values = {name; values}

let number_of_values {values;_} = List.length values
let list_of_values t = t.values

let to_string {name; _} = "SA: " ^ name
