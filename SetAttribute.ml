open Core.Std

type t = { id: int; name: string option; values: SetValue.t list }

(* TODO check if all values given different ids (or hide the ids somehow) *)
let create id name values = { id; name = Some name; values }
let create_unnamed id values = {id; name = None; values }

let number_of_values {values;_} = List.length values
let list_of_values t = t.values

let to_string {id; name; _} = match name with
| Some n -> "SA: {" ^ Int.to_string id ^ "; " ^ n ^ "}"
| None   -> "SA: {" ^ Int.to_string id ^ "}"
