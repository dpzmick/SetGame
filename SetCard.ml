open Core.Std

type t = (SetAttribute.name_type, SetValue.t) List.Assoc.t

let empty () = []

let of_tuple_list pairs =
    let rec aux head acc =
        match head with
        | (attr, v)::xs -> aux xs (List.Assoc.add acc (SetAttribute.name_type_of_t attr) v)
        | [] -> acc
    in aux pairs (empty ())

let attr_value_exn t attr = List.Assoc.find_exn t (SetAttribute.name_type_of_t attr)

let to_string t =
    let inner = List.fold
        ~init:""
        ~f:(fun acc (_,v) -> match acc with
            | "" -> SetValue.to_string v
            | _  -> acc ^ " " ^ SetValue.to_string v)
        t
    in "SC: {" ^ inner ^ "}"
