open Core.Std

type t = int * string option

let create i s = (i, Some s)
let create_unnamed i = (i, None)

let to_string = function
    | (i, Some s) -> "SV: {" ^ (String.concat ~sep:" " [Int.to_string i; s]) ^ "}"
    | (i, None)   -> "SV: {" ^ Int.to_string i ^ "}"
