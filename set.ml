open Core.Std;;

(* A (slightly) generalized set game.
 *
 * Each game has n attributes (Color, Number, Shading, etc) and m possible values for each attribute.
 *
 * A valid set consists of cards for which the following is true:
 * * For every attribute a, either:
 *      c1[a] = c2[a] = ... = cm[a] (all are equal)
 *                      OR
 *      c1[a] != c2[a] != ... != cm[a] (all are different)
 *
 * * The number of cards in the set must be m
 *)

(* TODO think about eliminating the id thing with some sort of comparable interface *)
module SetValue : sig
    type t

    val create : int -> string -> t
    val create_unnamed : int -> t

    val to_string : t -> string
end = struct
    type t = int * string option

    let create i s = (i, Some s)
    let create_unnamed i = (i, None)

    let to_string = function
        | (i, Some s) -> "SV: {" ^ (String.concat ~sep:" " [Int.to_string i; s]) ^ "}"
        | (i, None)   -> "SV: {" ^ Int.to_string i ^ "}"
end

module SetAttribute : sig
    type t

    val create : int -> string -> SetValue.t list -> t
    val create_unnamed : int -> SetValue.t list -> t

    val number_of_values : t -> int
    val list_of_values : t -> SetValue.t list

    val to_string : t -> string
end = struct
    type t = { id: int; name: string option; values: SetValue.t list }

    (* TODO check if all values given different ids (or hide the ids somehow) *)
    let create id name values = { id; name = Some name; values }
    let create_unnamed id values = {id; name = None; values }

    let number_of_values {values;_} = List.length values
    let list_of_values t = t.values

    let to_string {id; name; _} = match name with
    | Some n -> "SA: {" ^ Int.to_string id ^ "; " ^ n ^ "}"
    | None   -> "SA: {" ^ Int.to_string id ^ "}"
end

module SetCard : sig
    type t

    val of_tuple_list : (SetAttribute.t * SetValue.t) list -> t

    val attr_value_exn : t -> SetAttribute.t -> SetValue.t

    val to_string : t -> string
end = struct
    type t = (SetAttribute.t, SetValue.t) List.Assoc.t

    let create_empty () = []

    let of_tuple_list pairs =
        let rec aux head acc =
            match head with
            | (attr, v)::xs -> aux xs (List.Assoc.add acc attr v)
            | [] -> acc
        in aux pairs (create_empty ())

    let attr_value_exn t attr = List.Assoc.find_exn t attr

    let to_string = List.fold
        ~init:""
        ~f:(fun acc (a,v) -> acc ^ " " ^ SetValue.to_string v)
end

module SetGame : sig
    type t = {
        board: SetCard.t array;
        deck: SetCard.t list;
        attributes: SetAttribute.t list;
        m : int (* the number of values per attribute *)
    }

    val create : SetAttribute.t list -> t

    val make_deck : SetAttribute.t list -> SetCard.t list

    val validate_set : t -> SetCard.t list -> bool
    (* val all_equal : 'a list -> bool *)

end = struct
    type t = {
        board: SetCard.t array;
        deck: SetCard.t list;
        attributes: SetAttribute.t list;
        m : int (* the number of values per attribute *)
    }

    (* private helpers *)

    (* pulled out an not using a fold so I can termiate early *)
    let all_equal lst =
        let rec aux curr last = match curr with
            | x::xs -> if x = last then aux xs x else false
            | []    -> true
        in match lst with
            | x::xs -> aux xs x
            | [] -> true

    let check_attr set attr =
        let set_values = List.map ~f:(fun e -> SetCard.attr_value_exn e attr) set in
        let all_same = all_equal set_values in
        let all_different = List.contains_dup set_values in
        all_same || all_different


    let rec make_deck_helper history remain = match remain with
    | []           -> [history]
    | attr::remain -> List.fold
        ~init:[]
        ~f:(fun acc v ->
            let new_choice = (attr, v) in
            let child_list = make_deck_helper (new_choice::history) remain in
            acc @ child_list)
        (SetAttribute.list_of_values attr)

    let make_deck attrs = List.map ~f:SetCard.of_tuple_list (make_deck_helper [] attrs)

    let draw_cards deck = ([], deck)
    let next_board old_board new_cards = [| |]

    (* public stuff *)

    (* check if all attributes have same number of values *)
    (* TODO check if all attributes given different ids *)
    let create attributes =
        let lens = List.map ~f:SetAttribute.number_of_values attributes in
        if all_equal lens
        then
            let deck = make_deck attributes in
            (* let (deck, board) = draw_cards deck in *)
            {board = [||] ; deck ; attributes ; m = List.hd_exn lens}
        else
            failwith "All attributes must have same number of values"

    let validate_set t set =
        let validate_each_attr = List.map ~f:(check_attr set) t.attributes in
        (List.length set = t.m) && (List.fold ~init:true ~f:(fun acc e -> acc && e) validate_each_attr)
end
