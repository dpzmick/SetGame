open Core.Std

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
    let all_different = not (List.contains_dup set_values) in
    all_same || all_different

(* TODO find a way to make at least tail recursive *)
let make_deck attrs =
    let rec make_deck_helper history remain = match remain with
    | []           -> [history]
    | attr::remain -> List.fold
        ~init:[]
        ~f:(fun acc v ->
            let new_choice = (attr, v) in
            let child_list = make_deck_helper (new_choice::history) remain in
            acc @ child_list)
        (SetAttribute.list_of_values attr)

    in List.map ~f:SetCard.of_tuple_list (make_deck_helper [] attrs)

(* grabs 12 cards and puts them on the boards *)
let init_board_from deck =
    let board = Array.create ~len:12 (SetCard.empty ()) in
    let rec aux count deck =
        match count with
        | 12 -> (board, deck)
        | _  -> (board.(count) <- (List.hd_exn deck); aux (count + 1) (List.tl_exn deck))
    in aux 0 deck

(* public stuff *)

(* check if all attributes have same number of values *)
(* TODO check if all attributes given different ids, or design a sane interface ;p *)
let create attributes =
    let lens = List.map ~f:SetAttribute.number_of_values attributes in
    if all_equal lens
    then
        let deck = make_deck attributes in
        let (board, deck) = init_board_from deck in
        {board ; deck ; attributes ; m = List.hd_exn lens}
    else
        failwith "All attributes must have same number of values"

let board {board;_} = board

let validate_set t set =
    let validate_each_attr = List.map ~f:(check_attr set) t.attributes in
    (List.length set = t.m) && (List.fold ~init:true ~f:(fun acc e -> acc && e) validate_each_attr)
