open Core.Std

type t = {
    board: SetCard.t array;
    deck: SetCard.t list;
    attributes: SetAttribute.t list;
    m : int; (* the number of values per attribute *)
    score : int;
    board_len : int;
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

    in
    let unshuffled = List.map ~f:SetCard.of_tuple_list (make_deck_helper [] attrs) in
    List.permute unshuffled ~random_state:(Random.State.make_self_init ())
    (* unshuffled *)

(* grabs cards and puts them on the boards *)
let init_board_from deck size =
    let board = Array.create ~len:size (SetCard.empty ()) in
    let rec aux count deck =
        if count = size
        then (board, deck)
        else (board.(count) <- (List.hd_exn deck); aux (count + 1) (List.tl_exn deck))
    in aux 0 deck

(* public stuff *)
let draw_cards how_many deck =
    let rec aux acc deck i = match deck with
    | [] -> (acc, [])
    | top::rest -> if i = how_many then (acc, deck) else aux (top::acc) rest (i+1)
    in aux [] deck 0

let remove_board_entries board entries =
    let not_in_entries c = Option.is_none (List.find ~f:(fun e -> e = c) entries) in
    Array.filter ~f:not_in_entries board

let replace_board_entries board replace_list =
    let replacement_for board_element =
        List.find ~f:(fun (be, _) -> be = board_element) replace_list
    in
    let replace_element e =
        match replacement_for e with
        | Some ne -> snd ne
        | None    -> e
    in Array.replace_all board ~f:replace_element

(* TODO functional record updating??? *)
let deal_more {board; deck; attributes; m; score; board_len} =
    let num_to_draw = m in
    let (new_cards, deck) = draw_cards m deck in
    let board = Array.append board (List.to_array new_cards) in
    {board; deck; attributes; m; score = score - num_to_draw; board_len}

let deal_more_custom {board; deck; attributes; m; score; board_len} num_to_draw =
    let (new_cards, deck) = draw_cards m deck in
    let board = Array.append board (List.to_array new_cards) in
    {board; deck; attributes; m; score = score - num_to_draw; board_len}

(* check if all attributes have same number of values *)
(* TODO check if all attributes given different ids, or design a sane interface ;p *)
let create attributes =
    let lens = List.map ~f:SetAttribute.number_of_values attributes in
    if all_equal lens
    then
        let deck = make_deck attributes in
        let board_len = (List.length attributes) * (List.hd_exn lens) in
        let (board, deck) = init_board_from deck board_len in
        {board ; deck ; attributes ; m = List.hd_exn lens; score = 0; board_len}
    else
        failwith "All attributes must have same number of values"

let validate_set t set =
    let validate_each_attr = List.map ~f:(check_attr set) t.attributes in
    (List.length set = t.m) && (List.fold ~init:true ~f:(fun acc e -> acc && e) validate_each_attr)

(* makes a new game, hence the array copy *)
let remove_set {board; deck; attributes; m; score; board_len} set =
    let (new_cards, deck) = draw_cards (List.length set) deck in
    if List.length new_cards = 0 || Array.length board > board_len
    then
            let board = remove_board_entries board set in
            {board; deck; attributes; m; score = score + 1; board_len}
    else
            let board = Array.copy board in
            let () = replace_board_entries board (List.zip_exn set new_cards) in
            {board; deck; attributes; m; score = score + 1; board_len}

let board {board;_} = board
let cards_remain {deck;_} = List.length deck
let score {score;_} = score
let cards_needed {m;_} = m
