open Core.Std
(*
#load "SetValue.cmo"
#load "SetCard.cmo"
#load "SetAttribute.cmo"
#load "SetGame.cmo"
*)

let print_board b =
    Array.iter ~f:(fun c -> Printf.printf "%s\n" (SetCard.to_string c)) b

let print_set s =
    List.iter ~f:(fun c -> Printf.printf "%s\n" (SetCard.to_string c)) s

let combs len limit base =
    let rec aux idxes last = if List.length idxes = len
    then (base idxes)
    else
        let rec loop i =
            if i = limit
            then None
            else
                let child = aux (idxes @ [i]) (i+1) in
                if Option.is_some child
                then child
                else loop (i+1)
        in (loop last)
    in aux [] 0

let find_set game =
    let board = SetGame.board game in
    let base lst =
        let set = List.map ~f:(fun i -> board.(i)) lst in
        (*
        let () = Printf.printf "trying set:\n" in
        let () = print_set set in
        *)
        if SetGame.validate_set game set then Some set else None
    in combs (SetGame.cards_needed game) (Array.length board) base

let rec play_game game =
    match find_set game with
    | None ->
            let () = Printf.printf "\nBoard (len: %d)\n" (Array.length (SetGame.board game)) in
            let () = print_board (SetGame.board game) in
            let () = Printf.printf "No sets found\n" in
            if SetGame.cards_remain game > 0
            then
                let () = Printf.printf "drawing more cards\n" in
                play_game (SetGame.deal_more game)
            else
                SetGame.score game

    | Some set ->
            let () = Printf.printf "\nBoard (len: %d)\n" (Array.length (SetGame.board game)) in
            let () = print_board (SetGame.board game) in
            let () = Printf.printf "\n%d cards remain\n\nFound set:\n" (SetGame.cards_remain game) in
            let () = print_set set in
            play_game (SetGame.remove_set game set)


(*
let () =
    let number = SetAttribute.create 1 "number" [SetValue.create_unnamed 1; SetValue.create_unnamed 2] in
    let shape = SetAttribute.create 2 "shape" [SetValue.create_unnamed 1; SetValue.create_unnamed 2] in
    let shading = SetAttribute.create 3 "shading" [SetValue.create_unnamed 1; SetValue.create_unnamed 2] in
    let game = SetGame.create [number;shape;shading] in
    Printf.printf "Final Score: %d\n" (play_game game)
*)

let () =
    (* define the real set game TODO there must be a better way*)
    let one = SetValue.create_unnamed 1 in
    let two = SetValue.create_unnamed 2 in
    let three = SetValue.create_unnamed 3 in

    let number = SetAttribute.create 1 "number" [one; two; three] in

    let diamond  = SetValue.create 1 "diamond" in
    let oval     = SetValue.create 2 "oval" in
    let squiggle = SetValue.create 3 "squiggle" in

    let symbol = SetAttribute.create 2 "symbol" [diamond;oval;squiggle] in

    let solid  = SetValue.create 1 "solid" in
    let shaded = SetValue.create 2 "shaded" in
    let opn    = SetValue.create 3 "open" in

    let shading = SetAttribute.create 3 "shading" [solid;shaded;opn] in
    let red    = SetValue.create 1 "red" in
    let green  = SetValue.create 2 "green" in
    let purple = SetValue.create 3 "purple" in

    let color = SetAttribute.create 4 "color" [red; green; purple] in

    let test = SetGame.create [number; symbol; shading; color] in

    (* play the game *)
    Printf.printf "%d\n" (play_game test)
