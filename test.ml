open Core.Std
#load "SetValue.cmo"
#load "SetCard.cmo"
#load "SetAttribute.cmo"
#load "SetGame.cmo"

(* define the real set game *)
let one = SetValue.create_unnamed 1;;
let two = SetValue.create_unnamed 2;;
let three = SetValue.create_unnamed 3;;

let number = SetAttribute.create 1 "number" [one; two; three];;

let diamond  = SetValue.create 1 "diamond";;
let oval     = SetValue.create 2 "oval";;
let squiggle = SetValue.create 3 "squiggle";;

let symbol = SetAttribute.create 2 "symbol" [diamond;oval;squiggle];;

let solid  = SetValue.create 1 "solid";;
let shaded = SetValue.create 2 "shaded";;
let opn    = SetValue.create 3 "open";;

let shading = SetAttribute.create 3 "shading" [solid;shaded;opn];;

let red    = SetValue.create 1 "red";;
let green  = SetValue.create 2 "green";;
let purple = SetValue.create 3 "purple";;

let color = SetAttribute.create 4 "color" [red; green; purple];;

let test = SetGame.create [number; symbol; shading; color];;

let find_set game =
    let rec aux i j k =
        let board = SetGame.board game in
        let out_of_bounds = (i+1)*(j+1)*(k+1) >= Array.length board in
        let any_same = i = j || j = k || i = k in
        if out_of_bounds || any_same
        then None
        else
            let test_set = [board.(i);board.(j);board.(k)] in
            if SetGame.validate_set game test_set
            then Some test_set
            else
                let left = aux (i+1) j k in
                let center = aux i (j+1) k in
                let right = aux i j (k+1) in
                if Option.is_none left then
                    if Option.is_none center then
                        if Option.is_none right then None else right
                    else center
                else left
    in aux 0 1 2

let () =
    List.iter ~f:(fun e -> Printf.printf "%s\n" (SetCard.to_string e)) (Option.value_exn (find_set test))
