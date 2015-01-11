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

let test = SetGame.make_deck [number; symbol; shading; color];;
