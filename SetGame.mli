open Core.Std

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
type t = {
    board: SetCard.t array;
    deck: SetCard.t list;
    attributes: SetAttribute.t list;
    m : int (* the number of values per attribute *)
}

val create : SetAttribute.t list -> t

val board : t -> SetCard.t array

val validate_set : t -> SetCard.t list -> bool
