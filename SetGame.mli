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
type t

val create : SetAttribute.t list -> t

(* game play *)
val validate_set : t -> SetCard.t list -> bool
val remove_set : t -> SetCard.t list -> t
val deal_more: t -> t (* decreases score by number of cards drawn *)
val deal_more_custom: t -> int -> t

(* some accessor sort of funtions *)
val board : t -> SetCard.t array
val cards_remain : t -> int
val score : t -> int
val cards_needed: t -> int
