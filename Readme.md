A (slightly) generalized set game.

Each game has n attributes (Color, Number, Shading, etc) and m possible values for each attribute.

A valid set consists of cards for which the following is true:
* For every attribute a, either:
     c1[a] = c2[a] = ... = cm[a] (all are equal)
                     OR
     c1[a] != c2[a] != ... != cm[a] (all are different)

* The number of cards in the set must be m

Though I would try to be clever and write good code, turns out I am not so
clever!
