# The centre of the incircle of a triangle is an intersection of its angles' bisectors.
let A | B | C | O = Point();

# O will be the centre
dst(O, AB) = dst(O, BC | AC);

dst(AB, C) > 0.1;