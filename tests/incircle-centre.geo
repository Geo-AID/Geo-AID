# The centre of the incircle of a triangle is an intersection of its angles' bisectors.
let A, B, C = Point();

# O will be the centre as an intersection of bisectors.
let O = intersection(bisector(ABC), bisector(BAC));

dst(AB, C) > 0.1;