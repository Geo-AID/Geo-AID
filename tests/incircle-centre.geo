# The centre of the incircle of a triangle is an intersection of its angles' bisectors.
let A, B, C = Point();

# I will be the centre as an intersection of bisectors.
let I = intersection(bisector(ABC), bisector(BAC));