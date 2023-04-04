# Inner bisectors of angles at A, B, C of the triangle ABC intersect opposite sides at D, E, F respectively
# and the circumcircle of ABC at K, L, M respectively.
# Show that AD/DK + BE/EL + CF/FM >= 9.

let A, B, C = Point();
let x, y, z = bisector((ABC, BCA, CAB));

let D, E, F = intersection(
    (x, y, z), (AC, AB, CB)
);

let K, L, M = ...;