# A circle has center on the side AB of the cyclic quaderilateral ABCD. The other three sides are tangent to the circle.
# Prove that AD + BC = AB.

let A, B, C, D = Point();

let O = intersection(bisector(AB), bisector(BC));
OD = OA;

let X = intersection(bisector(BCD), bisector(CDA));
AX + BX = AB;