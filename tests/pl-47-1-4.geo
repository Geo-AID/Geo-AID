# A line tangent to the incircle of triangle ABC intersects sides AB and AC at D and E, respectively.
# Show that AD/DB + AE/EC = 1.

let A, B, C = Point();
let I = intersection(bisector(ABC), bisector(BCA));

let X = Point();
XI = dst(I, AB);
let D, E = intersection(perpendicular_through(X, XI), (AB, AC));

AD + BD = AB;
AE + CE = AC;