let A, B, C, D = Point();
AB + CD = BC + AD;

let I = intersection(
	bisector(ABC) [display=false],
	bisector(BCD) [display = false]
) !lies_on AC [display=false];

let omega = Circle(I, dst(I, AB));
let E = intersection(AC, BD);
let k = perpendicular_through(E, BD);
let P, Q = intersection(k, (AI, CI));
