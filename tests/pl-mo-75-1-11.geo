let A, B, C = Point();

let J = intersection(
	bisector(BAC),
	perpendicular_through(B, bisector(ABC) [display=false])
);

let b = perpendicular_through(B, bisector(ABC));

let P, Q = Point() lies_on BC;
PB + BC = PC;
BC + CQ = BQ;

PB = AB;
QC = AC;

angle(BAC) > degrees(60);

?Circle(J, dst(J, AB));
