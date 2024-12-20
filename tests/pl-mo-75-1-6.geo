let A, B, C = Point();
AC = BC;
BC > AB;

let O = intersection(
	bisector(AB) [display=false],
	bisector(AC) [display=false]
);

let M = mid(A, C);

let amo_center = intersection(bisector(AM), bisector(MO)) [display=false];
let amo = Circle(amo_center, dst(amo_center, A) [display = false]);

let X = Point() on Segment(BM);

M != X on amo;

?Circle(O, OA [display=false]);
