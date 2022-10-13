let lies_on = operator(A: Point, l: Line) {
    distance(A, l) < 0.01;
};

let lies_on = operator(C: Point, AB: Ray) {weight = 1.0} : {
    C lies on AB {weight/2};
    angle(BAC) = 0 deg {weight/2};
};

let lies_on = operator(C: Point, AB: Segment) : {
    AC + BC = AB;
};