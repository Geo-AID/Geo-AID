let A, B, C = Point();

let B' = homothety(A, -0.5).t(B);
let A' = rotate(B, deg(90)).t(A);
let C' = reflect(AB).t(C);
let D = reflect(AB).compose(rotate(B, deg(90))).t(A);
