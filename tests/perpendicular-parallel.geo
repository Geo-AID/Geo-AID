let A | B | C = Point();

let l = perpendicular_through(AB, C);
let D = intersection(AB, l);

let k = parallel_through(AB, C);
let E = Point();
AB = CE;