# This is a particularly complicated file with a lot of binds based on the initial 3 points.
# In theory, it should perform poorly without optimization.
let A, B, C = Point();

@optimizations.identical_expressions: off;

let l = bisector(ABC);
let D = intersection(l, AC);
let f = perpendicular_through(l, D);
let g = parallel_through(f, B);
dst(D, intersection(f, bisector(DAC))) < dst(D, g);