# Point P lies inside a parallelogram ABCD with angle ABP = angle ADP.
# Show that angle PAB = angle PCB

let A, B, C, P = Point();

let D = intersection(
    parallel_through(C, AB),
    parallel_through(A, BC)
);

# Currently there is no way to tell Geo-AID to put a point inside a polygon.
angle(ABP) = angle(ADP);