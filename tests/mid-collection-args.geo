# `mid` with a single length-2 point collection
# argument should return the mean of the points.
# #156

let A, B = Point();

let C = mid(AB);

# now interpret mid(AB, AB+1) as scalar:
AC = mid(AB, AB+1);
