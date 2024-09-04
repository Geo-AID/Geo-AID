# In v0.4.2, this file produced 4 errors that weren't previously caught

let A, B, C, P = Point();

# These two lines produced an "unexpected token" in place of P, despite the syntax clearly being valid.
[display = false]
P !lies_on BC;

# This line produced:
# an error about the ambiguity of point collections in this spot. Single-point pcs should not be marked ambiguous, though.
# an error about pt_a and pt_b not being valid math label identifiers, even they don't have to be.
let pt_a, pt_b = A, B;

# This line produced an error about there not being fields on point collections.
A.y = B.y;

# As of v0.5.0, all these bugs have been fixed and the file should compile correctly.