# Primitives

## Scalar

A scalar is a simple real value with a unit - a unit is a product of integer powers of simple units. The simple units are:
- Distance
- Angle

Any scalar, whose unit cannot be determined, is assumed to be unit-less. Scalars in this reference are denoted as `Scalar(<unit>)`.

Note: A literal will never be coerced to an angle, since that would introduce uncertainty whether it should be treated as given in radians or degrees. Instead, look for their respective functions.

## Point

A point is defined as a point on a Euclidean plane. Denoted as `Point`.

Points have two fields: `x` and `y`, denoting the respective coordinate values. Use them carefully, though, as there are no guarantees as to what values they might be.

## Circle

A circle is given a center and a radius. It is a set of points with the distance to its center equal to its radius. Denoted as `Circle`.

Circles have two fields: `center` and `radius`, both of which are self-explanatory.

## Line

A line is a line in Euclidean sense. Denoted as `Line`.