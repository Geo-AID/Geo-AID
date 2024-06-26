# Types

GeoScript has two kinds of types: primitives and bundles.

Primitives - the points, lines, scalars and circles, are the building blocks of every value in GeoScript. Bundles, on the other hand, consist of multiple primitives.

The compiler is capable of performing some implicit conversions:
* Unknown-unit scalars (usually literals) can be converted into a scalar with a distance unit.
* A point collection consisting of two points can be converted into a line or the distance between the two points, depending on the context.
* A point collection of length one is always automatically converted into a point.
* When performing multiplication/division over a scalar with a unit and a scalar with an unknown unit, the latter is automatically converted into a unit-less scalar (standard scalar in mathematics).
* Any variable defined with an unknown-unit scalar is assumed to be unit-less.