# Types

## Number

A number is a simple complex value with a unit - a unit is a product of integer powers of simple units. The simple units are:
- Distance
- Angle

Unknown-unit numbers (usually literals) can be converted into a number with a distance unit.
Any number, whose unit cannot be determined, is assumed to be unit-less. Numbers in this reference are denoted as `Number(<unit>)`.
A point collection consisting of two points can be converted into a line or the distance between the two points,
depending on the context.
Any variable defined with an unknown-unit number is assumed to be unit-less.
When performing multiplication/division over a number with a unit and a number with an unknown unit, the latter is
automatically converted into a unit-less number.

Note: A literal will never be coerced to an angle, since that would introduce uncertainty whether it should be
treated as given in radians or degrees. Instead, look for their respective functions.

*Methods*

* `acos()` for no unit

**Return type**: [Number (angle)](#number)

**Returns**: Arccosine of this number.

* `acot()` (alias `actg`) for no unit

**Return type**: [Number (angle)](#number)

**Returns**: Arccotangent of this number.

* `acsc()` for no unit

**Return type**: [Number (angle)](#number)

**Returns**: Arccosecant of this number.

* `asec()` for no unit

**Return type**: [Number (angle)](#number)

**Returns**: Arcsecant of this number.

* `asin()` for no unit

**Return type**: [Number (angle)](#number)

**Returns**: Arcsine of this number.

* `atan()` (alias `atg`) for no unit

**Return type**: [Number (angle)](#number)

**Returns**: Arctangent of this number.

* `conjugate()` for any unit

**Return type**: [Number (same unit)](#number)

**Returns**: the conjugate of this number.

* `cos()` for angle

**Return type**: [Number (no unit)](#number)

**Returns**: Cosine of this angle.

* `cot()` (alias `ctg`) for angle

**Return type**: [Number (no unit)](#number)

**Returns**: Cotangent of this angle.

* `csc()` for angle

**Return type**: [Number (no unit)](#number)

**Returns**: Cossecant of this angle.

* `degrees()` (alias `deg`) if the number is unitless.

**Return type**: [Number (angle)](#number)

**Returns**: Angle value with measurement equal to this number in degrees.

* `degrees()` (alias `deg`) if the number is an angle.

**Return type**: [Number (no unit)](#number)

**Returns**: The measurement of this angle in degrees.

* `imaginary()` (alias `im`) for any unit

**Return type**: [Number (same unit)](#number)

**Returns**: The imaginary part of this number.

* `radians()` (alias `rad`) if the number is unitless.

**Return type**: [Number (angle)](#number)

**Returns**: Angle value with measurement equal to this number in radians.

* `radians()` (alias `rad`) if the number is an angle.

**Return type**: [Number (no unit)](#number)

**Returns**: The measurement of this angle in radians.

* `real()` (alias `re`) for any unit

**Return type**: [Number (same unit)](#number)

**Returns**: The real part of this number.

* `sec()` for angle

**Return type**: [Number (no unit)](#number)

**Returns**: Secant of this angle.

* `sin()` for angle

**Return type**: [Number (no unit)](#number)

**Returns**: Sine of this angle.

* `tan()` (alias `tg`) for angle

**Return type**: [Number (no unit)](#number)

**Returns**: Tangent of this angle.

* `to_point()` for distances

**Return type**: [Point](#point)

**Returns**: this number as a point.

## Point

A point is defined as a point on a Euclidean plane. Denoted as `Point`.

Points have two methods: `x` and `y`, returning the respective coordinate values.

A point collection of length one is always automatically converted into a point.

*Methods*

* `to_complex()`

**Return type**: [Number (distance)](#number)

**Returns**: This point as a number.

## Circle

A circle is given a center and a radius. It is a set of points with the distance to its center equal to its radius.
Denoted as `Circle`.

*Methods*

* `center()`

**Return type**: [Point](#point)

**Returns**: the circle's center.

* `radius()`

**Return type**: [Number (distance)](#number)

**Returns**: the circle's radius.

## Line

A point collection consisting of two points can be converted into a line or the distance between the two points,
depending on the context.
A line is a line in Euclidean sense. Denoted as `Line`.

## Point collections

Point collections are simply ordered collections of points. It is never a separate entity, only an abstraction over a set of points. Denoted as `<length>-P`. If `<length>` is given as `0`, it means a collection of any length. Most functions that accept points as arguments, also accept point collections.

*Methods*

* `area()` if the collection has length of at least 3.

**Return type**: [Number (distance^2)](#number)

**Returns**: The area of the polygon.

* `signedarea()` if the collection has length of at least 3.

**Return type**: [Number (distance^2)](#number)

**Returns**: The signed area of the polygon, where the sign depends on the clockwiseness of the points given.

* `circumcircle()` if the collection has length of 3.

**Return type**: [Circle](#circle)

**Returns**: The circle circumscribed on the three points.

* `dst()` (alias `len`) if the collection has length of 2.

**Return type**: [Number (distance)](#number)

**Returns**: the distance between the two points.

* `incircle()` if the collection has length of 3.

**Return type**: [Circle](#circle)

**Returns**: The circle inscribed in the three points.

* `mid()` for any length.

**Return type**: [Point](#point)

**Returns**: The arithmetic average of the points included in the collection (coordinates-wise).

* `orthocenter()` (alias `orthocentre`) if the collection has length of 3.

**Return type**: [Point](#point)

**Returns**: The intersection of the triangle's altitudes.

* `vector()` (alias `vec`) if the collection has length of 2.

**Return type**: [Number (distance)](#number)

**Returns**: The vector from the first point to the second point.

### Segment

Any two points can be connected with a `Segment`.

*Methods*

* `len()`

**Return type**: [Number (distance)](#number)

**Returns**: the distance `AB`.

**Displays**: exactly what `dst` displays, except that the `draw_segment` property is `false` by default.

### TransformType

Represents a plane transformation. Currently only similarities are supported.

*Methods*

* `compose(other: TransformType)`

**Return type**: [TransformType](#transformtype)

**Returns**: The composition of this transform and the other transform (the other is performed first).

* `transform(object: Any)` (alias `t`)

**Return type**: Depends on the transformation.

**Returns**: The transformed object. If the transformation doesn't support a type, an error will be raised.
