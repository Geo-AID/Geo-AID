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

* `conjugate()` for any unit

**Return type**: [Number (same unit)](#number)

**Returns**: the conjugate of this number.

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

### Segment

Any two points can be connected with a `Segment`.

*Methods*

* `len()`

**Return type**: [Number (distance)](#number)

**Returns**: the distance `AB`.

**Displays**: exactly what `dst` displays, except that the `draw_segment` property is `false` by default.
