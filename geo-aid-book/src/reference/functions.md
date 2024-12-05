# Functions

Here are listed all of GeoScript's functions. Note that, the names are case-insensitive and ignore underscores. This means that functions `perpendicular_through`, `perpendicularthrough`, `PERPendicularthrougH` and `P_erpendic_ular___Through_` are the same function.
Overloads are listed in the order they are checked.

## `acos`

* `acos(v: Number (no unit))`

**Return type**: [Number (angle)](.types.md#number)

**Returns**: Arccosine of this value.

## `acot` (alias `actg`)

* `acot(v: Number (no unit))`

**Return type**: [Number (angle)](.types.md#number)

**Returns**: Arccotangent of this value.

## `acsc`

* `acsc(v: Number (no unit))`

**Return type**: [Number (angle)](.types.md#number)

**Returns**: Arccosecant of this value.

## `angle`

* `angle(ABC: 3-P)`
* `angle(A: Point, B: Point, C: Point)`

**Return type**: [Number (angle)](./types.md#number)

**Returns**: measurement of the angle `ABC`

**Displays**: the angle's arms.

The function accepts additional properties in the form of:

```rust
struct Angle {
    display_arms: bool, // Default: true,
    arms_type: LineType, // Default: SEGMENT
}
```

`display_arms` decides whether the arms should be displayed and `arms_type` decides whether they should be segments, rays or lines. The assumed order for rays is `B -> A` and `B -> C`;

* `angle(k: Line, l: Line)`

**Return type**: [Number (angle)](./types.md#number)

**Returns**: measurement of the angle between `k` and `l`. Which angle, depends on the order of the lines. For predictable outcome, the point versions are strongly recommended.

## `area`

* `area(A (Point), B (Point), C (Point), ...)`
* `area(ABC... (Point collection))`

Works for 3 or more points.

**Return type**: [Number (distance^2)](./types.md#number)

**Returns**: the area of the given polygon.

## `asec`

* `asec(v: Number (no unit))`

**Return type**: [Number (angle)](.types.md#number)

**Returns**: Arcsecant of this value.

## `asin`

* `asin(v: Number (no unit))`

**Return type**: [Number (angle)](.types.md#number)

**Returns**: Arcsine of this value.

## `atan` (alias `atg`)

* `atan(v: Number (no unit))`

**Return type**: [Number (angle)](.types.md#number)

**Returns**: Arctangent of this value.

## `bisector`

* `bisector(AB: 2-P)`
* `bisector(A: Point, B: Point)`

**Return type**: [Line](./types.md#Line)

**Returns**: a bisector of the segment `AB` - a perpendicular line passing through its center.

* `bisector(ABC: 3-P)`
* `bisector(A: Point, B: Point, C: Point)`

**Return type**: [Line](./types.md#Line)

**Returns**: a bisector of the angle `ABC` - a line between lines `AB` and `BC`, where each point is in the same distance from both of these lines.

**Displays**: the angle's arms.

The function accepts additional properties in the form of:

```rust
struct Bisector {
    display_arms: bool, // Default: true,
    arms_type: LineType, // Default: SEGMENT
}
```

`display_arms` decides whether the arms should be displayed and `arms_type` decides whether they should be segments, rays or lines. The assumed order for rays is `B -> A` and `B -> C`;

* `angle(k: Line, l: Line)`

## `center` (alias `centre`)

* `center(circle: Circle)`

**Return type: [Point](./types.md#Point)

## `circle`

* `circle(center: Point, radius: Number (distance))`
* `circle(radius: Number (distance), center: Point)`

**Return type**: [Circle](./types.md#Circle)

**Returns**: a circle with the given `center` and `radius`.

* `circle()`

**Return type**: [Circle](./types.md#Circle)

**Returns**: a circle with an adjusted (free point) `center` and an adjusted (free real) `radius`.

## `circumcircle`

* `circumcircle(a: Point, b: Point, c: Point)`
* `circumcircle(abc: 3-P)`

**Return type**: [Circle](./types.md#Circle)

**Returns**: the circle circumscribed on the three points given.

## `circumcenter`

* `circumcenter(a: Point, b: Point, c: Point)`
* `icircumcenter(abc: 3-P)`

**Return type**: [Point](./types.md#Point)

**Returns**: the cetner of the circle circumscribed on the three points given.

## `conjugate`

* `conjugate(v: Number (any unit))`

**Return type**: [Number (the same unit)](#./types.md#Number)

**Returns**: The conjugate of this number.

## `convex` (alias `convexpolygon`, `convexpoly`)

* `convex(n: Number (literal, no unit))`

Only works with a number literal.

**Return type**: [PC-n](./types.md#pointcollections)

**Returns**: A convex polygon with `n` sides.

## `cos`

* `cos(v: Number (angle))`

**Return type**: [Number (no unit)](.types.md#number)

**Returns**: Cosine of this angle.

## `cot` (alias `ctg`)

* `cot(v: Number (angle))`

**Return type**: [Number (no unit)](.types.md#number)

**Returns**: Cotangent of this angle.

## `csc`

* `csc(v: Number (angle))`

**Return type**: [Number (no unit)](.types.md#number)

**Returns**: Cosecant of this angle.

## `degrees` (alias `deg`)

* `degrees(value: Number (no unit))`

**Return type**: [Number (angle)](./types.md#number)

**Returns**: an angle with the given measurement in degrees. Related: [radians](#radians)

* `degrees(value: Number (angle))`

**Return type**: [Number (no unit)](./types.md#number)

**Returns**: the angle value in degrees. Related: [radians](#radians)

## `dst` (alias `len`)

* `dst(AB: 2-P)`
* `dst(A: Point, B: Point)`

**Return type**: [Number (distance)](./types.md#number)

**Returns**: the distance between points `A` and `B`.

**Displays**: the segment `AB`.

The function accepts additional properties in the form of:

```rust
struct Dst {
    display_segment: bool, // Default: true,
    style: Style, // Default: SOLID
}
```

`display_segment` decides whether the segment should be displayed and `style` decides how it should be displayed.

* `dst(P: Point, k: Line)`
* `dst(k: Line, P: Point)`

**Return type**: [Number (distance)](./types.md#number)

**Returns**: the distance between point `P` and line `k`.

**Displays**: the segment between `P` and its perpendicular projection onto `k`.

The function accepts additional properties in the form of:

```rust
struct Dst {
    display_segment: bool, // Default: true,
    style: Style, // Default: DASHED
}
```

`display_segment` decides whether the segment should be displayed and `style` decides how it should be displayed.

* `dst(value: Number (no unit / distance))`

**Return type**: [Number (angle)](./types.md#number)

**Returns**: the value with a distance unit.

## `equilateral` (alias `equilateral_triangle`)

* `equilateral()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: an equilateral triangle.

## `excircle`

* `excircle(a: Point, b: Point, c: Point)`
* `excircle(abc: 3-P)`

**Return type**: [Circle](./types.md#Circle)

**Returns**: the circle excribed to the points given, with the center on the bisector of angle `ABC`.

## `homothety`

* `homothety(origin: Point, scale: Number (no unit))`
* `homothety(scale: Number (no unit), origin: Point)`

**Return type**: [TransformType](./types.md#transformtype)

**Returns**: a homothety with an origin and scale.

## `imaginary` (alias `im`)

* `imaginary(v: Number (any unit))`

**Return type**: [Number (the same unit)](./types.md#number)

**Returns**: The imaginary part of this number.

## `incircle`

* `incircle(a: Point, b: Point, c: Point)`
* `incircle(abc: 3-P)`

**Return type**: [Circle](./types.md#Circle)

**Returns**: the circle inscribed in the three points given.

## `incenter`

* `incenter(a: Point, b: Point, c: Point)`
* `incenter(abc: 3-P)`

**Return type**: [Point](./types.md#Point)

**Returns**: the cetner of the circle inscribed in the three points given.

## `intersection`

All overloads by default don't display the point dot. This can be changed with properties.

* `intersection(k: Line, l: Line)`

**Return type**: [Point](./types.md#point)

**Returns**: intersection of lines `k` and `l`.

* `intersection(k: Line, circle: Circle)`
* `intersection(circle: Circle, k: Line)`

**Return type**: [Point](./types.md#point)

**Returns**: intersection of line `k` and circle `circle`.

* `intersection(o1: Circle, o2: Circle)`

**Return type**: [Point](./types.md#point)

**Returns**: intersection of circles `o1` and `o2`.

**Note**: `display_dot` property is not currently supported.

## `isosceles` (alias `isosceles_triangle`)

* `isosceles()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: an isosceles triangle with `AC = BC`.

## `line`

* `line(col: 2-PC)`
* `line(P: Point, Q: Point)`

**Return type**: [Line](./types.md#Line)

**Returns**: a line through two given points.

**Displays**: The created line.

## `main_equilateral` (alias `main_equilateral_triangle`)

* `main_equilateral()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: an equilateral triangle with `C.y > A.y = B.y` and `A.x < B.x`.

## `main_isosceles` (alias `main_isosceles_triangle`)

* `main_isosceles()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: an isosceles triangle with `AC = BC`, `C.y > A.y = B.y` and `A.x < B.x`.

## `main_right` (alias `main_right_triangle`)

* `main_right()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: a right triangle with `angle(ACB) = deg(90)`, `B.y > A.y = C.y` and `C.x < A.x`.

## `main_triangle` (alias `main_triangle`)

* `main_triangle()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: a triangle with `C.y > A.y = B.y` and `A.x < B.x`.

## `mid`

* `mid(col: 0-P)`

**Return Type**: [Point](.types.md#Point)

**Returns**: The middle point of all points in the collection.

**Note**: The following functions allow any positive numbers of arguments.

* `mid(v_1: Number (any unit u), v_2 Number (the same unit u), ..., v_n: Number (the same unit u))`

**Return type**: [Number (the same unit u)](./types.md#number)

**Returns**: The average value of `v_1`, `v_2`, ... `v_n`.

* `mid(P_1: Point, P_2: Point, ..., P_n: Point)`

**Return type**: [Point](./types.md#Point)

**Returns**: The middle point of `P_1`, `P_2`, ... `P_n`. Special cases: when `n=2`, the middle of a segment; When `n=3`, the centroid of a triangle.

## `orthocenter` (alias `orthocentre`)

* `orthocenter(A: Point, B: Point, C: Point)`
* `orthocenter(ABC: 3-P)`

**Return type**: [Point](./types.md#Point)

**Returns**: The intersection of the triangles altitudes.

## `parallel_through` (alias `parallel`)

* `parallel_through(P: Point, k: Line)`
* `parallel_through(k: Line, P: Point)`

**Return type**: [Line](./types.md#Line)

**Returns**: a line parallel to `k`, passing through `P`.

## `perpendicular_through` (alias `perpendicular`)

* `perpendicular_through(P: Point, k: Line)`
* `perpendicular_through(k: Line, P: Point)`

**Return type**: [Line](./types.md#Line)

**Returns**: a line perpendicular to `k`, passing through `P`.

## `point`

* `point()`

**Return type**: [Point](./types.md#Circle)

**Returns**: an adjusted (free) point.

## `polygon` (alias `poly`)

* `polygon(n: Number (literal, no unit))`

Only works with a number literal.

**Return type**: [PC-n](./types.md#pointcollections)

**Returns**: A polygon with `n` sides. Possibly concave, possibly self-intersecting.

## `radians` (alias `rad`)

* `radians(value: Number (no unit))`

**Return type**: [Number (angle)](./types.md#number)

**Returns**: an angle with the given measurement in radians. Related: [degrees](#degrees)

* `radians(value: Number (angle))`

**Return type**: [Number (no unit)](./types.md#number)

**Returns**: the value of the angle in radians. Related: [degrees](#degrees)

## `radius`

* `radius(circle: Circle)`

**Return type**: [Number (distance)](./types.md#number)

**Returns**: the radius of the given circle.

## `real` (alias `re`)

* `real(v: Number (any unit))`

**Return type**: [Number (the same unit)](./types.md#number)

**Returns**: The real part of this number.

* `real()`

**Return type**: [Number (no unit)](./types.md#number)

**Returns**: A free, adjustable real number.

## `reflect` (alias `reflection`)

* `reflect(line: Line)`

**Return type**: [TransformType](./types.md#transformtype)

**Returns**: A reflection about a line.

## `rotate` (alias `rotation`)

* `rotate(origin: Point, angle: Number (angle), scale: Number (unitless))`
* `rotate(origin: Point, scale: Number (unitless), angle: Number (angle))`
* `rotate(angle: Number (angle), origin: Point, scale: Number (unitless))`
* `rotate(angle: Number (angle), scale: Number (unitless), origin: Point)`
* `rotate(scale: Number (unitless), origin: Point, angle: Number (angle))`
* `rotate(scale: Number (unitless), angle: Number (angle), origin: Point)`
* `rotate(angle: Number (angle), origin: Point)`
* `rotate(origin: Point, angle: Number (angle))`

**Return type**: [TransformType](./types.md#transformtype)

**Returns**: A rotation around an origin by an angle (possibly negative), along with an optional homothety at the same point.

## `right` (alias `right_triangle`)

* `right()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: a right triangle with `angle(ACB) = deg(90)`.

## `sec`

* `sec(v: Number (angle))`

**Return type**: [Number (no unit)](./types.md#number)

**Returns**: Secant of this angle.

## `segment`

* `segment(AB: 2-P)`
* `segment(A: Point, B: Point)`

**Return type**: [Segment](./types.md#segment)

**Returns**: the segment `AB`.

**Displays**: the segment `AB`.

The function accepts additional properties in the form of:

```rust
struct Segment {
    display_segment: bool, // Default: true,
    style: Style, // Default: SOLID
}
```

`display_segment` decides whether the segment should be displayed and `style` decides how it should be displayed.

## `signedarea`

* `signedarea(A (Point), B (Point), C (Point), ...)`
* `signedarea(ABC... (Point collection))`

Works for 3 or more points.

**Return type**: [Number (distance^2)](./types.md#number)

**Returns**: the signed area of the given polygon.

## `sin`

* `sin(v: Number (angle))`

**Return type**: [Number (no unit)](.types.md#number)

**Returns**: Sine of this angle.

## `tan` (alias `tg`)

* `tan(v: Number (angle))`

**Return type**: [Number (no unit)](.types.md#number)

**Returns**: Tangent of this angle.

## `to_complex`

* `to_complex(A: Point)`

**Return type**: [Number (distance)](types.md#Number)

**Returns**: the point as a complex number.

## `to_point`

* `to_point(v: Number (distance))`

**Return type**: [Point](types.md#Number)

**Returns**: the complex number as a point.

## `transform`

* `transform(t: TransformType, object: Any)`

**Return type**: Depends on the transformationi and input.

**Returns**: The object transformed using the transformation. Is type isn't supported, a compile error will be raised.

## `translate` (alias `translation`)

* `translate(vector: Distance)`

**Return type**: [TransformType](./types.md#transformtype)

**Returns**: A translation by a vector.

## `triagngle` (alias `triangle`)

* `triangle()`

**Return type**: [Point collection (3)](./types.md#point-collections)

**Returns**: a triangle.

## `x`

* `x(P: Point)`

**Return type**: [Number (distance)](types.md#number)

**Returns**: The `x` coordinate of the point.

## `y`

* `y(P: Point)`

**Return type**: [Number (distance)](types.md#number)

**Returns**: The `y` coordinate of the point.
