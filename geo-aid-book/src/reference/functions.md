# Functions

## `angle`

* `angle(ABC: 3-P)`
* `angle(A: Point, B: Point, C: Point)`

**Return type**: [Scalar (angle)](./types/primitives.md#scalar)

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

**Return type**: [Scalar (angle)](./types/primitives.md#scalar)

**Returns**: measurement of the angle between `k` and `l`. Which angle, depends on the order of the lines. For predictable outcome, the point versions are strongly recommended.

## `bisector`

* `bisector(AB: 2-P)`
* `bisector(A: Point, B: Point)`

**Return type**: [Line](./types/primitives.md#Line)

**Returns**: a bisector of the segment `AB` - a perpendicular line passing through its center.

* `bisector(ABC: 3-P)`
* `bisector(A: Point, B: Point, C: Point)`

**Return type**: [Line](./types/primitives.md#Line)

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

## `Circle`

* `Circle(center: Point, radius: Scalar (distance))`
* `circle(radius: Scalar (distance), center: Point)`

**Return type**: [Circle](./types/primitives.md#Circle)

**Returns**: a circle with the given `center` and `radius`.

* `Circle()`

**Return type**: [Circle](./types/primitives.md#Circle)

**Returns**: a circle with an adjusted (free point) `center` and an adjusted (free scalar) `radius`.

## `degrees`

* `degrees(value: Scalar (no unit))`

**Return type**: [Scalar (angle)](./types/primitives.md#Scalar)

**Returns**: an angle with the given measurement in degrees. Related: [radians](#radians)

## `dst`

* `dst(AB: 2-P)`
* `dst(A: Point, B: Point)`

**Return type**: [Scalar (distance)](./types/primitives.md#Scalar)

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

**Return type**: [Scalar (distance)](./types/primitives.md#Scalar)

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

* `dst(value: Scalar (no unit / distance))`

**Return type**: [Scalar (angle)](./types/primitives.md#Scalar)

**Returns**: the value with a distance unit.

## `intersection`

* `intersection(k: Line, l: Line)`

**Return type**: [Point](./types/primitives.md#point)

**Returns**: intersection of lines `k` and `l`.

**Displays**: By default doesn't display the point dot. Modifiable with properties.

**Note**: `display_dot` property is not currently supported.

## `mid`

* `mid(col: 0-P)`

**Return Type**: [Point](.types/primitives.md#Point)

**Returns**: The middle point of all points in the collection.

**Note**: The following functions allow any positive numbers of arguments.

* `mid(v_1: Scalar (any unit u), v_2 Scalar (the same unit u), ..., v_n: Scalar (the same unit u))`

**Return type**: [Scalar (the same unit u)](./types/primitives.md#Scalar)

**Returns**: The average value of `v_1`, `v_2`, ... `v_n`.

* `mid(P_1: Point, P_2: Point, ..., P_n: Point)`

**Return type**: [Point](./types/primitives.md#Point)

**Returns**: The middle point of `P_1`, `P_2`, ... `P_n`. Special cases: when `n=2`, the middle of a segment; When `n=3`, the centroid of a triangle.

## `parallel_through`

* `parallel_through(P: Point, k: Line)`
* `parallel_through(k: Line, P: Point)`

**Return type**: [Line](./types/primitives.md#Line)

**Returns**: a line parallel to `k`, passing through `P`.

## `perpendicular_through`

* `perpendicular_through(P: Point, k: Line)`
* `perpendicular_through(k: Line, P: Point)`

**Return type**: [Line](./types/primitives.md#Line)

**Returns**: a line perpendicular to `k`, passing through `P`.

## `Point`

* `Point()`

**Return type**: [Point](./types/primitives.md#Circle)

**Returns**: an adjusted (free) point.

## `radians`

* `radians(value: Scalar (no unit))`

**Return type**: [Scalar (angle)](./types/primitives.md#Scalar)

**Returns**: an angle with the given measurement in radians. Related: [degrees](#degrees)

## `Segment`

* `Segment(AB: 2-P)`
* `Segment(A: Point, B: Point)`

**Return type**: [Segment](./types/bundle-types.md#segment)

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
