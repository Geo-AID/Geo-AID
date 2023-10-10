# Functions

## `angle`

* `angle(ABC: [3-P](./types/bundle-types.md#point-collections))`
* `angle(A: [Point](./types/primitives.md#point), B: [Point](./types/primitives.md#point), C: [Point](./types/primitives.md#point))`

**Return type**: [Scalar (angle)](./types/primitives.md#scalar)

**Returns**: measurement of the angle `ABC`

* `angle(k: [Line](./types/primitives.md#point), l: [Line](./types/primitives.md#line))`

**Return type**: [Scalar (angle)](./types/primitives.md#scalar)

**Returns**: measurement of the angle between `k` and `l`. Which angle, depends on the order of the lines. For predictable outcome, the point versions are strongly recommended.

## `bisector`

* `bisector(AB: [2-P](./types/bundle-types.md#point-collections))`
* `bisector(A: [Point](./types/primitives.md#point), B: [Point](./types/primitives.md#point))`

**Return type**: [Line](./types/primitives.md#Line)

**Returns**: a bisector of the segment `AB` - a perpendicular line passing through its center.

* `bisector(ABC: [3-P](./types/bundle-types.md#point-collections))`
* `bisector(A: [Point](./types/primitives.md#point), B: [Point](./types/primitives.md#point), C: [Point](./types/primitives.md#point))`

**Return type**: [Line](./types/primitives.md#Line)

**Returns**: a bisector of the angle `ABC` - a line between lines `AB` and `BC`, where each point is in the same distance from both of these lines.

## `Circle`

* `Circle(center: [Point](./types/primitives.md#point), radius: [Scalar (distance)](./types/primitives.md#point))`
* `circle(radius: [Scalar (distance)](./types/primitives.md#point), center: [Point](./types/primitives.md#point))`

**Return type**: [Circle](./types/primitives.md#Circle)

**Returns**: a circle with the given `center` and `radius`.

* `Circle()`

**Return type**: [Circle](./types/primitives.md#Circle)

**Returns**: a circle with an adjusted (free point) `center` and an adjusted (free scalar) `radius`.

## `degrees`

* `degrees(value: [Scalar (no unit)])`

**Return type**: [Scalar (angle)](./types/primitives.md#Scalar)

**Returns**: an angle with the given measurement in degreees. Related: [radians](#radians)

## `dst`

* `dst(AB: [2-P](./types/bundle-types.md#point-collections))`
* `dst(A: [Point](./types/primitives.md#point), B: [Point](./types/primitives.md#point))`

**Return type**: [Scalar (distance)](./types/primitives.md#Scalar)

**Returns**: the distance between points `A` and `B`.

* `dst(P: [Point](./types/primitives.md#point), k: [Line](./types/primitives.md#line))`
* `dst(k: [Line](./types/primitives.md#line), P: [Point](./types/primitives.md#point))`

**Return type**: [Scalar (distance)](./types/primitives.md#Scalar)

**Returns**: the distance between point `P` and line `k`.

* `dst(value: [Scalar (no unit / distance)])`

**Return type**: [Scalar (angle)](./types/primitives.md#Scalar)

**Returns**: the value with a distance unit.

## `intersection`

* `intersection(k: [Line](./types/primitives.md#point), l: [Line](./types/primitives.md#line))`

**Return type**: [Point](./types/primitives.md#point)

**Returns**: intersection of lines `k` and `l`.

## `mid`

**Note**: The following functions allow any positive numbers of arguments.

* `mid(v_1: [Scalar (any unit u)](./types/primitives.md#Scalar), v_2 [Scalar (the same unit u)](./types/primitives.md#Scalar), ..., v_n: [Scalar (the same unit u)](./types/primitives.md#Scalar))`

**Return type**: [Scalar (the same unit u)](./types/primitives.md#Scalar)

**Returns**: The average value of `v_1`, `v_2`, ... `v_n`.

* `mid(P_1: [Point](./types/primitives.md#Point), P_2 [Point](./types/primitives.md#Point), ..., P_n: [Point](./types/primitives.md#Point))`

**Return type**: [Point](./types/primitives.md#Point)

**Returns**: The middle point of `P_1`, `P_2`, ... `P_n`. Special cases: when `n=2`, the middle of a segment; When `n=3`, the centroid of a triangle.

## `parallel_through`

* `parallel_through(P: [Point](./types/primitives.md#point), k: [Line](./types/primitives.md#Line))`
* `parallel_through(k: [Line](./types/primitives.md#line), P: [Point](./types/primitives.md#Point))`

**Return type**: [Line](./types/primitives.md#Line)

**Returns**: a line parallel to `k`, passing through `P`.

## `perpendicular_through`

* `perpendicular_through(P: [Point](./types/primitives.md#point), k: [Line](./types/primitives.md#Line))`
* `perpendicular_through(k: [Line](./types/primitives.md#line), P: [Point](./types/primitives.md#Point))`

**Return type**: [Line](./types/primitives.md#Line)

**Returns**: a line perpendicular to `k`, passing through `P`.

## `Point`

* `Point()`

**Return type**: [Point](./types/primitives.md#Circle)

**Returns**: an adjusted (free) point.

## `radians`

* `radians(value: [Scalar (no unit)])`

**Return type**: [Scalar (angle)](./types/primitives.md#Scalar)

**Returns**: an angle with the given measurement in radians. Related: [degrees](#degrees)

## `Segment`

* `Segment(AB: [2-P](./types/bundle-types.md#point-collections))`
* `Segment(A: [Point](./types/primitives.md#point), B: [Point](./types/primitives.md#point))`

**Return type**: [Segment](./types/bundle-types.md#segment)

**Returns**: the semgent `AB`.