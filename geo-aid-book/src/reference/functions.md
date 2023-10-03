# Functions

## `angle`

* `angle(ABC: [3-P](./types/budnle-types.md#point-collections))`
* `angle(A: [Point](./types/primitives.md#point), B: [Point](./types/primitives.md#point), C: [Point](./types/primitives.md#point))`

**Return type**: [Scalar (angle)](./types/primitives.md#scalar)

**Returns**: measurement of the angle `ABC`

* `angle(k: [Line](./types/primitives.md#point), l: [Line](./types/primitives.md#line))`

**Return type**: [Scalar (angle)](./types/primitives.md#scalar)

**Returns**: measurement of the angle between `k` and `l`. Which angle, depends on the order of the lines. For predictable outcome, the point versions are strongly recommended.