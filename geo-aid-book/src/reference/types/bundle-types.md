# Bundle types

Bundle types are zero-cost abstractions over primitives. There are two types of bundles: point collections and named bundles.

## Point collections

Point collections are simply ordered collections of points. It is never a seperate entity, only an abstraction over a set of points. Denoted as `<length>-P`. If `<length>` is given as `0`, it means a collection of any length. Most functions that accept points as arguments, also accept point collections.

## Named Bundles

Named bundles are similar to structs in C. They have names and named fields of any type (accessible through [field indexing](../syntax/names.md)). Denoted with their unique names different from the names of any other type.

### Segment

```
Segment {
    A: [Point](primitives.md#point),
    B: [Point](primitives.md#point)
}
```

`Segment`s have two fields denoting their ends.

*Methods*

* `len()`

**Return type**: [Scalar (distance)](./types/bundle-types.md#segment)

**Returns**: the distance `AB`.

**Displays**: exactly what `dst` displays, except that the `draw_segment` property is `false` by default.