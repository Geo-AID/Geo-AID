# Flags

Flags are divided into flag groups.

## Ungrouped

These are flags directly in the global scope of flags. They're not in any group and generally refer to some specific settings.

### `point_bounds`

**Type**: `bool`

**Default**: `false`

**Description**: Automatically adds rules for all adjusted points for their coordinates not to go out of the (0,0) to (1,1) plane.

**Notes**: It increases the rule count by a lot, decreases figure stability. Experiments with it have rendered it unsuitable for most uses.

## `optimizations`

This group of flags modifies how optimization the compiler and generator optimize the figure.

### `identical_expressions`

**Type**: `bool`

**Default**: `true`

**Description**: This optimization makes the generator cache the result of an expression's result when there is another, identical expression. Trivial expressions, where caching would only introduce unnecessary overhead, don't get cached.

**Notes**: On simple figures, paradoxically, this optimization increases the generation time. The introduced penalty, however, is minimal, which is why the flag is enabled by default.