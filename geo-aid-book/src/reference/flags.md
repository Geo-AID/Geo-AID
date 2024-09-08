# Flags

Flags are divided into flag groups.

## Ungrouped

These are flags directly in the global scope of flags. They're not in any group and generally refer to some specific settings.

### `point_inequalities`

**Type**: `bool`

**Default**: `true`

**Description**: Automatically adds rules for inequalities of all point entities.

**Notes**: It increases the rule count by a lot, decreases figure stability. Experiments with it have rendered it unsuitable for most uses.

## `optimizations`

This group of flags modifies how the compiler and generator optimize the figure.
