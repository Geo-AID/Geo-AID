# Flags

Flags are divided into flag groups.

## Ungrouped

These are flags directly in the global scope of flags. They're not in any group and generally refer to some specific settings.

### `point_inequalities`

**Type**: `bool`

**Default**: `true`

**Description**: Automatically adds rules for inequalities of all point entities.

**Notes**: Disabling this might lead to Geo-AID creating figures compressed into one point.
Only recommended for debugging or experimenting.

## `language`

This group of flags modifies how the script is interpreted.

### `complex_numbers`

**Type**: `bool`

**Default**: `false`

**Description**: Creates a variable `i` containing the complex unit.

## `optimizations`

This group of flags modifies how the compiler and generator optimize the figure.
