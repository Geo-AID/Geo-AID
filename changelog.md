## Version 0.1.0
- Primitive GeoScript
- Primitive figure rendering
- Basic generation

## Version 0.2.0
- Refactored svg.rs to optimize the rendering
- Created the latex drawer
- Completed the documentation as of now.
- Added a Command Line Interface
- Pretty error printing
- Added the JSON drawer
- Added the raw drawer
- Added angle support
- Fixed draw signatures
- Added benchmarking for generation

## Version 0.2.1
- Added testing environment for the projector and drawers
- Added testing environment for Geo-AID in general.
- Added support for multiple iteration levels in iterators
- Changed implicit iterators from being separated with `|` to being separated with `,`.
- Fixed faulty display of multiline error messages.
- Added command line options for JSON and raw drawers.
- Added builtin functions: bisector, mid (average), parallel, perpendicular, intersection
- Added point collection constructors.
- Added parser support for compiler flags.
- Added identical_expressions optimization flag, allowing to optimise for calculating identical expressions.
- Improved generator-projector pipeline.
- Distance literals now work properly.
- Error messages now can suggest code changes.

## Version 0.2.2
- An overhaul of expressions with proper value caching and precomputed weights.
- Improved angle support.
- Added segment support.

## Version 0.2.3
- Added ray support.
- Added circle primitive and function.
- Added circle support and improved latex drawer.
- Heavily changed the display system.

## Version 0.2.4
- Added the circle clip and the line clip
- Added basic bundle type support
- Added the `Segment` function
- Added the `lies_on` operator for points on circles, lines and segments.

## Version 0.2.5
- Huge compiler & generator refactor
- A point laying on two lines is now defined as an intersection of them.
- Added proper documentation.

## Version 0.2.6
- Added drawing modes for lines, segments, rays, angles and circles (dashed, dotted, bolded, default)
- Fixed that any file involving the `lies_on` rule would inevitably crash because of point equality prevention rules.
- Fixed that negating some normal rules would not actually negate them.
- Fixed that negating `lies_on` would not actually negate it (no effect would be achieved).
- The `svg` drawing option is now default.
- Updated README to contain information about the book.

## Version 0.3.0
- Rewrote the entire display system (deciding what is displayed and what is not, based on the script)
- Added smart label positioning
- Point collection variables are no longer valid.

## Version 0.3.1
- **GeoScript**: Added custom weighing
- **GeoScript**: Added exponentiation support and rational unit powers
- **GeoScript**: Added fields and methods to the language
- **GeoScript**: Modified the syntax of negation and exponentiation
- **Drawers**: Refactored drawers for better maintenance capability