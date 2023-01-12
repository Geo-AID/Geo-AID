## Version 0.1.0
- Primitive GeoScript
- Primitive figure rendering
- Basic generation

## Version 0.2.0
- Refactorized svg.rs to optimize the rendering
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