//! The `perpendicular_through` function

use super::prelude::*;

/// `perpendicular_through(line, point)` - returns a line perpendicular to the 1st argument going through point at 2nd argument.
pub fn line_point(
    line: Expr<Line>,
    point: Expr<Point>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Line> {
    context.perpendicular_through_display(line, point, display)
}

/// Register the function
pub fn register(library: &mut Library) {
    library.add(
        Function::new("perpendicularthrough")
            .alias("pependicular")
            .overload(line_point)
            .overload(
                |point: Expr<Point>, line: Expr<Line>, context: &CompileContext, props| {
                    line_point(line, point, context, props)
                },
            ),
    );
}
