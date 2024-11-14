//! The `parallel_through` function

use super::prelude::*;

/// `parallel_through(line, point)` - returns a line parallel to the 1st argument going through point at 2nd argument.
fn line_point(
    line: Expr<Line>,
    point: Expr<Point>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Line> {
    context.parallel_through_display(line, point, display)
}

/// Register the function
pub fn register(library: &mut Library) {
    library.add(
        Function::new("parallelthrough")
            .alias("parallel")
            .overload(line_point)
            .overload(
                |point: Expr<Point>, line: Expr<Line>, context: &CompileContext, props| {
                    line_point(line, point, context, props)
                },
            ),
    );
}
