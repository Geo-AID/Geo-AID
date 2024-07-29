use super::prelude::*;
use geo_aid_derive::overload;

/// `parallel_through(line, point)` - returns a line parallel to the 1st argument going through point at 2nd argument.
fn parallel_function_line_point(
    line: Expr<Line>,
    point: Expr<Point>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Line> {
    context.parallel_through_display(line, point, display)
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("parallel_through"),
        Function {
            overloads: vec![
                overload!((POINT, LINE) -> LINE {
                    |point: Expr<Point>, line: Expr<Line>, figure, _| {
                        call!(figure:parallel_function_line_point(line, point))
                    }
                }),
                overload!((LINE, POINT) -> LINE : parallel_function_line_point),
            ],
        },
    );
}
