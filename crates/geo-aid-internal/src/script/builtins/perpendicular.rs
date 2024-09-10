use super::prelude::*;
use geo_aid_derive::overload;

/// `perpendicular_through(line, point)` - returns a line perpendicular to the 1st argument going through point at 2nd argument.
pub fn line_point(
    line: Expr<Line>,
    point: Expr<Point>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Line> {
    context.perpendicular_through_display(line, point, display)
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("perpendicular_through"),
        Function {
            overloads: vec![
                overload!((POINT, LINE) -> LINE {
                    |point: Expr<Point>, line: Expr<Line>, figure, display| {
                        call!(figure:line_point(line, point) with display)
                    }
                }),
                overload!((LINE, POINT) -> LINE : line_point),
            ],
        },
    );
}
