//! The `Line` function

use super::prelude::*;
use geo_aid_derive::overload;

/// `Line(point, point)` - a line through two points
pub fn function_pp(
    a: Expr<Point>,
    b: Expr<Point>,
    context: &CompileContext,
    mut display: Properties,
) -> Expr<Line> {
    let line_type = display.get("type").maybe_unset(LineType::Line);
    let mut ln = context.line_display(a, b, display);

    if let Some(node) = &mut ln.node {
        node.root.line_type = line_type;
    }

    ln
}

/// Register the function
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Line"),
        Function {
            overloads: vec![
                overload!((2-P) -> LINE {
                    |mut col: Expr<PointCollection>, context, display| call!(context:function_pp(
                        index!(node col, 0),
                        index!(node col, 1)
                    ) with display)
                }),
                overload!((POINT, POINT) -> LINE : function_pp),
            ],
        },
    );
}
