use super::prelude::*;
use geo_aid_derive::overload;

pub fn line_function_pp(
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

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Line"),
        Function {
            overloads: vec![
                overload!((2-P) -> LINE {
                    |mut col: Expr<PointCollection>, context, display| call!(context:line_function_pp(
                        index!(node col, 0),
                        index!(node col, 1)
                    ) with display)
                }),
                overload!((POINT, POINT) -> LINE : line_function_pp),
            ],
        },
    );
}
