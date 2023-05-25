use std::mem;

use crate::script::{
    unroll::{
        CompileContext, Function, UnrolledExpression, Properties,
    },
    compile::PreFigure,
};

use super::macros::{angle_expr, call, index, overload};

/// angle(point, point, point) - angle depicted by 3 points.
fn angle_function_point_point_point(
    args: &[UnrolledExpression],
    _figure: &mut PreFigure,
    display: Option<Properties>,
) -> UnrolledExpression {
    mem::drop(display);
    angle_expr!(args[0], args[1], args[2])
}

/// angle(line, line) - distance between a point and a line.
fn angle_function_line_line(
    args: &[UnrolledExpression],
    _figure: &mut PreFigure,
    display: Option<Properties>,
) -> UnrolledExpression {
    mem::drop(display);
    angle_expr!(args[0], args[1])
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("angle"),
        Function {
            name: String::from("angle"),
            overloads: vec![
                overload!((3-P) -> ANGLE {
                    |args, figure, _| call!(figure:angle_function_point_point_point(
                        index!(args[0], 0),
                        index!(args[0], 1),
                        index!(args[0], 2)
                    ))
                }),
                overload!((POINT, POINT, POINT) -> ANGLE : angle_function_point_point_point),
                overload!((LINE, LINE) -> ANGLE : angle_function_line_line),
            ],
        },
    );
}
