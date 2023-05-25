use std::mem;

use crate::script::{
        unroll::{
            CompileContext, Function, UnrolledExpression,
            Properties,
        }, compile::PreFigure
    };

use super::macros::{bisector, call, index, intersection, line2, overload};

/// bisector(point, point, point) - angle bisector.
pub fn point_point_point(
    args: &[UnrolledExpression],
    figure: &mut PreFigure,
    display: Option<Properties>,
) -> UnrolledExpression {
    mem::drop(display);
    let expr = bisector!(args[0], args[1], args[2]);

    // Render the bisector.
    figure.rays.push((
        args[1].clone(),
        intersection!(expr, line2!(args[0], args[2])),
    ));

    figure.segments.push((
        args[0].clone(),
        args[1].clone()
    ));

    figure.segments.push((
        args[2].clone(),
        args[1].clone()
    ));

    expr
}

/// bisector(point, point) - bisector of a segment.
pub fn point_point(
    args: &[UnrolledExpression],
    figure: &mut PreFigure,
    display: Option<Properties>,
) -> UnrolledExpression {
    use super::mid::mid_function_point;
    mem::drop(display);

    let expr = call!(
            figure:
                line_point(
                    line2!(args[0], args[1]),
                    call!(figure:mid_function_point(args[0], args[1]))
                )
    );

    figure.lines.push(expr.clone());

    expr
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("bisector"),
        Function {
            name: String::from("bisector"),
            overloads: vec![
                overload!((3-P) -> LINE {
                    |args, figure, _| call!(figure:point_point_point(
                        index!(args[0], 0),
                        index!(args[0], 1),
                        index!(args[0], 2)
                    ))
                }),
                overload!((POINT, POINT, POINT) -> LINE : point_point_point),
                overload!((2-P) -> LINE {
                    |args, figure, _| call!(figure:point_point(
                        index!(args[0], 0),
                        index!(args[0], 1)
                    ))
                }),
                overload!((POINT, POINT) -> LINE : point_point),
            ],
        },
    );
}
