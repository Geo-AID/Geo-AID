use crate::script::{
        token::{Position, Span},
        unroll::{
            CompileContext, Function, UnrolledExpression,
            Properties,
        }, compile::PreFigure,
    };

use super::{overload, call, index, bisector_expr, line_expr};

/// bisector(point, point, point) - angle bisector.
pub fn point_point_point(args: &Vec<UnrolledExpression>, figure: &mut PreFigure, display: Option<Properties>) -> UnrolledExpression {
    bisector_expr!(args[0], args[1], args[2])
}

/// bisector(point, point) - bisector of a segment.
pub fn point_point(args: &Vec<UnrolledExpression>, figure: &mut PreFigure, display: Option<Properties>) -> UnrolledExpression {
    use super::perpendicular::line_point;
    use super::mid::mid_function_point;

    call!(figure:line_point(line_expr!(args[0], args[1]), call!(figure:mid_function_point(args[0], args[1]))))
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
                overload!((POINT, POINT) -> LINE : point_point)
            ],
        },
    );
}
