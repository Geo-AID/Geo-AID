use std::mem;

use crate::script::{
    unroll::{
        CompileContext, Function, UnrolledExpression,
        Properties,
    }, compile::PreFigure
};

use super::macros::{overload, call, perpendicular_through};

/// `perpendicular_through(line, point)` - returns a line perpendicular to the 1st argument going through point at 2nd argument.
pub fn line_point(args: &[UnrolledExpression], figure: &mut PreFigure, display: Option<Properties>) -> UnrolledExpression {
    mem::drop(display);
    let expr = perpendicular_through!(args[0], args[1]);

    figure.lines.push(expr.clone());

    expr
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("perpendicular_through"),
        Function {
            name: String::from("perpendicular_through"),
            overloads: vec![
                overload!((POINT, LINE) -> LINE {
                    |args, figure, _| {
                        call!(figure:line_point(args[1], args[0]))
                    }
                }),
                overload!((LINE, POINT) -> LINE : line_point),
            ],
        },
    );
}
