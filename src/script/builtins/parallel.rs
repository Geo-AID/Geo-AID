use std::mem;

use crate::script::{
    token::{Position, Span},
    unroll::{
        CompileContext, Function, UnrolledExpression,
        Properties,
    }, compile::PreFigure,
};

use super::macros::{overload, call, parallel_through};

/// `parallel_through(line, point)` - returns a line parallel to the 1st argument going through point at 2nd argument.
fn parallel_function_line_point(args: &[UnrolledExpression], figure: &mut PreFigure, display: Option<Properties>) -> UnrolledExpression {
    mem::drop(display);
    let expr = parallel_through!(args[0], args[1]);

    figure.lines.push(expr.clone());

    expr
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("parallel_through"),
        Function {
            name: String::from("parallel_through"),
            overloads: vec![
                overload!((POINT, LINE) -> LINE {
                    |args, figure, _| {
                        call!(figure:parallel_function_line_point(args[1], args[0]))
                    }
                }),
                overload!((LINE, POINT) -> LINE : parallel_function_line_point),
            ],
        },
    );
}
