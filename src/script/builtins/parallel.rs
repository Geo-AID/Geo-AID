use std::mem;

use crate::script::unroll::{
    CompileContext, Function, UnrolledExpression,
    Properties,
};

use super::macros::{overload, call, parallel_through};

/// `parallel_through(line, point)` - returns a line parallel to the 1st argument going through point at 2nd argument.
fn parallel_function_line_point(args: &[UnrolledExpression], context: &mut CompileContext, display: Option<Properties>) -> UnrolledExpression {
    mem::drop(display);
    let expr = parallel_through!(args[0], args[1]);

    context.figure.lines.push(expr.clone());

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
