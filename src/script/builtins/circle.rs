use std::mem;

use crate::script::unroll::{CompileContext, Function, Library, Properties, UnrolledExpression};

use super::macros::{call, circle_expr, entity, overload};

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function(
    args: &[UnrolledExpression],
    context: &mut CompileContext,
    display: Option<Properties>,
) -> UnrolledExpression {
    mem::drop(display);
    let expr = circle_expr!(args[0], args[1]);

    context.figure.circles.push(expr.clone());
    expr
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Circle"),
        Function {
            name: String::from("Circle"),
            overloads: vec![
                overload!((POINT, DISTANCE) -> CIRCLE : circle_function),
                overload!((DISTANCE, POINT) -> CIRCLE {
                    |args, context, _| call!(context:circle_function(args[1], args[0]))
                }),
                overload!(() -> CIRCLE {
                    |_, context, _| call!(context:circle_function(entity!(POINT context.add_point()), entity!(SCALAR context.add_scalar())))
                })
            ],
        },
    );
}
