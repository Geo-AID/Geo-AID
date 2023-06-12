use std::mem;

use crate::script::{
    unroll::{
        CompileContext, Function, UnrolledExpression, Properties,
    }, compile::PreFigure,
};

use super::macros::{overload, call, circle_expr, free};

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function(args: &[UnrolledExpression],figure: &mut PreFigure, display: Option<Properties>) -> UnrolledExpression {
    mem::drop(display);
    let expr = circle_expr!(args[0], args[1]);

    figure.circles.push(expr.clone());
    expr
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("Circle"),
        Function {
            name: String::from("Circle"),
            overloads: vec![
                overload!((POINT, DISTANCE) -> CIRCLE : circle_function),
                overload!((DISTANCE, POINT) -> CIRCLE {
                    |args, figure, _| call!(figure:circle_function(args[1], args[0]))
                }),
                overload!(() -> CIRCLE {
                    |_, figure, _| call!(figure:circle_function(free!(POINT), free!(SCALAR)))
                })
            ],
        },
    );
}
