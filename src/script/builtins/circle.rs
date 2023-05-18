use crate::script::{
    token::{Position, Span},
    unroll::{
        CompileContext, Function, UnrolledExpression, Properties,
    }, compile::PreFigure,
};

use super::{overload, call, circle_expr};

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function(args: &Vec<UnrolledExpression>, figure: &mut PreFigure, display: Option<Properties>) -> UnrolledExpression {
    circle_expr!(args[0], args[1])
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
            ],
        },
    );
}
