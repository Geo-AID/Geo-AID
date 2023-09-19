use crate::script::{
    compile::PreFigure,
    token::{Position, Span},
    unroll::{CompileContext, Function, Properties, UnrolledExpression},
};

use super::macros::{average, overload};

macro_rules! mid_function {
    ($t:ident, $name:ident) => {
        pub fn $name(
            args: &[UnrolledExpression],
            _figure: &mut PreFigure,
            _display: Option<Properties>,
        ) -> UnrolledExpression {
            average!($t: args)
        }
    };
}

mid_function! {POINT, mid_function_point}
mid_function! {DISTANCE, mid_function_distance}
mid_function! {ANGLE, mid_function_angle}
mid_function! {SCALAR, mid_function_scalar}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("mid"),
        Function {
            name: String::from("mid"),
            overloads: vec![
                overload!((...ANGLE) -> ANGLE : mid_function_angle),
                overload!((...DISTANCE) -> DISTANCE : mid_function_distance),
                overload!((...POINT) -> POINT : mid_function_point),
                overload!((...SCALAR) -> SCALAR : mid_function_scalar),
            ],
        },
    );
}
