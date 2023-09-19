use std::f64::consts::PI;

use crate::script::{
    token::{Position, Span},
    unroll::{CompileContext, Function},
};

use super::macros::{math, number, overload, set_unit};

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("degrees"),
        Function {
            name: String::from("degrees"),
            overloads: vec![overload!((SCALAR) -> ANGLE {
                |args, _, _| set_unit!(math!(*, args[0], number!(PI / 180.0)), %ANGLE)
            })],
        },
    );
}
