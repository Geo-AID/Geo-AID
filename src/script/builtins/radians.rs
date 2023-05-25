use crate::script::{
    unroll::{
        CompileContext, Function
    },
};

use super::macros::{overload, set_unit};

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("radians"),
        Function {
            name: String::from("radians"),
            overloads: vec![
                overload!((SCALAR) -> ANGLE {
                    |args, _, _| set_unit!(args[0], %ANGLE)
                })
            ],
        },
    );
}
