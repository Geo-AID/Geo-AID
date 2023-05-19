use crate::script::{
    token::{Position, Span},
    unroll::{
        CompileContext, Function
    },
};

use super::macros::{overload, intersection};

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("intersection"),
        Function {
            name: String::from("intersection"),
            overloads: vec![
                overload!((LINE, LINE) -> POINT {
                    |args, _, _| intersection!(args[0], args[1])
                })
            ],
        },
    );
}
