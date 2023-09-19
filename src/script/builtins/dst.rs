use crate::script::{
    token::{Position, Span},
    unroll::{CompileContext, Function},
};

use super::macros::{distance, overload, set_unit};

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("dst"),
        Function {
            name: String::from("dst"),
            overloads: vec![
                overload!((DISTANCE) -> DISTANCE {
                    |args, _, _| args[0].clone()
                }),
                overload!((SCALAR) -> DISTANCE {
                    |args, _, _| set_unit!(args[0], %DISTANCE)
                }),
                overload!((POINT, POINT) -> DISTANCE {
                    |args, _, _| distance!(PP: args[0], args[1])
                }),
                overload!((POINT, LINE) -> DISTANCE {
                    |args, _, _| distance!(PL: args[0], args[1])
                }),
                overload!((POINT, POINT) -> DISTANCE {
                    |args, _, _| distance!(PL: args[1], args[0])
                }),
            ],
        },
    );
}
