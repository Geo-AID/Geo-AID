use crate::script::unroll::{Function, Library};

use super::macros::{overload, set_unit};

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("radians"),
        Function {
            name: String::from("radians"),
            overloads: vec![overload!((SCALAR) -> ANGLE {
                |args, _, _| set_unit!(args[0], %ANGLE)
            })],
        },
    );
}
