use crate::script::unroll::{Function, Library};

use super::macros::{entity, overload};

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Point"),
        Function {
            name: String::from("Point"),
            overloads: vec![overload!(() -> POINT {
                |_, context, _| {
                    entity!(POINT context.add_point())
                }
            })],
        },
    );
}
