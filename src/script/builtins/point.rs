use crate::script::{
    unroll::{
        Function, Library
    },
};

use super::macros::{overload, entity};

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("Point"),
        Function {
            name: String::from("Point"),
            overloads: vec![
                overload!(() -> POINT {
                    |_, context, _| {
                        entity!(context.add_point())
                    }
                })
            ],
        },
    );
}
