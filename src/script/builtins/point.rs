use crate::script::{
    unroll::{CompileContext, Function},
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
