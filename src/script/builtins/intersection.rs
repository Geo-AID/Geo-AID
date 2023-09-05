use crate::script::{
    unroll::{
        Function, Library
    },
};

use super::macros::{overload, intersection};

pub fn register(library: &mut Library) {
    library.functions.insert(
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
