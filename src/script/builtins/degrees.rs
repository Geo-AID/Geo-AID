use std::f64::consts::PI;

use crate::script::{
    unroll::{
        Function, Library
    }
};

use super::macros::{overload, set_unit, math, number};

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("degrees"),
        Function {
            name: String::from("degrees"),
            overloads: vec![
                overload!((SCALAR) -> ANGLE {
                    |args, _, _| set_unit!(math!(*, args[0], number!(PI / 180.0)), %ANGLE)
                })
            ],
        },
    );
}
