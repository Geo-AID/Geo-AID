//! The `degrees` function.

#[allow(unused_imports)]
use crate::script::token::number::ProcNum;
use geo_aid_derive::overload;
#[allow(unused_imports)]
use num_traits::FromPrimitive;

use super::prelude::*;

/// Register the function
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("degrees"),
        Function {
            overloads: vec![overload!((SCALAR) -> ANGLE {
                |v: Expr<Scalar>, context: &mut CompileContext, display| {
                    context.set_unit_display(context.mult(v, number!(ProcNum::pi() / &ProcNum::from_i32(180).unwrap())), unit::ANGLE, display)
                }
            })],
        },
    );
}
