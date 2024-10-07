//! The `degrees` function.

#[allow(unused_imports)]
use crate::token::number::ProcNum;
#[allow(unused_imports)]
use num_traits::FromPrimitive;

use super::prelude::*;

/// Register the function
pub fn register(library: &mut Library) {
    library.add(
        Function::new("degrees")
            .alias("deg")
            .overload(|v: Unitless, context: &CompileContext, display| {
                Angle::from(context.mult_display(
                    v.0,
                    number!(ANGLE ProcNum::pi() / &ProcNum::from_i32(180).unwrap()),
                    display,
                ))
            })
            .overload(|v: Angle, context: &CompileContext, display| {
                Unitless::from(context.div_display(
                    v.0,
                    number!(ANGLE ProcNum::pi() / &ProcNum::from_i32(180).unwrap()),
                    display,
                ))
            }),
    );
}
