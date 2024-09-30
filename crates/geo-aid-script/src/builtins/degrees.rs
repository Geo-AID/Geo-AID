//! The `degrees` function.

#[allow(unused_imports)]
use crate::token::number::ProcNum;
#[allow(unused_imports)]
use num_traits::FromPrimitive;

use super::prelude::*;

/// Register the function
pub fn register(library: &mut Library) {
    library.add(Function::new("degrees").overload(
        |v: Unitless, context: &CompileContext, display| {
            Angle::from(context.set_unit_display(
                context.mult(
                    v.0,
                    number!(ProcNum::pi() / &ProcNum::from_i32(180).unwrap()),
                ),
                unit::ANGLE,
                display,
            ))
        },
    ));
}
