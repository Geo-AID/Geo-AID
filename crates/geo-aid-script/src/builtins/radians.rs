//! The `radians` function.

use num_traits::One;

use crate::token::number::ProcNum;

use super::prelude::*;

/// Register the function
pub fn register(library: &mut Library) {
    library.add(
        Function::new("radians")
            .overload(|v: Unitless, context: &CompileContext, display| {
                Angle::from(context.set_unit_display(v.0, unit::ANGLE, display))
            })
            .overload(|v: Angle, context: &CompileContext, display| {
                Unitless::from(context.div_display(v.0, number!(ANGLE ProcNum::one()), display))
            }),
    );
}
