//! The `radians` function.

use super::prelude::*;

/// Register the function
pub fn register(library: &mut Library) {
    library.add(Function::new("radians").overload(
        |v: Unitless, context: &CompileContext, display| {
            Angle::from(context.set_unit_display(v.0, unit::ANGLE, display))
        },
    ));
}
