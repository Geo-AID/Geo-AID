//! The `radians` function.

use super::prelude::*;
use geo_aid_derive::overload;

/// Register the function
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("radians"),
        Function {
            overloads: vec![overload!((SCALAR) -> ANGLE {
                |v: Expr<Scalar>, context: &CompileContext, display| context.set_unit_display(v, unit::ANGLE, display)
            })],
        },
    );
}
