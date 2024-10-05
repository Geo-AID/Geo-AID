//! The `Point` function.

use super::prelude::*;

/// Register the function
pub fn register(library: &mut Library) {
    library.add(
        Function::new("point").overload(|context: &CompileContext, props| {
            context.expr_with(Point::Free, props, Vec::new())
        }),
    );
}
