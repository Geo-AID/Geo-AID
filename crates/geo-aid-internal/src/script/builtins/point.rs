use super::prelude::*;
use geo_aid_derive::overload;

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Point"),
        Function {
            overloads: vec![overload!(() -> POINT {
                |context: &mut CompileContext, props| {
                    context.expr_with(Point::Free, props, Vec::new())
                }
            })],
        },
    );
}
