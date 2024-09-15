//! The circle function

use super::prelude::*;
use geo_aid_derive::overload;

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function(
    center: Expr<Point>,
    radius: Expr<Scalar>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Circle> {
    context.circle_display(center, radius, display)
}

/// Register the function
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Circle"),
        Function {
            overloads: vec![
                overload!((POINT, DISTANCE) -> CIRCLE : circle_function),
                overload!((DISTANCE, POINT) -> CIRCLE {
                    |radius: Expr<Scalar>, center: Expr<Point>, context, display| call!(context:circle_function(center, radius) with display)
                }),
                overload!(() -> CIRCLE {
                    |context: &mut CompileContext, display| {
                        let mut center = context.free_point();
                        let mut radius = context.free_scalar();

                        center.take_node();
                        radius.take_node();

                        call!(
                            context:circle_function(
                                center,
                                context.set_unit(radius, unit::DISTANCE)
                            ) with display
                        )
                    }
                })
            ],
        },
    );
}
