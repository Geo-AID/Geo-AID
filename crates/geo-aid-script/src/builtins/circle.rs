//! The circle function

use super::{prelude::*, ScalarUnit};

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function(
    center: Expr<Point>,
    radius: Distance,
    context: &CompileContext,
    display: Properties,
) -> Expr<Circle> {
    context.circle_display(center, radius.0, display)
}

/// Register the function
pub fn register(library: &mut Library) {
    library.add(
        Function::new("Circle")
            .overload(circle_function)
            .overload(
                |radius: Distance, center: Expr<Point>, context: &CompileContext, display| {
                    circle_function(center, radius, context, display)
                },
            )
            .overload(|context: &mut CompileContext, display| {
                let mut center = context.free_point();
                let mut radius = context.free_scalar();

                center.take_node();
                radius.take_node();

                circle_function(
                    center,
                    ScalarUnit::from(context.set_unit(radius, unit::DISTANCE)),
                    context,
                    display,
                )
            }),
    );
}
