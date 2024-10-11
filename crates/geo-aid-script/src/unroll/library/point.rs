//! The `Point` function.

use crate::unroll::{figure::Node, Scalar};

use super::prelude::*;

/// Register the function
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("point").overload(|context: &CompileContext, props| {
                context.expr_with(Point::Free, props, Vec::new())
            }),
        )
        .add(
            Function::new("x")
                .alias("[point]::x")
                .alias("[point collection (1)]::x")
                .overload(|mut point: Expr<Point>, context: &CompileContext, props| {
                    let node = point.take_node().map(|n| Box::new(n) as Box<dyn Node>);
                    Distance::from(context.expr_with(
                        Scalar {
                            unit: Some(unit::DISTANCE),
                            data: ScalarData::PointX(point),
                        },
                        props,
                        node.into_iter().collect(),
                    ))
                }),
        )
        .add(
            Function::new("y")
                .alias("[point]::y")
                .alias("[point collection (1)]::y")
                .overload(|mut point: Expr<Point>, context: &CompileContext, props| {
                    let node = point.take_node().map(|n| Box::new(n) as Box<dyn Node>);
                    Distance::from(context.expr_with(
                        Scalar {
                            unit: Some(unit::DISTANCE),
                            data: ScalarData::PointY(point),
                        },
                        props,
                        node.into_iter().collect(),
                    ))
                }),
        );
}
