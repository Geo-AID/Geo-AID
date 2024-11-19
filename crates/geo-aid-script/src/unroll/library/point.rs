//! The `Point` function.

use crate::unroll::{figure::Node, Number};

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
                .alias_method(ty::POINT, "x")
                .alias_method(ty::collection(1), "x")
                .overload(|mut point: Expr<Point>, context: &CompileContext, props| {
                    let node = point.take_node().map(|n| Box::new(n) as Box<dyn Node>);
                    Distance::from(context.expr_with(
                        Number {
                            unit: Some(unit::DISTANCE),
                            data: NumberData::PointX(point),
                        },
                        props,
                        node.into_iter().collect(),
                    ))
                }),
        )
        .add(
            Function::new("y")
                .alias_method(ty::POINT, "y")
                .alias_method(ty::collection(1), "y")
                .overload(|mut point: Expr<Point>, context: &CompileContext, props| {
                    let node = point.take_node().map(|n| Box::new(n) as Box<dyn Node>);
                    Distance::from(context.expr_with(
                        Number {
                            unit: Some(unit::DISTANCE),
                            data: NumberData::PointY(point),
                        },
                        props,
                        node.into_iter().collect(),
                    ))
                }),
        )
        .add(
            Function::new("topoint")
                .alias_method(ty::POINT, "topoint")
                .overload(|number: Distance, context: &CompileContext, props| {
                    context.to_point_display(number.0, props)
                }),
        );
}
