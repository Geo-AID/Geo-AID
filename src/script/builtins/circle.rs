/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use crate::{script::{unroll::{
    Circle, CompileContext, Expr, Function, Library, Point, Properties, Scalar
}, unit}, take_nodes};
use geo_aid_derive::overload;

#[allow(unused_imports)]
use super::macros::call;

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function(
    mut center: Expr<Point>,
    mut radius: Expr<Scalar>,
    context: &mut CompileContext,
    display: Properties
) -> Expr<Circle> {
    let nodes = take_nodes!(center, radius);
    context.expr_with(
        Circle::Circle(center, radius),
        display,
        nodes
    )
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Circle"),
        Function {
            name: String::from("Circle"),
            overloads: vec![
                overload!((POINT, DISTANCE) -> CIRCLE : circle_function),
                overload!((DISTANCE, POINT) -> CIRCLE {
                    |radius: Expr<Scalar>, center: Expr<Point>, context, display| call!(context:circle_function(center, radius) with display)
                }),
                overload!(() -> CIRCLE {
                    |context: &mut CompileContext, display| {
                        let pt = context.add_point();
                        let sc = context.add_scalar();

                        call!(
                            context:circle_function(
                                context.entity_p(pt),
                                context.entity_s(sc, unit::DISTANCE)
                            ) with display
                        )
                    }
                })
            ],
        },
    );
}
