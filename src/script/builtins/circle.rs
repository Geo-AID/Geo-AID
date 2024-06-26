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
