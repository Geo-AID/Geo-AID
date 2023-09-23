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

use std::mem;

use crate::script::unroll::{CompileContext, Function, Library, Properties, UnrolledExpression};

use super::macros::{overload, call, index, bisector, line2, intersection};

/// bisector(point, point, point) - angle bisector.
pub fn point_point_point(args: &[UnrolledExpression], context: &mut CompileContext, display: Option<Properties>) -> UnrolledExpression {
    mem::drop(display);
    let expr = bisector!(args[0], args[1], args[2]);

    // Render the bisector.
    context.figure.rays.push((
        args[1].clone(),
        intersection!(expr, line2!(args[0], args[2]))
    ));

    context.figure.segments.push((
        args[0].clone(),
        args[1].clone()
    ));

    context.figure.segments.push((
        args[2].clone(),
        args[1].clone()
    ));

    expr
}

/// bisector(point, point) - bisector of a segment.
pub fn point_point(args: &[UnrolledExpression], context: &mut CompileContext, display: Option<Properties>) -> UnrolledExpression {
    use super::perpendicular::line_point;
    use super::mid::mid_function_point;
    mem::drop(display);

    let expr = call!(context:line_point(line2!(args[0], args[1]), call!(context:mid_function_point(args[0], args[1]))));

    context.figure.lines.push(expr.clone());

    expr
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("bisector"),
        Function {
            name: String::from("bisector"),
            overloads: vec![
                overload!((3-P) -> LINE {
                    |args, context, _| call!(context:point_point_point(
                        index!(args[0], 0),
                        index!(args[0], 1),
                        index!(args[0], 2)
                    ))
                }),
                overload!((POINT, POINT, POINT) -> LINE : point_point_point),
                overload!((2-P) -> LINE {
                    |args, context, _| call!(context:point_point(
                        index!(args[0], 0),
                        index!(args[0], 1)
                    ))
                }),
                overload!((POINT, POINT) -> LINE : point_point)
            ],
        },
    );
}
