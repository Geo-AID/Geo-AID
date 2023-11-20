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

#[allow(unused_imports)]
use crate::script::unroll::{
    CompileContext, Expr, Function, Library, Line, Point, PointCollection, Properties,
};
use geo_aid_derive::overload;

#[allow(unused_imports)]
use super::macros::{bisector, call, index, intersection, line2};

/// bisector(point, point, point) - angle bisector.
pub fn point_point_point(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    mut c: Expr<Point>,
    _context: &mut CompileContext,
    display: Properties,
) -> Expr<Line> {
    drop(display);
    let expr = bisector!(a, b, c);

    // Render the bisector.
    // context
    //     .figure
    //     .rays
    //     .push((b.clone(), intersection!(expr, line2!(a, c))));

    // context.figure.segments.push((a.clone(), b.clone()));

    // context.figure.segments.push((c.clone(), b.clone()));

    expr
}

/// bisector(point, point) - bisector of a segment.
pub fn point_point(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    context: &mut CompileContext,
    display: Properties,
) -> Expr<Line> {
    use super::mid::function_point;
    use super::perpendicular::line_point;
    drop(display);

    let expr = call!(context:line_point(line2!(a, b), call!(context:function_point(vec![a, b]))));

    // context.figure.lines.push(expr.clone());

    expr
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("bisector"),
        Function {
            name: String::from("bisector"),
            overloads: vec![
                overload!((3-P) -> LINE {
                    |mut col: Expr<PointCollection>, context, _| call!(context:point_point_point(
                        index!(node col, 0),
                        index!(node col, 1),
                        index!(node col, 2)
                    ))
                }),
                overload!((POINT, POINT, POINT) -> LINE : point_point_point),
                overload!((2-P) -> LINE {
                    |mut col: Expr<PointCollection>, context, _| call!(context:point_point(
                        index!(node col, 0),
                        index!(node col, 1)
                    ))
                }),
                overload!((POINT, POINT) -> LINE : point_point),
            ],
        },
    );
}
