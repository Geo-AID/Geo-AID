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
    CompileContext, Expr, Function, Library, Line, Point, PointCollection, Properties, Scalar,
};
use geo_aid_derive::overload;

#[allow(unused_imports)]
use super::macros::{angle_expr, call, index};

/// angle(point, point, point) - angle depicted by 3 points.
fn angle_function_point_point_point(
    a: &Expr<Point>,
    b: &Expr<Point>,
    c: &Expr<Point>,
    _context: &mut CompileContext,
    display: Option<Properties>,
) -> Expr<Scalar> {
    drop(display);
    angle_expr!(a, b, c)
}

/// angle(line, line) - distance between a point and a line.
fn angle_function_line_line(
    k: &Expr<Line>,
    l: &Expr<Line>,
    _context: &mut CompileContext,
    display: Option<Properties>,
) -> Expr<Scalar> {
    drop(display);
    angle_expr!(k, l)
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("angle"),
        Function {
            name: String::from("angle"),
            overloads: vec![
                overload!((3-P) -> ANGLE {
                    |col: &Expr<PointCollection>, context, _| call!(context:angle_function_point_point_point(
                        index!(col, 0),
                        index!(col, 1),
                        index!(col, 2)
                    ))
                }),
                overload!((POINT, POINT, POINT) -> ANGLE : angle_function_point_point_point),
                overload!((LINE, LINE) -> ANGLE : angle_function_line_line),
            ],
        },
    );
}
