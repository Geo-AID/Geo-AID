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

/// `perpendicular_through(line, point)` - returns a line perpendicular to the 1st argument going through point at 2nd argument.
pub fn line_point(
    line: Expr<Line>,
    point: Expr<Point>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Line> {
    context.perpendicular_through_display(line, point, display)
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("perpendicular_through"),
        Function {
            overloads: vec![
                overload!((POINT, LINE) -> LINE {
                    |point: Expr<Point>, line: Expr<Line>, figure, _| {
                        call!(figure:line_point(line, point))
                    }
                }),
                overload!((LINE, POINT) -> LINE : line_point),
            ],
        },
    );
}
