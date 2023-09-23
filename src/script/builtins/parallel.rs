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

use super::macros::{call, overload, parallel_through};

/// `parallel_through(line, point)` - returns a line parallel to the 1st argument going through point at 2nd argument.
fn parallel_function_line_point(
    args: &[UnrolledExpression],
    context: &mut CompileContext,
    display: Option<Properties>,
) -> UnrolledExpression {
    mem::drop(display);
    let expr = parallel_through!(args[0], args[1]);

    context.figure.lines.push(expr.clone());

    expr
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("parallel_through"),
        Function {
            name: String::from("parallel_through"),
            overloads: vec![
                overload!((POINT, LINE) -> LINE {
                    |args, figure, _| {
                        call!(figure:parallel_function_line_point(args[1], args[0]))
                    }
                }),
                overload!((LINE, POINT) -> LINE : parallel_function_line_point),
            ],
        },
    );
}
