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

use crate::script::builtins::macros::construct_bundle;
use std::mem;

use crate::script::unroll::{CompileContext, Function, Library, Properties, UnrolledExpression};

use super::macros::{call, index, overload};

fn segment_function_point_point(
    args: &[UnrolledExpression],
    _context: &mut CompileContext,
    display: Option<Properties>,
) -> UnrolledExpression {
    mem::drop(display);

    construct_bundle!(Segment {
        A: args[0],
        B: args[1]
    })
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Segment"),
        Function {
            name: String::from("Segment"),
            overloads: vec![
                overload!((2-P) -> Segment {
                    |args, context, _| call!(context:segment_function_point_point(
                        index!(args[0], 0),
                        index!(args[0], 1)
                    ))
                }),
                overload!((POINT, POINT) -> Segment : segment_function_point_point),
            ],
        },
    );
}
