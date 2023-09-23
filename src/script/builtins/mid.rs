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

use crate::script::unroll::{CompileContext, Function, Library, Properties, UnrolledExpression};

use super::macros::{average, overload};

macro_rules! mid_function {
    ($t:ident, $name:ident) => {
        pub fn $name(args: &[UnrolledExpression], _context: &mut CompileContext, _display: Option<Properties>) -> UnrolledExpression {
            average!($t : args)
        }
    };
}

mid_function! {POINT, mid_function_point}
mid_function! {DISTANCE, mid_function_distance}
mid_function! {ANGLE, mid_function_angle}
mid_function! {SCALAR, mid_function_scalar}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("mid"),
        Function {
            name: String::from("mid"),
            overloads: vec![
                overload!((...ANGLE) -> ANGLE : mid_function_angle),
                overload!((...DISTANCE) -> DISTANCE : mid_function_distance),
                overload!((...POINT) -> POINT : mid_function_point),
                overload!((...SCALAR) -> SCALAR : mid_function_scalar),
            ],
        },
    );
}
