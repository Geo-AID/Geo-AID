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

use geo_aid_derive::overload;
#[allow(unused_imports)]
use std::f64::consts::PI;

#[allow(unused_imports)]
use crate::script::{
    unit,
    unroll::{CompileContext, Expr, Function, Library, Scalar, Properties},
};

#[allow(unused_imports)]
use super::macros::number;

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("degrees"),
        Function {
            name: String::from("degrees"),
            overloads: vec![overload!((SCALAR) -> ANGLE {
                |v: Expr<Scalar>, context: &mut CompileContext, display| {
                    context.set_unit_display(context.mult(v, number!(PI / 180.0)), unit::ANGLE, display)
                }
            })],
        },
    );
}
