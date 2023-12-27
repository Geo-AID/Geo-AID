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

use crate::script::unroll::{BuildAssociated, Point, PointNode, Properties};
use crate::script::unroll::{CompileContext, Expr, Function, Library, Line};
use geo_aid_derive::overload;

fn intersection_function_line_line(
    k: Expr<Line>,
    l: Expr<Line>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Point> {
    let mut expr = context.intersection_display(k, l, display);

    if let Some(node) = &mut expr.node {
        node.set_associated(Associated);
    }

    expr
}

#[derive(Debug)]
pub struct Associated;

impl BuildAssociated<PointNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        _compiler: &mut crate::script::compile::Compiler,
        _figure: &mut crate::script::figure::Figure,
        associated: &mut crate::script::unroll::HierarchyNode<PointNode>,
    ) {
        associated.root.display_dot.set_if_unset(false);
    }
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("intersection"),
        Function {
            name: String::from("intersection"),
            overloads: vec![overload!((LINE, LINE) -> POINT : intersection_function_line_line)],
        },
    );
}
