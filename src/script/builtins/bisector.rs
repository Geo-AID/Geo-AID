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

use std::rc::Rc;

use crate::{script::{unroll::{CloneWithNode, AssociatedData, HierarchyNode, LineNode, LineType, BuildAssociated}, figure::{MathString, Figure}, compile::Compiler}, generator::fast_float::FastFloat, span};
#[allow(unused_imports)]
use crate::script::unroll::{
    CompileContext, Expr, Function, Library, Line, Point, PointCollection, Properties,
};
use geo_aid_derive::overload;

#[allow(unused_imports)]
use super::macros::{call, index};

/// bisector(point, point, point) - angle bisector.
pub fn point_point_point(
    a: Expr<Point>,
    b: Expr<Point>,
    c: Expr<Point>,
    context: &CompileContext,
    mut display: Properties,
) -> Expr<Line> {
    // We're highjacking the node creation, so util functions.
    let mut expr = Expr {
        weight: FastFloat::One,
        span: span!(0, 0, 0, 0),
        data: Rc::new(Line::AngleBisector(a, b, c)),
        node: None
    };

    // Line node.
    let mut node = HierarchyNode::new(LineNode {
        display: display.get("display").maybe_unset(true),
        label: display.get("label").maybe_unset(MathString::new(span!(0, 0, 0, 0))),
        display_label: display.get("display_label").maybe_unset(false),
        line_type: display.get("is_ray").maybe_unset(LineType::Ray), // The change. Bisectors are to be treated as rays.
        expr: expr.clone_without_node()
    });

    let display_arms = display.get("display_arms").maybe_unset(true);

    display.finish(&["display", "label", "display_label", "line_type"], context);

    node.insert_data("display_arms", AssociatedData::Bool(display_arms));

    expr.node = Some(node);
    expr
}

#[derive(Debug)]
pub struct BisectorNode;

impl BuildAssociated<LineNode> for BisectorNode {
    fn build_associated(&self, compiler: &mut Compiler, figure: &mut Figure, associated: &HierarchyNode<LineNode>) {
        let display_arms = associated.get_data("display_arms").unwrap().as_bool();
    }
}

/// bisector(point, point) - bisector of a segment.
pub fn point_point(
    a: Expr<Point>,
    b: Expr<Point>,
    context: &mut CompileContext,
    display: Properties,
) -> Expr<Line> {
    use super::mid::function_point;
    use super::perpendicular::line_point;

    let expr = call!(context:line_point(
        context.line(a.clone_without_node(), b.clone_without_node()),
        call!(context:function_point(vec![a, b]))
    ) with display);

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
                    |mut col: Expr<PointCollection>, context, display| call!(context:point_point_point(
                        index!(node col, 0),
                        index!(node col, 1),
                        index!(node col, 2)
                    ) with display)
                }),
                overload!((POINT, POINT, POINT) -> LINE : point_point_point),
                overload!((2-P) -> LINE {
                    |mut col: Expr<PointCollection>, context, display| call!(context:point_point(
                        index!(node col, 0),
                        index!(node col, 1)
                    ) with display)
                }),
                overload!((POINT, POINT) -> LINE : point_point),
            ],
        },
    );
}
