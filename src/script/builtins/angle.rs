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

use crate::script::{unroll::{BuildAssociated, ScalarNode, HierarchyNode, ScalarData}, compile::{Compiler, Compile}, figure::{Figure, Mode}};
#[allow(unused_imports)]
use crate::script::unroll::{
    CompileContext, Expr, Function, Library, Line, Point, PointCollection, Properties, Scalar,
};
use geo_aid_derive::overload;

#[allow(unused_imports)]
use super::macros::{call, index};

/// angle(point, point, point) - angle depicted by 3 points.
fn angle_function_point_point_point(
    a: Expr<Point>,
    b: Expr<Point>,
    c: Expr<Point>,
    context: &CompileContext,
    mut display: Properties,
) -> Expr<Scalar> {
    let display_arms = display.get("display_arms").maybe_unset(true);
    let arms_style = display.get("arms_style").maybe_unset(Mode::Default);

    let mut expr = context.angle_ppp_display(a, b, c, display);

    if let Some(node) = &mut expr.node {
        node.insert_data("display_arms", display_arms);
        node.insert_data("arms_style", arms_style);

        node.set_associated(Associated);
    }
    expr
}

/// ```
/// struct Associated {
///     display_arms: bool,
///     amrs_style: Style
/// }
/// ```
#[derive(Debug)]
pub struct Associated;

impl BuildAssociated<ScalarNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        compiler: &mut Compiler,
        figure: &mut Figure,
        associated: &mut HierarchyNode<ScalarNode>,
    ) {
        let display_arms = associated
            .get_data("display_arms")
            .unwrap()
            .as_bool()
            .unwrap()
            .unwrap();

        let arms_style = associated
            .get_data("arms_style")
            .unwrap()
            .as_style()
            .unwrap()
            .unwrap();

        if display_arms {
            match &associated.root.expr.data.data {
                ScalarData::ThreePointAngle(a, b, c)
                | ScalarData::ThreePointAngleDir(a, b, c) => {
                    let a = compiler.compile(a);
                    let b = compiler.compile(b);
                    let c = compiler.compile(c);

                    figure.segments.push((b.clone(), a, arms_style));
                    figure.segments.push((b, c, arms_style));
                }
                _ => unreachable!(),
            }
        }
    }
}

/// angle(line, line) - distance between a point and a line.
fn angle_function_line_line(
    k: Expr<Line>,
    l: Expr<Line>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Scalar> {
    context.angle_ll_display(k, l, display)
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("angle"),
        Function {
            name: String::from("angle"),
            overloads: vec![
                overload!((3-P) -> ANGLE {
                    |mut col: Expr<PointCollection>, context, display| call!(context:angle_function_point_point_point(
                        index!(node col, 0),
                        index!(node col, 1),
                        index!(node col, 2)
                    ) with display)
                }),
                overload!((POINT, POINT, POINT) -> ANGLE : angle_function_point_point_point),
                overload!((LINE, LINE) -> ANGLE : angle_function_line_line),
            ],
        },
    );
}
