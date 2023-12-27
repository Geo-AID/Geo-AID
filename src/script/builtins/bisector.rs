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
use crate::script::{
    compile::{Compile, Compiler},
    figure::{Figure, Style},
    unroll::{BuildAssociated, CloneWithNode, HierarchyNode, LineNode, LineType},
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
    let display_arms = display.get("display_arms").maybe_unset(true);
    let arms_style = display.get("arms_style").maybe_unset(Style::default());
    let line_type = display.get("line_type").maybe_unset(LineType::Ray);
    let arms_type = display.get("arms_type").maybe_unset(LineType::Segment);

    let mut expr = context.bisector_ppp_display(a, b, c, display);

    if let Some(node) = &mut expr.node {
        node.insert_data("display_arms", display_arms);
        node.insert_data("arms_style", arms_style);
        node.insert_data("line_type", line_type);
        node.insert_data("arms_type", arms_type);

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

impl BuildAssociated<LineNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        compiler: &mut Compiler,
        figure: &mut Figure,
        associated: &mut HierarchyNode<LineNode>,
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

        let mut line_type = associated
            .get_data("line_type")
            .unwrap()
            .as_line_type()
            .unwrap();

        let arms_type = associated
            .get_data("arms_type")
            .unwrap()
            .as_line_type()
            .unwrap()
            .unwrap();

        // The old value takes advantage only if it has been manually set and the `line_type` prop has not.
        line_type.try_set_if_unset(associated.root.line_type.try_get().copied());
        associated.root.line_type.set(line_type.unwrap());

        if display_arms {
            match associated.root.expr.data.as_ref() {
                Line::AngleBisector(a_expr, b_expr, c_expr) => {
                    let a = compiler.compile(a_expr);
                    let b = compiler.compile(b_expr);
                    let c = compiler.compile(c_expr);

                    match arms_type {
                        LineType::Line => {
                            let line_a = Expr::new_spanless(Line::LineFromPoints(
                                b_expr.clone_without_node(),
                                a_expr.clone_without_node(),
                            ));
                            let line_c = Expr::new_spanless(Line::LineFromPoints(
                                b_expr.clone_without_node(),
                                c_expr.clone_without_node(),
                            ));

                            figure.lines.push((compiler.compile(&line_a), arms_style));
                            figure.lines.push((compiler.compile(&line_c), arms_style));
                        }
                        LineType::Ray => {
                            figure.rays.push((b.clone(), a, arms_style));
                            figure.rays.push((b, c, arms_style));
                        }
                        LineType::Segment => {
                            figure.segments.push((b.clone(), a, arms_style));
                            figure.segments.push((b, c, arms_style));
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

/// bisector(point, point) - bisector of a segment.
pub fn point_point(
    a: Expr<Point>,
    b: Expr<Point>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Line> {
    use super::mid::function_point;
    use super::perpendicular::line_point;

    call!(context:line_point(
        context.line(a.clone_without_node(), b.clone_without_node()),
        call!(context:function_point(vec![a, b]))
    ) with display)
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
