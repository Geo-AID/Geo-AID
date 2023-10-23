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
use std::rc::Rc;

use crate::script::builtins::macros::{distance, field, line2};
use crate::script::unroll::{Bundle, Circle, Line, Point, PointCollection, Simplify, UnrolledRule, UnrolledRuleKind};
use crate::script::{
    builtins::macros::{angle_expr, index, math, number, rule, circle_center, circle_radius},
    unroll::{CompileContext, Expr, Library, Properties, Rule},
};

fn pt_lies_on_circle(
    lhs: &Expr<Point>,
    rhs: &Expr<Circle>,
    context: &mut CompileContext,
    properties: Option<Properties>,
    invert: bool
) {
    drop(properties);

    let point = lhs.simplify(context);
    let circle = rhs.simplify(context);

    if invert {
        rule!(context:S=(
            circle_radius!(circle),
            distance!(PP: point, circle_center!(circle))
        ) neg=true);
    } else {
        context.point_on_circle(&point, &circle);
    }
}

fn pt_lies_on_line(
    lhs: &Expr<Point>,
    rhs: &Expr<Line>,
    context: &mut CompileContext,
    properties: Option<Properties>,
    invert: bool
) {
    drop(properties);

    let point = lhs.simplify(context);
    let line = rhs.simplify(context);

    if invert {
        rule!(context:S=(
            number!(=0.0),
            distance!(PL: point, line)
        ) neg=true);
    } else {
        context.point_on_line(&point, &line);
    }
}

fn col_lies_on_circle(
    lhs: &Expr<PointCollection>,
    rhs: &Expr<Circle>,
    context: &mut CompileContext,
    properties: Option<Properties>,
    invert: bool
) {
    drop(properties);
    let len = lhs.data.length;

    for i in 0..len {
        rule!(context:pt_lies_on_circle(index!(lhs, i), rhs) neg=invert);
    }

    /*
     * For a collection of A_1, A_2, ... A_n, we check if we have the same clockwise-ness for triples:
     * (A_1, A_2, A_3), (A_2, A_3, A_4), ... (A_n-1, A_n, A_1)
     */

    if !invert {
        for i in 1..len {
            let i_plus_1 = (i + 1) % len;
            let i_plus_2 = (i + 2) % len;

            rule!(context:>(
                math!(
                    *,
                    angle_expr!(dir index!(lhs, i), index!(lhs, i-1), index!(lhs, i_plus_1)),
                    angle_expr!(dir index!(lhs, i_plus_1), index!(lhs, i), index!(lhs, i_plus_2))
                ),
                number!(ANGLE 0.0)
            ));
        }
    }
}

fn pt_lies_on_segment(
    lhs: &Expr<Point>,
    rhs: &Expr<Bundle>,
    context: &mut CompileContext,
    properties: Option<Properties>,
    invert: bool
) {
    drop(properties);

    let point = lhs.simplify(context);
    let line = line2!(field!(POINT rhs, A), field!(POINT rhs, B)).simplify(context);

    if invert {
        // not on line or not between A, B
        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Alternative(vec![
                UnrolledRule {
                    kind: UnrolledRuleKind::ScalarEq(
                        math!(
                            +, distance!(PP: field!(POINT rhs, A), lhs),
                            distance!(PP: field!(POINT rhs, B), lhs)
                        ),
                        distance!(PP: field!(POINT rhs, A), field!(POINT rhs, B))
                    ),
                    inverted: true
                },
                UnrolledRule {
                    kind: UnrolledRuleKind::ScalarEq(
                        math!(
                            +, distance!(PP: field!(POINT rhs, A), lhs),
                            distance!(PP: field!(POINT rhs, B), lhs)
                        ),
                        distance!(PP: field!(POINT rhs, A), field!(POINT rhs, B))
                    ),
                    inverted: true
                }
            ]),
            inverted: false
        });
    } else {
        context.point_on_line(&point, &line);

        rule!(context:S=(
            math!(
                +, distance!(PP: field!(POINT rhs, A), lhs),
                distance!(PP: field!(POINT rhs, B), lhs)
            ),
            distance!(PP: field!(POINT rhs, A), field!(POINT rhs, B))
        ));
    }
}

pub fn register(library: &mut Library) {
    library.rule_ops.insert(
        String::from("lies_on"),
        Rc::new(Rule {
            name: String::from("lies_on"),
            overloads: vec![
                overload!(POINT lies_on CIRCLE : pt_lies_on_circle),
                overload!(POINT lies_on LINE : pt_lies_on_line),
                overload!(0-P lies_on CIRCLE : col_lies_on_circle),
                overload!(POINT lies_on Segment : pt_lies_on_segment),
            ],
        }),
    );
}
