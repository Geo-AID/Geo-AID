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
use num_traits::One;
use std::rc::Rc;

use super::prelude::*;
use crate::script::token::number::ProcNum;

fn pt_lies_on_circle(
    mut lhs: Expr<Point>,
    mut rhs: Expr<Circle>,
    context: &mut CompileContext,
    display: Properties,
    inverted: bool,
    weight: ProcNum,
) -> CollectionNode {
    let mut node = CollectionNode::from_display(display, context);
    node.extend(lhs.node.take());
    node.extend(rhs.node.take());

    let point = lhs.simplify(context);
    let circle = rhs.simplify(context);

    if inverted {
        context.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::ScalarEq(
                context.circle_radius(circle.clone_without_node()),
                context.distance_pp(point, context.circle_center(circle)),
            ),
            inverted: true,
            weight,
        });
    } else {
        context.point_on_circle(&point, &circle, weight);
    }

    node
}

fn pt_lies_on_line(
    mut lhs: Expr<Point>,
    mut rhs: Expr<Line>,
    context: &mut CompileContext,
    display: Properties,
    inverted: bool,
    weight: ProcNum,
) -> CollectionNode {
    let mut node = CollectionNode::from_display(display, context);
    node.extend(lhs.node.take());
    node.extend(rhs.node.take());

    let point = lhs.simplify(context);
    let line = rhs.simplify(context);

    if inverted {
        context.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::ScalarEq(number!(=0.0), context.distance_pl(point, line)),
            inverted: true,
            weight,
        });
    } else {
        context.point_on_line(&point, &line, weight);
    }

    node
}

fn col_lies_on_circle(
    mut lhs: Expr<PointCollection>,
    mut rhs: Expr<Circle>,
    context: &mut CompileContext,
    display: Properties,
    inverted: bool,
    weight: ProcNum,
) -> CollectionNode {
    let len = lhs.data.length;

    let mut node = CollectionNode::from_display(display, context);

    for i in 0..len {
        node.push(pt_lies_on_circle(
            index!(node lhs, i),
            rhs.clone_with_node(),
            context,
            Properties::default(),
            inverted,
            weight,
        ));
    }

    /*
     * For a collection of A_1, A_2, ... A_n, we check if we have the same clockwise-ness for triples:
     * (A_1, A_2, A_3), (A_2, A_3, A_4), ... (A_n-1, A_n, A_1)
     */

    if !inverted {
        for i in 1..len {
            let i_plus_1 = (i + 1) % len;
            let i_plus_2 = (i + 2) % len;

            context.push_rule(UnrolledRule {
                kind: UnrolledRuleKind::ScalarEq(
                    context.mult(
                        context.angle_dir(
                            index!(no-node lhs, i),
                            index!(no-node lhs, i-1),
                            index!(no-node lhs, i_plus_1),
                        ),
                        context.angle_dir(
                            index!(no-node lhs, i_plus_1),
                            index!(no-node lhs, i),
                            index!(no-node lhs, i_plus_2),
                        ),
                    ),
                    number!(ANGLE 0.0),
                ),
                inverted: false,
                weight,
            });
        }
    }

    node
}

fn pt_lies_on_segment(
    mut lhs: Expr<Point>,
    mut rhs: Expr<Bundle>,
    context: &mut CompileContext,
    display: Properties,
    inverted: bool,
    weight: ProcNum,
) -> CollectionNode {
    let mut node = CollectionNode::from_display(display, context);
    node.extend(lhs.node.take());
    node.extend(rhs.node.take());

    let point = lhs.simplify(context);
    let line = context
        .line(
            field!(node POINT rhs, A with context),
            field!(node POINT rhs, B with context),
        )
        .simplify(context);

    if inverted {
        // not on line or not between A, B
        context.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::Alternative(vec![
                UnrolledRule {
                    kind: UnrolledRuleKind::ScalarEq(
                        number!(=0.0),
                        context.distance_pl(point, line),
                    ),
                    inverted: true,
                    weight: ProcNum::one(),
                },
                UnrolledRule {
                    kind: UnrolledRuleKind::ScalarEq(
                        context.add(
                            context.distance_pp(
                                field!(no-node POINT rhs, A with context),
                                lhs.clone_without_node(),
                            ),
                            context.distance_pp(field!(no-node POINT rhs, B with context), lhs),
                        ),
                        context.distance_pp(
                            field!(no-node POINT rhs, A with context),
                            field!(no-node POINT rhs, B with context),
                        ),
                    ),
                    inverted: true,
                    weight: ProcNum::one(),
                },
            ]),
            inverted: false,
            weight,
        });
    } else {
        context.point_on_line(&point, &line, weight);

        context.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::ScalarEq(
                context.add(
                    context.distance_pp(
                        field!(no-node POINT rhs, A with context),
                        lhs.clone_without_node(),
                    ),
                    context.distance_pp(field!(no-node POINT rhs, B with context), lhs),
                ),
                context.distance_pp(
                    field!(no-node POINT rhs, A with context),
                    field!(no-node POINT rhs, B with context),
                ),
            ),
            inverted: false,
            weight,
        });
    }

    node
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
