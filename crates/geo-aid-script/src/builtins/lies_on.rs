//! The `lies_on` rule

use geo_aid_derive::overload;
use num_traits::{One, Zero};
use std::rc::Rc;

use super::prelude::*;
use crate::token::number::ProcNum;

/// `point lies_on circle` - a point lies on a circle.
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

    let point = lhs;
    let circle = rhs;

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

/// `point lies_on line` - a point lies on a line.
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

    let point = lhs;
    let line = rhs;

    if inverted {
        context.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::ScalarEq(
                number!(=ProcNum::zero()),
                context.distance_pl(point, line),
            ),
            inverted: true,
            weight,
        });
    } else {
        context.point_on_line(&point, &line, weight);
    }

    node
}

/// `pc lies_on circle` - a point collection lies on a circle.
#[allow(clippy::needless_pass_by_value)]
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
            weight.clone(),
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
                kind: UnrolledRuleKind::Gt(
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
                    number!(ANGLE ProcNum::zero()),
                ),
                inverted: false,
                weight: weight.clone(),
            });
        }
    }

    node
}

/// `point lies_on segment` - a point lies on a segment (on the containing line, between the delimiting points)
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

    let point = lhs;
    let line = context.line(
        field!(node POINT rhs, A with context),
        field!(node POINT rhs, B with context),
    );

    if inverted {
        // not on the line or not between A, B
        context.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::Alternative(vec![
                UnrolledRule {
                    kind: UnrolledRuleKind::ScalarEq(
                        number!(=ProcNum::zero()),
                        context.distance_pl(point.clone_without_node(), line),
                    ),
                    inverted: true,
                    weight: ProcNum::one(),
                },
                UnrolledRule {
                    kind: UnrolledRuleKind::ScalarEq(
                        context.add(
                            context.distance_pp(
                                field!(no-node POINT rhs, A with context),
                                point.clone_without_node(),
                            ),
                            context.distance_pp(field!(no-node POINT rhs, B with context), point),
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
        context.point_on_line(&point, &line, weight.clone());

        context.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::ScalarEq(
                context.add(
                    context.distance_pp(
                        field!(no-node POINT rhs, A with context),
                        point.clone_without_node(),
                    ),
                    context.distance_pp(field!(no-node POINT rhs, B with context), point),
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

/// Register the rule
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
