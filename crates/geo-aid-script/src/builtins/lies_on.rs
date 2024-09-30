//! The `lies_on` rule

use num_traits::{One, Zero};

use super::{prelude::*, segment::Segment};
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
fn col_lies_on_circle(
    mut lhs: Pc<0>,
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
    mut rhs: Segment,
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
    library.add(
        Rule::new("lies_on")
            .overload(pt_lies_on_circle)
            .overload(pt_lies_on_line)
            .overload(col_lies_on_circle)
            .overload(pt_lies_on_segment),
    );
}
