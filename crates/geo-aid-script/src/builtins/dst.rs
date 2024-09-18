//! The `dst` function

use super::prelude::*;
use crate::{figure::SegmentItem, math::Build, unroll::Convert};
use geo_aid_derive::overload;
use geo_aid_figure::math_string::MathString;

/// `dst(point, point)` - distance between two points.
pub fn distance_function_pp(
    a: Expr<Point>,
    b: Expr<Point>,
    context: &CompileContext,
    mut display: Properties,
) -> Expr<Scalar> {
    let display_segment = display.get("display_segment").maybe_unset(true);
    let style = display.get("style").maybe_unset(Style::Solid);

    let mut expr = context.distance_pp_display(a, b, display);

    if let Some(node) = &mut expr.node {
        node.set_associated(Associated);
        node.insert_data("display_segment", display_segment);
        node.insert_data("style", style);
    }

    expr
}

/// `dst(point, line)` - distance of a point from a line.
fn distance_function_pl(
    a: Expr<Point>,
    k: Expr<Line>,
    context: &CompileContext,
    mut display: Properties,
) -> Expr<Scalar> {
    let display_segment = display.get("display_segment").maybe_unset(true);
    let style = display.get("style").maybe_unset(Style::Dashed);

    let mut expr = context.distance_pl_display(a, k, display);

    if let Some(node) = &mut expr.node {
        node.set_associated(Associated);
        node.insert_data("display_segment", display_segment);
        node.insert_data("style", style);
    }

    expr
}

/// Convert a point collection to a distance.
fn distance_convert_pc(
    mut pc: Expr<PointCollection>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Scalar> {
    if let Some(node) = pc.node.as_mut() {
        node.root.props = Some(
            node.root
                .props
                .take()
                .unwrap_or_default()
                .merge_with(display),
        );
    }

    pc.convert(context)
}

/// ```
/// # use geo_aid_figure::Style;
/// struct Associated {
///     display_segment: bool,
///     style: Style
/// }
#[derive(Debug)]
pub struct Associated;

impl BuildAssociated<ScalarNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        build: &mut Build,
        associated: &mut HierarchyNode<ScalarNode>,
    ) {
        let display_segment = associated
            .get_data("display_segment")
            .unwrap()
            .as_bool()
            .unwrap();
        let style = associated.get_data("style").unwrap().as_style().unwrap();

        if display_segment.unwrap() {
            match &associated.root.expr.data.data {
                ScalarData::PointPointDistance(a, b) => {
                    let p_id = build.load(a);
                    let q_id = build.load(b);
                    build.add(SegmentItem {
                        p_id,
                        q_id,
                        label: MathString::new(),
                        style: style.unwrap(),
                    });
                }
                ScalarData::PointLineDistance(a, k) => {
                    // Projection
                    let b = Expr::new_spanless(Point::LineLineIntersection(
                        Expr::new_spanless(Line::PerpendicularThrough(
                            k.clone_without_node(),
                            a.clone_without_node(),
                        )),
                        k.clone_without_node(),
                    ));

                    let p_id = build.load(a);
                    let q_id = build.load(&b);
                    build.add(SegmentItem {
                        p_id,
                        q_id,
                        label: MathString::new(),
                        style: style.unwrap(),
                    });
                }
                _ => unreachable!(),
            }
        }
    }
}

/// Register the function
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("dst"),
        Function {
            overloads: vec![
                overload!((2-P) -> DISTANCE : distance_convert_pc),
                overload!((DISTANCE) -> DISTANCE {
                    |v: Expr<Scalar>, context: &CompileContext, display: Properties| {
                        display.finish(context);
                        v
                    }
                }),
                overload!((SCALAR) -> DISTANCE {
                    |v: Expr<Scalar>, context: &CompileContext, display: Properties| {
                        display.finish(context);
                        context.set_unit(v, unit::DISTANCE)
                    }
                }),
                overload!((POINT, POINT) -> DISTANCE : distance_function_pp),
                overload!((POINT, LINE) -> DISTANCE : distance_function_pl),
                overload!((LINE, POINT) -> DISTANCE {
                    |k: Expr<Line>, a: Expr<Point>, context: &CompileContext, display| call!(context:distance_function_pl(
                        a, k
                    ) with display)
                }),
            ],
        },
    );
}
