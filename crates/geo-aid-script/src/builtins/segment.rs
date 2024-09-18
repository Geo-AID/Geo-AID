//! The `Segment` type and function

use crate::{figure::SegmentItem, math::Build, parser::PropertyValue, span, token::StrLit};

use super::prelude::*;
use crate::token::Span;
use geo_aid_derive::overload;
use geo_aid_figure::math_string::MathString;

/// `Segment(point, point)` - a segment connecting two points.
fn segment_function_point_point(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    context: &CompileContext,
    mut display: Properties,
) -> Expr<Bundle> {
    let mut expr = construct_bundle!(Segment { A: a, B: b });

    if let Some(node) = &mut expr.node {
        display.ignore("default-label");
        node.root.display = display.get("display").maybe_unset(true);

        let display_segment = display.get("display_segment").maybe_unset(true);
        let style = display.get("style").maybe_unset(Style::default());

        node.insert_data("display_segment", display_segment);
        node.insert_data("style", style);
        node.set_associated(Associated);
    }

    display.finish(context);

    expr
}

/// ```
/// # use geo_aid_figure::Style;
/// struct Associated {
///     display_segment: bool,
///     style: Style
/// }
/// ```
#[derive(Debug)]
pub struct Associated;

impl BuildAssociated<BundleNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        build: &mut Build,
        associated: &mut HierarchyNode<BundleNode>,
    ) {
        let display_segment = associated
            .get_data("display_segment")
            .unwrap()
            .as_bool()
            .unwrap();
        let style = associated.get_data("style").unwrap().as_style().unwrap();

        if display_segment.unwrap() {
            let p_id = build.load(associated.root.children["A"].as_point().unwrap());
            let q_id = build.load(associated.root.children["B"].as_point().unwrap());
            build.add(SegmentItem {
                p_id,
                q_id,
                label: MathString::new(),
                style: style.unwrap(),
            });
        }
    }
}

/// The length of a segment
#[allow(clippy::needless_pass_by_value)]
fn len(segment: Expr<Bundle>, context: &CompileContext, mut display: Properties) -> Expr<Scalar> {
    display.add_if_not_present(
        "display_segment",
        (
            Span::empty(),
            PropertyValue::String(StrLit {
                span: span!(0, 0, 0, 0),
                content: String::from("false"),
            }),
        ),
    );

    super::dst::distance_function_pp(
        field!(no-node POINT segment, A with context),
        field!(no-node POINT segment, B with context),
        context,
        display,
    )
}

/// Register the type and the function
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Segment"),
        Function {
            overloads: vec![
                overload!((2-P) -> Segment {
                    |mut col: Expr<PointCollection>, context, display| call!(context:segment_function_point_point(
                        index!(node col, 0),
                        index!(node col, 1)
                    ) with display)
                }),
                overload!((POINT, POINT) -> Segment : segment_function_point_point),
            ],
        },
    );

    library.functions.insert(
        String::from("[Segment]::len"),
        Function {
            overloads: vec![overload!((Segment) -> Scalar : len)],
        },
    );

    library.bundles.insert("Segment", ["A", "B"].into());
}