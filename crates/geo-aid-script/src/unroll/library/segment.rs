//! The `Segment` type and function

use std::fmt::Display;

use crate::{
    figure::SegmentItem,
    math::Build,
    parser::PropertyValue,
    span,
    token::StrLit,
    unroll::{
        figure::{MaybeUnset, Node},
        Dummy,
    },
};

use super::prelude::*;
use crate::token::Span;
use geo_aid_figure::math_string::MathString;

/// A segment with two delimiting points.
#[derive(Debug)]
pub struct Segment {
    pub a: Expr<Point>,
    pub b: Expr<Point>,
}

impl DerivedType for Segment {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Semgent({}, {})", self.a, self.b)
    }
}

impl_derived! {Segment}

#[derive(Debug)]
struct SegmentNode {
    /// Whether the node should be displayed
    display: MaybeUnset<bool>,
    /// Whether tge segment itself should be displayed
    display_segment: MaybeUnset<bool>,
    /// How the segment should be drawn.
    style: MaybeUnset<Style>,
    /// Expression of one endpoint
    a: Expr<Point>,
    /// Expression of the other endpoint
    b: Expr<Point>,
}

impl Dummy for SegmentNode {
    fn dummy() -> Self {
        Self {
            display: MaybeUnset::new(true),
            display_segment: MaybeUnset::new(true),
            style: MaybeUnset::new(Style::Solid),
            a: Expr::dummy(),
            b: Expr::dummy(),
        }
    }

    fn is_dummy(&self) -> bool {
        self.a.is_dummy() || self.b.is_dummy()
    }
}

impl Node for SegmentNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, build: &mut Build) {
        if self.display.unwrap() && !self.is_dummy() && self.display_segment.unwrap() {
            let p_id = build.load(&self.a);
            let q_id = build.load(&self.b);

            build.add(SegmentItem {
                p_id,
                q_id,
                label: MathString::new(),
                style: self.style.get_copied(),
            });
        }
    }
}

/// `Segment(point, point)` - a segment connecting two points.
fn segment_function_point_point(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    context: &CompileContext,
    mut display: Properties,
) -> SegmentExpr {
    let node = SegmentNode {
        display: display.get("display").maybe_unset(true),
        display_segment: display.get("display_segment").maybe_unset(true),
        style: display.get("style").maybe_unset(Style::default()),
        a: a.clone_without_node(),
        b: b.clone_without_node(),
    };

    display.ignore("default-label");
    display.finish(context);

    let mut node = HierarchyNode::new_dyn(node);
    node.extend_children(a.take_node());
    node.extend_children(b.take_node());

    SegmentExpr::new(Segment { a, b }, node)
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

/// The length of a segment.
fn len(segment: SegmentExpr, context: &mut CompileContext, mut display: Properties) -> Distance {
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

    let segment = segment.get();

    if let Some(segment) = segment {
        super::dst::distance_function_pp(
            segment.a.clone_without_node(),
            segment.b.clone_without_node(),
            context,
            display,
        )
    } else {
        Distance::dummy()
    }
}

/// Register the type and the function
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("segment")
                .overload(|mut col: Pc<2>, context: &CompileContext, display| {
                    segment_function_point_point(
                        index!(node col,0),
                        index!(node col,1),
                        context,
                        display,
                    )
                })
                .overload(segment_function_point_point),
        )
        .add(Function::new("[segment]::len").overload(len));
}
