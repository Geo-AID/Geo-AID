//! The `bisector` function

use crate::math::Build;

use super::{angle::display_angle_arms, prelude::*};

/// bisector(point, point, point) - an angle's bisector.
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
/// # use geo_aid_figure::Style;
/// struct Associated {
///     display_arms: bool,
///     arms_style: Style
/// }
/// ```
#[derive(Debug)]
pub struct Associated;

impl BuildAssociated<LineNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        build: &mut Build,
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
                    display_angle_arms(build, a_expr, b_expr, c_expr, arms_type, arms_style);
                }
                _ => unreachable!(),
            }
        }
    }
}

/// bisector(point, point) - a segment's perpendicular bisector line.
pub fn point_point(
    a: Expr<Point>,
    b: Expr<Point>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Line> {
    use super::perpendicular::line_point;

    line_point(
        context.line(a.clone_without_node(), b.clone_without_node()),
        context.average_p(vec![a, b]),
        context,
        display,
    )
}

/// Register the function
pub fn register(library: &mut Library) {
    library.add(
        Function::new("bisector")
            .overload(|mut col: Pc<3>, context: &CompileContext, display| {
                point_point_point(
                    index!(node col,0),
                    index!(node col,1),
                    index!(node col,2),
                    context,
                    display,
                )
            })
            .overload(point_point_point)
            .overload(|mut col: Pc<2>, context: &mut CompileContext, display| {
                point_point(index!(node col,0), index!(node col,1), context, display)
            })
            .overload(point_point),
    );
}
