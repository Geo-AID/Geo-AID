//! Triangle-related functions

use num_traits::FromPrimitive;

use crate::{
    token::{number::ProcNum, Span},
    unroll::{figure::PCNode, PointCollection, PointCollectionData},
};

use super::{bisector, prelude::*};

fn triangle(context: &CompileContext, mut props: Properties) -> Pc<3> {
    let points = (0..3).map(|_| context.free_point()).collect::<Vec<_>>();

    let mut expr = Expr {
        data: Rc::new(PointCollection {
            length: 3,
            data: PointCollectionData::PointCollection(points.into()),
        }),
        span: Span::empty(),
        node: None,
    };

    let node = PCNode {
        display: props.get("display").maybe_unset(true),
        children: (0..3).map(|_| None).collect(),
        props: None,
        expr: expr.clone_without_node(),
    };
    let mut node = HierarchyNode::new(node);
    node.set_associated(super::polygon::Associated);
    node.insert_data(
        "display_segments",
        props.get("displaysegments").maybe_unset(true),
    );
    node.insert_data("style", props.get("style").maybe_unset(Style::Solid));
    node.root.props = Some(props);
    expr.node = Some(node);

    expr.into()
}

fn main_triangle(context: &mut CompileContext, props: Properties) -> Pc<3> {
    let pc = triangle(context, props);

    let a_y = context.point_y(pc.index_without_node(0));
    let a_x = context.point_x(pc.index_without_node(0));
    let b_y = context.point_y(pc.index_without_node(1));
    let b_x = context.point_x(pc.index_without_node(1));
    let c_y = context.point_y(pc.index_without_node(2));
    context.scalar_eq(a_y, b_y.clone_without_node(), false);
    context.gt(c_y, b_y, false);
    context.gt(b_x, a_x, false);

    pc
}

fn isosceles_triangle(context: &mut CompileContext, props: Properties) -> Pc<3> {
    let pc = triangle(context, props);

    let ac = context.distance_pp(pc.index_without_node(0), pc.index_without_node(2));
    let bc = context.distance_pp(pc.index_without_node(1), pc.index_without_node(2));
    context.scalar_eq(ac, bc, false);

    pc
}

fn main_isosceles_triangle(context: &mut CompileContext, props: Properties) -> Pc<3> {
    let pc = isosceles_triangle(context, props);

    let a_y = context.point_y(pc.index_without_node(0));
    let a_x = context.point_x(pc.index_without_node(0));
    let b_y = context.point_y(pc.index_without_node(1));
    let b_x = context.point_x(pc.index_without_node(1));
    let c_y = context.point_y(pc.index_without_node(2));
    context.scalar_eq(a_y, b_y.clone_without_node(), false);
    context.gt(c_y, b_y, false);
    context.gt(b_x, a_x, false);

    pc.into()
}

fn equilateral_triangle(context: &mut CompileContext, props: Properties) -> Pc<3> {
    let pc = triangle(context, props);

    let ac = context.distance_pp(pc.index_without_node(0), pc.index_without_node(2));
    let bc = context.distance_pp(pc.index_without_node(1), pc.index_without_node(2));
    let ab = context.distance_pp(pc.index_without_node(0), pc.index_without_node(1));
    context.scalar_eq(ac.clone_without_node(), bc, false);
    context.scalar_eq(ac, ab, false);

    pc
}

fn main_equilateral_triangle(context: &mut CompileContext, props: Properties) -> Pc<3> {
    let pc = equilateral_triangle(context, props);

    let a_y = context.point_y(pc.index_without_node(0));
    let a_x = context.point_x(pc.index_without_node(0));
    let b_y = context.point_y(pc.index_without_node(1));
    let b_x = context.point_x(pc.index_without_node(1));
    let c_y = context.point_y(pc.index_without_node(2));
    context.scalar_eq(a_y, b_y.clone_without_node(), false);
    context.gt(c_y, b_y, false);
    context.gt(b_x, a_x, false);

    pc.into()
}

fn right_triangle(context: &mut CompileContext, props: Properties) -> Pc<3> {
    let pc = triangle(context, props);

    let acb = context.angle_ppp(
        pc.index_without_node(0),
        pc.index_without_node(2),
        pc.index_without_node(1),
    );
    context.scalar_eq(
        acb,
        number!(ANGLE ProcNum::pi() / &ProcNum::from_i8(2).unwrap()),
        false,
    );

    pc
}

fn main_right_triangle(context: &mut CompileContext, props: Properties) -> Pc<3> {
    let pc = right_triangle(context, props);

    let a_y = context.point_y(pc.index_without_node(0));
    let a_x = context.point_x(pc.index_without_node(0));
    let b_y = context.point_y(pc.index_without_node(1));
    let c_y = context.point_y(pc.index_without_node(2));
    let c_x = context.point_x(pc.index_without_node(2));
    context.scalar_eq(a_y, c_y.clone_without_node(), false);
    context.gt(b_y, c_y, false);
    context.gt(a_x, c_x, false);

    pc.into()
}

fn orthocenter(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    mut c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Expr<Point> {
    let a_node = a.take_node();
    let b_node = b.take_node();
    let c_node = c.take_node();
    let mut alt1 = context.perpendicular_through(
        context.line(a.clone_without_node(), b.clone_without_node()),
        c.clone_without_node(),
    );
    let mut alt2 = context.perpendicular_through(context.line(b, c), a);

    // Make sure the altitudes are not displayed.
    alt1.take_node();
    alt2.take_node();

    let mut expr = context.intersection_display(alt1, alt2, props);

    if let Some(node) = expr.node.as_mut() {
        node.extend_children([a_node, b_node, c_node].into_iter().flatten());
    }

    expr
}

fn circumcenter(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    mut c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Expr<Point> {
    let a_node = a.take_node();
    let b_node = b.take_node();
    let c_node = c.take_node();
    let mut bis1 = bisector::point_point(
        a.clone_without_node(),
        b.clone_without_node(),
        context,
        Properties::default(),
    );
    let mut bis2 = bisector::point_point(c, b, context, Properties::default());

    // Make sure the altitudes are not displayed.
    bis1.take_node();
    bis2.take_node();

    let mut expr = context.intersection_display(bis1, bis2, props);

    if let Some(node) = expr.node.as_mut() {
        node.extend_children([a_node, b_node, c_node].into_iter().flatten());
    }

    expr
}

fn incenter(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    mut c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Expr<Point> {
    let a_node = a.take_node();
    let b_node = b.take_node();
    let c_node = c.take_node();
    let mut bis1 = bisector::point_point_point(
        a.clone_without_node(),
        b.clone_without_node(),
        c.clone_without_node(),
        context,
        Properties::default(),
    );
    let mut bis2 = bisector::point_point_point(a, c, b, context, Properties::default());

    // Make sure the altitudes are not displayed.
    bis1.take_node();
    bis2.take_node();

    let mut expr = context.intersection_display(bis1, bis2, props);

    if let Some(node) = expr.node.as_mut() {
        node.extend_children([a_node, b_node, c_node].into_iter().flatten());
    }

    expr
}

/// Register the functions
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("orthocenter")
                .alias("orthocentre")
                .alias_method(ty::collection(3), "orthocenter")
                .alias_method(ty::collection(3), "orthocentre")
                .overload(orthocenter)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    orthocenter(
                        col.index_with_node(0),
                        col.index_with_node(1),
                        col.index_with_node(2),
                        context,
                        props,
                    )
                }),
        )
        .add(
            Function::new("circumcenter")
                .alias("circumcentre")
                .alias_method(ty::collection(3), "circumcenter")
                .alias_method(ty::collection(3), "circumcentre")
                .overload(circumcenter)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    circumcenter(
                        col.index_with_node(0),
                        col.index_with_node(1),
                        col.index_with_node(2),
                        context,
                        props,
                    )
                }),
        )
        .add(
            Function::new("incenter")
                .alias("incentre")
                .alias_method(ty::collection(3), "incenter")
                .alias_method(ty::collection(3), "incentre")
                .overload(incenter)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    incenter(
                        col.index_with_node(0),
                        col.index_with_node(1),
                        col.index_with_node(2),
                        context,
                        props,
                    )
                }),
        )
        .add(Function::new("triangle").overload(triangle))
        .add(Function::new("maintriangle").overload(main_triangle))
        .add(
            Function::new("isoscelestriangle")
                .alias("isosceles")
                .overload(isosceles_triangle),
        )
        .add(
            Function::new("mainisoscelestriangle")
                .alias("mainisosceles")
                .overload(main_isosceles_triangle),
        )
        .add(
            Function::new("equilateraltriangle")
                .alias("equilateral")
                .overload(equilateral_triangle),
        )
        .add(
            Function::new("mainequilateraltriangle")
                .alias("mainequilateral")
                .overload(main_equilateral_triangle),
        )
        .add(
            Function::new("righttriangle")
                .alias("right")
                .overload(right_triangle),
        )
        .add(
            Function::new("mainrighttriangle")
                .alias("mainright")
                .overload(main_right_triangle),
        );
}
