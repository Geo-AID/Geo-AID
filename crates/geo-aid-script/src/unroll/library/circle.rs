//! Functions directly related to circles.

use crate::unroll::Number;

use super::{bisector, prelude::*, NumberUnit};

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function(
    center: Expr<Point>,
    radius: Distance,
    context: &CompileContext,
    display: Properties,
) -> Expr<Circle> {
    context.circle_display(center, radius.0, display)
}

fn simple_circle(
    mut center: Expr<Point>,
    mut radius: Expr<Number>,
    nodes: [Option<HierarchyNode<PointNode>>; 3],
    context: &CompileContext,
    props: Properties,
) -> Expr<Circle> {
    center.take_node();
    radius.take_node();

    let mut expr = context.circle_display(center, radius, props);

    if let Some(node) = expr.node.as_mut() {
        node.extend_children(nodes.into_iter().flatten());
    }

    expr
}

/// A circle circumscribed on three points.
fn circumcircle(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    mut c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Expr<Circle> {
    let a_node = a.take_node();
    let b_node = b.take_node();
    let c_node = c.take_node();

    let k = bisector::point_point(
        a.clone_without_node(),
        b.clone_without_node(),
        context,
        Properties::default(),
    );
    let l = bisector::point_point(b, c, context, Properties::default());

    let center = context.intersection(k, l);
    let radius = context.distance_pp(center.clone_without_node(), a);

    simple_circle(center, radius, [a_node, b_node, c_node], context, props)
}

/// A circle inscribed in three points.
fn incircle(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    mut c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Expr<Circle> {
    let a_node = a.take_node();
    let b_node = b.take_node();
    let c_node = c.take_node();

    let k = context.bisector_ppp(
        a.clone_without_node(),
        b.clone_without_node(),
        c.clone_without_node(),
    );
    let l = context.bisector_ppp(b.clone_without_node(), c, a.clone_without_node());
    let ab = context.line(a, b);

    let center = context.intersection(k, l);
    let radius = context.distance_pl(center.clone_without_node(), ab);

    simple_circle(center, radius, [a_node, b_node, c_node], context, props)
}

/// A circle exscribed (?) to three points.
fn excircle(
    mut a: Expr<Point>,
    mut b: Expr<Point>,
    mut c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Expr<Circle> {
    let a_node = a.take_node();
    let b_node = b.take_node();
    let c_node = c.take_node();

    let k = context.bisector_ppp(
        a.clone_without_node(),
        b.clone_without_node(),
        c.clone_without_node(),
    );
    let l = context.perpendicular_through(
        context.bisector_ppp(
            b.clone_without_node(),
            c.clone_without_node(),
            a.clone_without_node(),
        ),
        c,
    );
    let ab = context.line(a, b);

    let center = context.intersection(k, l);
    let radius = context.distance_pl(center.clone_without_node(), ab);

    simple_circle(center, radius, [a_node, b_node, c_node], context, props)
}

/// Register the function
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("circle")
                .overload(circle_function)
                .overload(
                    |radius: Distance, center: Expr<Point>, context: &CompileContext, display| {
                        circle_function(center, radius, context, display)
                    },
                )
                .overload(|context: &mut CompileContext, display| {
                    let mut center = context.free_point();
                    let mut radius = context.free_scalar();

                    center.take_node();
                    radius.take_node();

                    circle_function(
                        center,
                        NumberUnit::from(context.set_unit(radius, unit::DISTANCE)),
                        context,
                        display,
                    )
                }),
        )
        .add(
            Function::new("radius")
                .alias_method(ty::CIRCLE, "radius")
                .overload(|circle: Expr<Circle>, context: &CompileContext, props| {
                    Distance::from(context.circle_radius_display(circle, props))
                }),
        )
        .add(
            Function::new("center")
                .alias("centre")
                .alias_method(ty::CIRCLE, "center")
                .alias_method(ty::CIRCLE, "centre")
                .overload(|circle: Expr<Circle>, context: &CompileContext, props| {
                    context.circle_center_display(circle, props)
                }),
        )
        .add(
            Function::new("circumcircle")
                .alias_method(ty::collection(3), "circumcircle")
                .overload(circumcircle)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    circumcircle(
                        index!(node col, 0),
                        index!(node col, 1),
                        index!(node col, 2),
                        context,
                        props,
                    )
                }),
        )
        .add(
            Function::new("incircle")
                .alias_method(ty::collection(3), "incircle")
                .overload(incircle)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    incircle(
                        index!(node col, 0),
                        index!(node col, 1),
                        index!(node col, 2),
                        context,
                        props,
                    )
                }),
        )
        .add(
            Function::new("excircle")
                .alias_method(ty::collection(3), "excircle")
                .overload(excircle)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    excircle(
                        index!(node col, 0),
                        index!(node col, 1),
                        index!(node col, 2),
                        context,
                        props,
                    )
                }),
        );
}
