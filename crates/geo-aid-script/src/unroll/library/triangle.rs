//! Triangle-related functions

use super::{bisector, prelude::*};

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
        );
}
