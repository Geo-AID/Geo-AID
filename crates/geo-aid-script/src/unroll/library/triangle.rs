//! Triangle-related functions

use super::prelude::*;

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

/// Register the functions
pub fn register(library: &mut Library) {
    library.add(
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
    );
}
