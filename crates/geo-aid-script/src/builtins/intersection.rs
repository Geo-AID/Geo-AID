//! The `intersection` function

use crate::math::Build;

use super::prelude::*;

/// `intesection(line, line)` - intersection of two lines.
fn intersection_function_ll(
    k: Expr<Line>,
    l: Expr<Line>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Point> {
    // println!("{display:#?}");
    let mut expr = context.intersection_display(k, l, display);

    if let Some(node) = &mut expr.node {
        node.set_associated(Associated);
    }

    expr
}

/// The associated data. No properties.
#[derive(Debug)]
pub struct Associated;

impl BuildAssociated<PointNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        _build: &mut Build,
        associated: &mut HierarchyNode<PointNode>,
    ) {
        associated.root.display_dot.set_if_unset(false);
    }
}

/// Register the function
pub fn register(library: &mut Library) {
    library.add(Function::new("intersection").overload(intersection_function_ll));
}
