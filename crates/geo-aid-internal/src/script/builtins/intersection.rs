use crate::script::math::Build;

use super::prelude::*;
use geo_aid_derive::overload;

fn intersection_function_line_line(
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

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("intersection"),
        Function {
            overloads: vec![overload!((LINE, LINE) -> POINT : intersection_function_line_line)],
        },
    );
}
