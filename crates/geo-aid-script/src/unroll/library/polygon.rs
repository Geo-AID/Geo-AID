//! Polygon related functions.

use crate::{
    figure::SegmentItem,
    math::Build,
    parser::Type,
    token::{number::ProcNum, Span},
    unroll::{figure::PCNode, AnyExpr, PointCollection, PointCollectionData},
};

use super::{prelude::*, Overload};

use geo_aid_figure::math_string::MathString;
use num_traits::{ToPrimitive, Zero};

fn poly(
    params: &[AnyExpr],
    context: &CompileContext,
    mut props: Properties,
) -> Expr<PointCollection> {
    let NumberData::Number(num) = &params[0].as_number().unwrap().get_data().data else {
        unreachable!()
    };

    let n = num.0.re.to_integer().to_usize().unwrap();
    let points = (0..n).map(|_| context.free_point()).collect::<Vec<_>>();

    let mut expr = Expr {
        data: Rc::new(PointCollection {
            length: n,
            data: PointCollectionData::PointCollection(points.into()),
        }),
        span: Span::empty(),
        node: None,
    };

    let node = PCNode {
        display: props.get("display").maybe_unset(true),
        children: (0..n).map(|_| None).collect(),
        props: None,
        expr: expr.clone_without_node(),
    };
    let mut node = HierarchyNode::new(node);
    node.set_associated(Associated);
    node.insert_data(
        "display_segments",
        props.get("displaysegments").maybe_unset(true),
    );
    node.insert_data("style", props.get("style").maybe_unset(Style::Solid));
    node.root.props = Some(props);
    expr.node = Some(node);

    expr
}

#[derive(Debug)]
struct Poly;

impl Overload for Poly {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        if params.len() != 1 {
            return None;
        }

        let NumberData::Number(num) = &params[0].as_number()?.get_data().data else {
            return None;
        };

        let n = if num.0.im.is_zero() && num.0.re.is_integer() {
            num.0.re.to_integer().to_usize()?
        } else {
            return None;
        };

        if n <= 2 {
            return None;
        }

        Some(Type::PointCollection(n))
    }

    fn unroll(
        &self,
        params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        AnyExpr::PointCollection(poly(&params, context, props))
    }
}

#[derive(Debug)]
struct Associated;

impl BuildAssociated<PCNode> for Associated {
    fn build_associated(
        self: Box<Self>,
        build: &mut Build,
        associated: &mut HierarchyNode<PCNode>,
    ) {
        let display_segments = associated
            .get_data("display_segments")
            .unwrap()
            .as_bool()
            .unwrap();
        let style = associated.get_data("style").unwrap().as_style().unwrap();

        if display_segments.unwrap() {
            let PointCollectionData::PointCollection(points) =
                &associated.root.expr.get_data().data
            else {
                unreachable!()
            };

            let mut pts: Vec<_> = points.iter().map(|pt| build.load(pt)).collect();
            pts.push(build.load(&points[0]));
            for i in 1..pts.len() {
                build.add(SegmentItem {
                    p_id: pts[i].clone(),
                    q_id: pts[i - 1].clone(),
                    label: MathString::new(),
                    style: style.unwrap(),
                })
            }
        }
    }
}

struct Convex;

impl Overload for Convex {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        Poly.get_returned_type(params)
    }

    fn unroll(
        &self,
        params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let pc = poly(&params, context, props);
        let len = pc.data.length;

        if len > 3 {
            // Every angle with origin in point 0 and arms defined by every pair of consecutive points should
            // have the same direction. Makes sure it's not self-intersecting.
            for i in 3..len {
                let alpha = context.angle_dir(
                    pc.index_without_node(i - 2),
                    pc.index_without_node(0),
                    pc.index_without_node(i - 1),
                );
                let beta = context.angle_dir(
                    pc.index_without_node(i - 1),
                    pc.index_without_node(0),
                    pc.index_without_node(i),
                );
                let product = context.mult(alpha, beta);
                // Technically the unit for 0 is invalid, but it's not checked later on anyway.
                context.gt(product, number!(ANGLE ProcNum::zero()), false);
            }

            // A copy of the code from collection lies on circle. Makes sure it's convex
            for i in 1..len {
                let i_plus_1 = (i + 1) % len;
                let i_plus_2 = (i + 2) % len;

                let product = context.mult(
                    context.angle_dir(
                        index!(no-node pc, i),
                        index!(no-node pc, i-1),
                        index!(no-node pc, i_plus_1),
                    ),
                    context.angle_dir(
                        index!(no-node pc, i_plus_1),
                        index!(no-node pc, i),
                        index!(no-node pc, i_plus_2),
                    ),
                );
                context.gt(product, number!(ANGLE ProcNum::zero()), false);
            }
        }

        pc.into()
    }
}

/// Register the functions
pub fn register(library: &mut Library) {
    library
        .add(Function::new("poly").alias("polygon").overload(Poly))
        .add(
            Function::new("convex")
                .alias("convexpolygon")
                .alias("convexpoly")
                .overload(Convex),
        );
}
