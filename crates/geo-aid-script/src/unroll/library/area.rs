//! Functions related to calculating an area.

use std::mem;

use num_traits::FromPrimitive;

use crate::{
    parser::Type,
    token::number::ProcNum,
    unroll::{AnyExpr, Convert, PointCollection},
};

use super::{complex::abs, prelude::*, Overload};

fn area(
    a: Expr<Point>,
    b: Expr<Point>,
    c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Area {
    let sin_angle = context.sin(context.angle_ppp(
        a.clone_without_node(),
        b.clone_without_node(),
        c.clone_without_node(),
    ));
    let ba = context.distance_pp(b.clone_without_node(), a);
    let bc = context.distance_pp(b, c);

    context
        .mult_display(
            context.mult(
                sin_angle,
                number!(SCALAR ProcNum::from_i64(1).unwrap() / &ProcNum::from_i64(2).unwrap()),
            ),
            context.mult(ba, bc),
            props,
        )
        .into()
}

fn signed_area(
    a: Expr<Point>,
    b: Expr<Point>,
    c: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Area {
    let sin_angle = context.sin(context.angle_dir(
        a.clone_without_node(),
        b.clone_without_node(),
        c.clone_without_node(),
    ));
    let ba = context.distance_pp(b.clone_without_node(), a);
    let bc = context.distance_pp(b, c);

    context
        .mult_display(
            context.mult(
                sin_angle,
                number!(SCALAR ProcNum::from_i64(1).unwrap() / &ProcNum::from_i64(2).unwrap()),
            ),
            context.mult(ba, bc),
            props,
        )
        .into()
}

#[derive(Debug)]
struct VariadicSignedArea;

impl Overload for VariadicSignedArea {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        if params.len() >= 3 && params.iter().all(|e| e.can_convert_to(Type::Point))
            || params.len() == 1
                && params[0]
                    .can_convert_to_collection(0)
                    .is_some_and(|v| v == 0 || v >= 3)
        {
            return Some(Type::Number(Some(unit::DISTANCE * &unit::DISTANCE)));
        }

        None
    }

    fn unroll(
        &self,
        mut params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        if params.len() == 1 {
            let mut pc: Expr<PointCollection> = params.swap_remove(0).convert(context);
            let mut points = Vec::new();

            for i in 0..pc.data.length {
                points.push(pc.index_with_node(i));
            }

            variadic_signed_area(points, context, props).into()
        } else {
            let points = params.into_iter().map(|e| e.convert(context)).collect();

            variadic_signed_area(points, context, props).into()
        }
    }
}

fn variadic_signed_area(
    mut points: Vec<Expr<Point>>,
    context: &CompileContext,
    mut props: Properties,
) -> Area {
    let mut first = points[0].clone_with_node();
    (2..points.len())
        .map(|i| {
            (
                signed_area(
                    first.clone_with_node(),
                    points[i - 1].clone_without_node(),
                    points[i].clone_without_node(),
                    context,
                    Properties::default(),
                )
                .0,
                i,
            )
        })
        .reduce(|(acc, _), e| {
            (
                if e.1 == points.len() - 1 {
                    context.add_display(acc, e.0, mem::take(&mut props))
                } else {
                    context.add(acc, e.0)
                },
                0,
            )
        })
        .unwrap()
        .0
        .into()
}

#[derive(Debug)]
struct VariadicArea;

impl Overload for VariadicArea {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        VariadicSignedArea.get_returned_type(params)
    }

    fn unroll(
        &self,
        params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let num = VariadicSignedArea
            .unroll(params, context, Properties::default())
            .to_scalar()
            .unwrap();
        abs(num, context, props).into()
    }
}

/// Register the related functions
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("area")
                .alias_method(Type::PointCollection(0), "area")
                .overload(area)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    area(
                        col.index_with_node(0),
                        col.index_without_node(1),
                        col.index_without_node(2),
                        context,
                        props,
                    )
                })
                .overload(VariadicArea),
        )
        .add(
            Function::new("signedarea")
                .alias_method(Type::PointCollection(0), "signedarea")
                .overload(area)
                .overload(|mut col: Pc<3>, context: &CompileContext, props| {
                    signed_area(
                        col.index_with_node(0),
                        col.index_without_node(1),
                        col.index_without_node(2),
                        context,
                        props,
                    )
                })
                .overload(VariadicSignedArea),
        );
}
