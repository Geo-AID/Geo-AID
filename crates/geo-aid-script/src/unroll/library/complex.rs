//! All functions for complex number manipulation

use num_rational::Ratio;

use crate::{
    parser::Type,
    token::number::ProcNum,
    unroll::{AnyExpr, Number},
};

use super::{prelude::*, Overload};

struct Real;

impl Overload for Real {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        if params.len() != 1 {
            return None;
        }

        params[0].can_convert_to_scalar(None).map(Type::Number)
    }

    fn unroll(
        &self,
        params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let ty = self.get_returned_type(&params).unwrap();
        context
            .real_display(
                params[0]
                    .clone_without_node()
                    .convert_to(ty, context)
                    .to_scalar()
                    .unwrap(),
                props,
            )
            .into()
    }
}

struct Imaginary;

impl Overload for Imaginary {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        if params.len() != 1 {
            return None;
        }

        params[0].can_convert_to_scalar(None).map(Type::Number)
    }

    fn unroll(
        &self,
        params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let ty = self.get_returned_type(&params).unwrap();
        context
            .imaginary_display(
                params[0]
                    .clone_without_node()
                    .convert_to(ty, context)
                    .to_scalar()
                    .unwrap(),
                props,
            )
            .into()
    }
}

struct Conjugate;

impl Overload for Conjugate {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        if params.len() != 1 {
            return None;
        }

        params[0].can_convert_to_scalar(None).map(Type::Number)
    }

    fn unroll(
        &self,
        params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let ty = self.get_returned_type(&params).unwrap();
        let v = params[0]
            .clone_without_node()
            .convert_to(ty, context)
            .to_scalar()
            .unwrap();

        let real = context.real(v.clone_without_node());
        let imaginary = context.imaginary(v.clone_without_node());
        let i_imaginary = context.mult(
            imaginary,
            Expr::new_spanless(Number {
                unit: Some(unit::SCALAR),
                data: NumberData::Number(ProcNum::i()),
            }),
        );

        context.sub_display(real, i_imaginary, props).into()
    }
}

fn vector(
    origin: Expr<Point>,
    target: Expr<Point>,
    context: &CompileContext,
    props: Properties,
) -> Distance {
    context
        .sub_display(
            context.to_complex(target),
            context.to_complex(origin),
            props,
        )
        .into()
}

/// Absolute value of a number, real or complex.
#[must_use]
pub fn abs(num: Expr<Number>, context: &CompileContext, props: Properties) -> Expr<Number> {
    let re2 = context.mult(
        context.real(num.clone_without_node()),
        context.real(num.clone_without_node()),
    );
    let im2 = context.mult(
        context.imaginary(num.clone_without_node()),
        context.imaginary(num.clone_without_node()),
    );
    let norm = context.add(re2, im2);
    context.pow_display(norm, Ratio::new(1, 2), props)
}

#[derive(Debug)]
struct Abs;

impl Overload for Abs {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        if params.len() != 1 {
            return None;
        }

        params[0].can_convert_to_scalar(None).map(Type::Number)
    }

    fn unroll(
        &self,
        mut params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let num = params
            .swap_remove(0)
            .convert_to(Type::Number(None), context)
            .to_scalar()
            .unwrap();

        abs(num, context, props).into()
    }
}

/// Register all the functions that need to be registered.
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("real")
                .alias("re")
                .alias_method(ty::SCALAR_UNKNOWN, "real")
                .alias_method(ty::SCALAR_UNKNOWN, "re")
                .overload(Real)
                .overload(|context: &CompileContext, props| {
                    Unitless::from(context.free_scalar_display(props))
                }),
        )
        .add(
            Function::new("imaginary")
                .alias("im")
                .alias_method(ty::SCALAR_UNKNOWN, "imaginary")
                .alias_method(ty::SCALAR_UNKNOWN, "im")
                .overload(Imaginary),
        )
        .add(
            Function::new("tocomplex")
                .alias_method(ty::POINT, "tocomplex")
                .overload(|point: Expr<Point>, context: &CompileContext, props| {
                    Distance::from(context.to_complex_display(point, props))
                }),
        )
        .add(
            Function::new("conjugate")
                .alias_method(ty::SCALAR_UNKNOWN, "conjugate")
                .overload(Conjugate),
        )
        .add(
            Function::new("vector")
                .alias("vec")
                .alias_method(ty::collection(2), "vector")
                .alias_method(ty::collection(2), "vec")
                .overload(vector)
                .overload(|mut col: Pc<2>, context: &CompileContext, props| {
                    vector(
                        col.index_with_node(0),
                        col.index_without_node(1),
                        context,
                        props,
                    )
                }),
        )
        .add(
            Function::new("abs")
                .alias("module")
                .alias_method(Type::Number(None), "abs")
                .alias_method(Type::Number(None), "module")
                .overload(Abs),
        );
}
