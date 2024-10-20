//! All functions for complex number manipulation

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

/// Register all the functions that need to be registered.
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("real")
                .alias("re")
                .alias_method(ty::SCALAR_UNKNOWN, "real")
                .alias_method(ty::SCALAR_UNKNOWN, "re")
                .overload(Real)
                .overload(Imaginary),
        )
        .add(
            Function::new("to_complex")
                .alias_method(ty::POINT, "to_complex")
                .overload(|point: Expr<Point>, context: &CompileContext, props| {
                    Distance::from(context.to_complex_display(point, props))
                }),
        )
        .add(
            Function::new("conjugate")
                .alias_method(ty::SCALAR_UNKNOWN, "conjugate")
                .overload(Conjugate),
        );
}
