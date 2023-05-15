use std::rc::Rc;

use crate::{
    script::{
        token::{Position, Span},
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData,
        },
        ty, unit,
    },
    span,
};

/// radians(scalar) - converts no-unit scalar into angle
fn radians_function_scalar() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::ANGLE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::SetUnit(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::SCALAR,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            unit::ANGLE,
        )),
    }
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("radians"),
        Function {
            name: String::from("radians"),
            overloads: vec![FunctionOverload {
                returned_type: ty::ANGLE,
                definition_span: None,
                definition: radians_function_scalar(),
                params: vec![ty::SCALAR],
                param_group: None,
            }],
        },
    );
}
