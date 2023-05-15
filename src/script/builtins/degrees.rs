use std::{f64::consts::PI, rc::Rc};

use crate::{
    script::{
        token::{Position, Span},
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData,
        }, ty, unit,
    },
    span,
};

/// degrees(scalar) - converts no-unit scalar into angle
fn degrees_function_scalar() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::ANGLE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::SetUnit(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::SCALAR,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Multiply(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::SCALAR,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::SCALAR,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Number(PI / 180.0)),
                    },
                )),
            },
            unit::ANGLE
        )),
    }
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("degrees"),
        Function {
            name: String::from("degrees"),
            overloads: vec![FunctionOverload {
                returned_type: ty::ANGLE,
                definition_span: None,
                definition: degrees_function_scalar(),
                params: vec![ty::SCALAR],
                param_group: None,
            }],
        },
    );
}
