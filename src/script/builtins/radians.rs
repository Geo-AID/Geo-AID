use std::rc::Rc;

use crate::{
    script::{
        parser::{PredefinedType, Type},
        token::{Position, Span},
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData,
        },
        ComplexUnit, SimpleUnit,
    },
    span,
};

/// radians(scalar) - converts no-unit scalar into angle
fn radians_function_scalar() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Angle,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::SetUnit(
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Scalar,
                )))),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            ComplexUnit::new(SimpleUnit::Angle),
        )),
    }
}

pub fn register_radians_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("radians"),
        Function {
            name: String::from("radians"),
            overloads: vec![FunctionOverload {
                returned_type: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Angle,
                )))),
                definition_span: None,
                definition: radians_function_scalar(),
                params: vec![Type::Predefined(PredefinedType::Scalar(Some(
                    ComplexUnit::new(SimpleUnit::Scalar),
                )))],
                param_group: None
            }],
        },
    );
}
