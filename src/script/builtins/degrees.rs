use std::{f64::consts::PI, rc::Rc};

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

/// degrees(scalar) - converts no-unit scalar into angle
fn degrees_function_scalar() -> UnrolledExpression {
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
                data: Rc::new(UnrolledExpressionData::Multiply(
                    UnrolledExpression {
                        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                            SimpleUnit::Scalar,
                        )))),
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    UnrolledExpression {
                        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                            SimpleUnit::Scalar,
                        )))),
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Number(PI / 180.0)),
                    },
                )),
            },
            ComplexUnit::new(SimpleUnit::Angle),
        )),
    }
}

pub fn register_degrees_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("degrees"),
        Function {
            name: String::from("degrees"),
            overloads: vec![FunctionOverload {
                returned_type: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Angle,
                )))),
                definition_span: None,
                definition: degrees_function_scalar(),
                params: vec![Type::Predefined(PredefinedType::Scalar(Some(
                    ComplexUnit::new(SimpleUnit::Scalar),
                )))],
                param_group: None
            }],
        },
    );
}
