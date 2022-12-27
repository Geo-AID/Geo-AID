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

/// angle(pc<3>) - converts a 3-long point collection to angle.
fn angle_function_pc3() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Angle,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::ThreePointAngle(
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        ty: Type::Predefined(PredefinedType::Point),
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    0,
                )),
            },
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        ty: Type::Predefined(PredefinedType::Point),
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    1,
                )),
            },
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        ty: Type::Predefined(PredefinedType::Point),
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    2,
                )),
            },
        )),
    }
}

/// angle(point, point, point) - angle depicted by 3 points.
fn angle_function_point_point_point() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Angle,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::ThreePointAngle(
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(2)),
            },
        )),
    }
}

/// angle(line, line) - distance between a point and a line.
fn angle_function_line_line() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Angle,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::TwoLineAngle(
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Line),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Line),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
        )),
    }
}

pub fn register_angle_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("angle"),
        Function {
            name: String::from("angle"),
            overloads: vec![
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Angle),
                    ))),
                    definition_span: None,
                    definition: angle_function_pc3(),
                    params: vec![Type::Predefined(PredefinedType::PointCollection(3))],
                },
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Angle),
                    ))),
                    definition_span: None,
                    definition: angle_function_point_point_point(),
                    params: vec![
                        Type::Predefined(PredefinedType::Point),
                        Type::Predefined(PredefinedType::Point),
                        Type::Predefined(PredefinedType::Point),
                    ],
                },
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Angle),
                    ))),
                    definition_span: None,
                    definition: angle_function_line_line(),
                    params: vec![
                        Type::Predefined(PredefinedType::Line),
                        Type::Predefined(PredefinedType::Line),
                    ],
                },
            ],
        },
    );
}
