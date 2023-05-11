use std::rc::Rc;

use crate::{
    script::{
        parser::{Type, Type},
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
        weight: 1.0,
        ty: Type::Predefined(Type::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Angle,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::ThreePointAngle(
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: Type::Predefined(Type::Point),
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    0,
                )),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: Type::Predefined(Type::Point),
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    1,
                )),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: Type::Predefined(Type::Point),
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
        weight: 1.0,
        ty: Type::Predefined(Type::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Angle,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::ThreePointAngle(
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(2)),
            },
        )),
    }
}

/// angle(line, line) - distance between a point and a line.
fn angle_function_line_line() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: Type::Predefined(Type::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Angle,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::TwoLineAngle(
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Line),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: Type::Predefined(Type::Line),
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
                    returned_type: Type::Predefined(Type::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Angle),
                    ))),
                    definition_span: None,
                    definition: angle_function_pc3(),
                    params: vec![Type::Predefined(Type::PointCollection(3))],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: Type::Predefined(Type::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Angle),
                    ))),
                    definition_span: None,
                    definition: angle_function_point_point_point(),
                    params: vec![
                        Type::Predefined(Type::Point),
                        Type::Predefined(Type::Point),
                        Type::Predefined(Type::Point),
                    ],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: Type::Predefined(Type::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Angle),
                    ))),
                    definition_span: None,
                    definition: angle_function_line_line(),
                    params: vec![
                        Type::Predefined(Type::Line),
                        Type::Predefined(Type::Line),
                    ],
                    param_group: None,
                },
            ],
        },
    );
}
