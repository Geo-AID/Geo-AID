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

/// dst(number), dst(pc<2>) - distance convertible as param.
fn dst_function_dst_convertible() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Distance,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::Parameter(0)), // All the params are going to be converted before this is called.
    }
}

/// dst(scalar) - converts no-unit scalar to distance.
fn dst_function_scalar() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Scalar,
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
            ComplexUnit::new(SimpleUnit::Distance),
        )),
    }
}

/// dst(point, point) - distance between two points.
fn dst_function_point_point() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Distance,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::PointPointDistance(
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
        )),
    }
}

/// dst(point, line) - distance between a point and a line.
fn dst_function_point_line() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Distance,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::PointLineDistance(
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
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

/// dst(line, point) - distance between a line and a point.
fn dst_function_line_point() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
            SimpleUnit::Distance,
        )))),
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::PointLineDistance(
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Point),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
            UnrolledExpression {
                ty: Type::Predefined(PredefinedType::Line),
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
        )),
    }
}

pub fn register_dst_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("dst"),
        Function {
            name: String::from("dst"),
            overloads: vec![
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Distance),
                    ))),
                    definition_span: None,
                    definition: dst_function_dst_convertible(),
                    params: vec![Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Distance),
                    )))],
                },
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Distance),
                    ))),
                    definition_span: None,
                    definition: dst_function_scalar(),
                    params: vec![Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Scalar),
                    )))],
                },
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Distance),
                    ))),
                    definition_span: None,
                    definition: dst_function_point_point(),
                    params: vec![
                        Type::Predefined(PredefinedType::Point),
                        Type::Predefined(PredefinedType::Point),
                    ],
                },
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Distance),
                    ))),
                    definition_span: None,
                    definition: dst_function_point_line(),
                    params: vec![
                        Type::Predefined(PredefinedType::Point),
                        Type::Predefined(PredefinedType::Line),
                    ],
                },
                FunctionOverload {
                    returned_type: Type::Predefined(PredefinedType::Scalar(Some(
                        ComplexUnit::new(SimpleUnit::Distance),
                    ))),
                    definition_span: None,
                    definition: dst_function_line_point(),
                    params: vec![
                        Type::Predefined(PredefinedType::Line),
                        Type::Predefined(PredefinedType::Point),
                    ],
                },
            ],
        },
    );
}
