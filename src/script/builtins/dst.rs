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

/// dst(number), dst(pc<2>) - distance convertible as param.
fn dst_function_dst_convertible() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::DISTANCE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::Parameter(0)), // All the params are going to be converted before this is called.
    }
}

/// dst(scalar) - converts no-unit scalar to distance.
fn dst_function_scalar() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::DISTANCE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::SetUnit(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::DISTANCE,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            unit::DISTANCE,
        )),
    }
}

/// dst(point, point) - distance between two points.
fn dst_function_point_point() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::DISTANCE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::PointPointDistance(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
        )),
    }
}

/// dst(point, line) - distance between a point and a line.
fn dst_function_point_line() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::DISTANCE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::PointLineDistance(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: ty::LINE,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
        )),
    }
}

/// dst(line, point) - distance between a line and a point.
fn dst_function_line_point() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::DISTANCE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::PointLineDistance(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: ty::LINE,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
        )),
    }
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("dst"),
        Function {
            name: String::from("dst"),
            overloads: vec![
                FunctionOverload {
                    returned_type: ty::DISTANCE,
                    definition_span: None,
                    definition: dst_function_dst_convertible(),
                    params: vec![ty::DISTANCE],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::DISTANCE,
                    definition_span: None,
                    definition: dst_function_scalar(),
                    params: vec![ty::DISTANCE],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::DISTANCE,
                    definition_span: None,
                    definition: dst_function_point_point(),
                    params: vec![
                        ty::POINT,
                        ty::POINT,
                    ],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::DISTANCE,
                    definition_span: None,
                    definition: dst_function_point_line(),
                    params: vec![
                        ty::POINT,
                        ty::LINE,
                    ],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::DISTANCE,
                    definition_span: None,
                    definition: dst_function_line_point(),
                    params: vec![
                        ty::LINE,
                        ty::POINT,
                    ],
                    param_group: None,
                },
            ],
        },
    );
}
