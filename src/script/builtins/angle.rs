use std::rc::Rc;

use crate::{
    script::{
        token::{Position, Span},
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData,
        },
        ty,
    },
    span,
};

/// angle(pc<3>) - converts a 3-long point collection to angle.
fn angle_function_pc3() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::ANGLE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::ThreePointAngle(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::POINT,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    0,
                )),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::POINT,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    1,
                )),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::IndexCollection(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::POINT,
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
        ty: ty::ANGLE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::ThreePointAngle(
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
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
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
        ty: ty::ANGLE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::TwoLineAngle(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::LINE,
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

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("angle"),
        Function {
            name: String::from("angle"),
            overloads: vec![
                FunctionOverload {
                    returned_type: ty::ANGLE,
                    definition_span: None,
                    definition: angle_function_pc3(),
                    params: vec![ty::collection(3)],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::ANGLE,
                    definition_span: None,
                    definition: angle_function_point_point_point(),
                    params: vec![
                        ty::POINT,
                        ty::POINT,
                        ty::POINT,
                    ],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::ANGLE,
                    definition_span: None,
                    definition: angle_function_line_line(),
                    params: vec![
                        ty::LINE,
                        ty::LINE,
                    ],
                    param_group: None,
                },
            ],
        },
    );
}
