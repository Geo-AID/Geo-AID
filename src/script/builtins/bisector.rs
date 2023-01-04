use std::rc::Rc;

use crate::{
    script::{
        token::{Position, Span},
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData, unroll_parameters,
        },
        ty,
    },
    span,
};

/// bisector(pc<3>) - angle bisector.
pub fn pc3() -> UnrolledExpression {
    unroll_parameters(&point_point_point(), &vec![
        UnrolledExpression {
            ty: ty::POINT,
            span: span!(0, 0, 0, 0),
            data: Rc::new(UnrolledExpressionData::IndexCollection(
                UnrolledExpression {
                    ty: ty::collection(3),
                    span: span!(0, 0, 0, 0),
                    data: Rc::new(UnrolledExpressionData::Parameter(0)),
                },
                0
            )),
        },
        UnrolledExpression {
            ty: ty::POINT,
            span: span!(0, 0, 0, 0),
            data: Rc::new(UnrolledExpressionData::IndexCollection(
                UnrolledExpression {
                    ty: ty::collection(3),
                    span: span!(0, 0, 0, 0),
                    data: Rc::new(UnrolledExpressionData::Parameter(0)),
                },
                1
            )),
        },
        UnrolledExpression {
            ty: ty::POINT,
            span: span!(0, 0, 0, 0),
            data: Rc::new(UnrolledExpressionData::IndexCollection(
                UnrolledExpression {
                    ty: ty::collection(3),
                    span: span!(0, 0, 0, 0),
                    data: Rc::new(UnrolledExpressionData::Parameter(0)),
                },
                2
            )),
        }
    ])
}

/// bisector(point, point, point) - angle bisector.
pub fn point_point_point() -> UnrolledExpression {
    UnrolledExpression {
        ty: ty::LINE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::AngleBisector(
            UnrolledExpression {
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
            UnrolledExpression {
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(2)),
            },
        )),
    }
}

/// bisector(pc<2>) - bisector of a segment.
pub fn pc2() -> UnrolledExpression {
    unroll_parameters(&point_point(), &vec![
        UnrolledExpression {
            ty: ty::POINT,
            span: span!(0, 0, 0, 0),
            data: Rc::new(UnrolledExpressionData::IndexCollection(
                UnrolledExpression {
                    ty: ty::collection(3),
                    span: span!(0, 0, 0, 0),
                    data: Rc::new(UnrolledExpressionData::Parameter(0)),
                },
                0
            )),
        },
        UnrolledExpression {
            ty: ty::POINT,
            span: span!(0, 0, 0, 0),
            data: Rc::new(UnrolledExpressionData::IndexCollection(
                UnrolledExpression {
                    ty: ty::collection(3),
                    span: span!(0, 0, 0, 0),
                    data: Rc::new(UnrolledExpressionData::Parameter(0)),
                },
                1
            )),
        },
    ])
}

/// bisector(point, point) - bisector of a segment.
pub fn point_point() -> UnrolledExpression {
    unroll_parameters(
        &super::perpendicular::line_point(),
        &vec![
            UnrolledExpression {
                ty: ty::LINE,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::LineFromPoints(
                    UnrolledExpression {
                        ty: ty::POINT,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    UnrolledExpression {
                        ty: ty::POINT,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(1)),
                    },
                ))
            },
            unroll_parameters(
                &super::mid::mid_function_point(),
                &vec![
                    UnrolledExpression {
                        ty: ty::POINT,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(0)),
                    },
                    UnrolledExpression {
                        ty: ty::POINT,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::Parameter(1)),
                    },
                ]
            )
        ]
    )
}

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("bisector"),
        Function {
            name: String::from("bisector"),
            overloads: vec![
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: pc3(),
                    params: vec![ty::collection(3)],
                    param_group: None
                },
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: point_point_point(),
                    params: vec![ty::POINT, ty::POINT, ty::POINT],
                    param_group: None
                },
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: pc2(),
                    params: vec![ty::collection(2)],
                    param_group: None
                },
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: point_point(),
                    params: vec![ty::POINT, ty::POINT],
                    param_group: None
                },
            ],
        },
    );
}
