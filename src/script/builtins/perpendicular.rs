use std::rc::Rc;

use crate::{
    script::{
        token::{Position, Span},
        ty,
        unroll::{
            unroll_parameters, CompileContext, Function, FunctionOverload, UnrolledExpression,
            UnrolledExpressionData,
        },
    },
    span,
};

/// `perpendicular_through(line, point)` - returns a line perpendicular to the 1st argument going through point at 2nd argument.
pub fn line_point() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::LINE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::PerpendicularThrough(
            UnrolledExpression {
                weight: 1.0,
                ty: ty::LINE,
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

/// `perpendicular_through(point, line)` - same as previous, just with swapped arguments.
pub fn point_line() -> UnrolledExpression {
    unroll_parameters(
        &line_point(),
        &vec![
            UnrolledExpression {
                weight: 1.0,
                ty: ty::LINE,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
            UnrolledExpression {
                weight: 1.0,
                ty: ty::POINT,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
        ],
    )
}

pub fn register_perpendicular_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("perpendicular_through"),
        Function {
            name: String::from("perpendicular_through"),
            overloads: vec![
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: point_line(),
                    params: vec![ty::POINT, ty::LINE],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: line_point(),
                    params: vec![ty::LINE, ty::POINT],
                    param_group: None,
                },
            ],
        },
    );
}
