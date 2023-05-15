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

/// `parallel_through(line, point)` - returns a line parallel to the 1st argument going through point at 2nd argument.
fn parallel_function_line_point() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::LINE,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::ParallelThrough(
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

/// `parallel_through(point, line)` - same as previous, just with swapped arguments.
fn parallel_function_point_line() -> UnrolledExpression {
    unroll_parameters(
        &parallel_function_line_point(),
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

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("parallel_through"),
        Function {
            name: String::from("parallel_through"),
            overloads: vec![
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: parallel_function_point_line(),
                    params: vec![ty::POINT, ty::LINE],
                    param_group: None,
                },
                FunctionOverload {
                    returned_type: ty::LINE,
                    definition_span: None,
                    definition: parallel_function_line_point(),
                    params: vec![ty::LINE, ty::POINT],
                    param_group: None,
                },
            ],
        },
    );
}
