use std::rc::Rc;

use crate::{
    script::{
        token::{Position, Span},
        ty,
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData,
        },
    },
    span,
};

/// `intersection(line, line)` - point where two lines intersect.
fn intersection_function_line_line() -> UnrolledExpression {
    UnrolledExpression {
        ty: ty::POINT,
        span: span!(0, 0, 0, 0),
        data: Rc::new(UnrolledExpressionData::LineLineIntersection(
            UnrolledExpression {
                ty: ty::LINE,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
            },
            UnrolledExpression {
                ty: ty::LINE,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
            },
        )),
    }
}

pub fn register_intersection_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("intersection"),
        Function {
            name: String::from("intersection"),
            overloads: vec![FunctionOverload {
                returned_type: ty::POINT,
                definition_span: None,
                definition: intersection_function_line_line(),
                params: vec![ty::LINE, ty::LINE],
                param_group: None,
            }],
        },
    );
}
