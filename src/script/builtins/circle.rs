use std::rc::Rc;

use crate::{
    script::{
        token::{Position, Span},
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData,
        }, ty,
    },
    span,
};

/// Circle constructor. Creates a circle based off of its center and radius.
fn circle_function() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::CIRCLE,
        data: Rc::new(UnrolledExpressionData::Circle(
            UnrolledExpression {
                data: Rc::new(UnrolledExpressionData::Parameter(0)),
                ty: ty::POINT,
                span: span!(0,0, 0,0),
                weight: 1.0
            },
            UnrolledExpression {
                data: Rc::new(UnrolledExpressionData::Parameter(1)),
                ty: ty::DISTANCE,
                span: span!(0, 0, 0, 0),
                weight: 1.0
            }
        )),
        span: span!(0, 0, 0, 0),
    }
}

pub fn register_circle_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("Circle"),
        Function {
            name: String::from("Circle"),
            overloads: vec![FunctionOverload {
                params: Vec::new(),
                returned_type: ty::CIRCLE,
                definition_span: None,
                definition: circle_function(),
                param_group: None,
            }],
        },
    );
}
