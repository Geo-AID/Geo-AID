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

/// Point constructor. Creates a free point.
fn point_function() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: ty::POINT,
        data: Rc::new(UnrolledExpressionData::FreePoint),
        span: span!(0, 0, 0, 0),
    }
}

pub fn register_point_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("Point"),
        Function {
            name: String::from("Point"),
            overloads: vec![FunctionOverload {
                params: Vec::new(),
                returned_type: ty::POINT,
                definition_span: None,
                definition: point_function(),
                param_group: None,
            }],
        },
    );
}
