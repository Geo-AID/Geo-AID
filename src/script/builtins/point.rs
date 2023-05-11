use std::rc::Rc;

use crate::{
    script::{
        parser::{Type, Type},
        token::{Position, Span},
        unroll::{
            CompileContext, Function, FunctionOverload, UnrolledExpression, UnrolledExpressionData,
        },
    },
    span,
};

/// Point constructor. Creates a free point.
fn point_function() -> UnrolledExpression {
    UnrolledExpression {
        weight: 1.0,
        ty: Type::Predefined(Type::Point),
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
                returned_type: Type::Predefined(Type::Point),
                definition_span: None,
                definition: point_function(),
                param_group: None,
            }],
        },
    );
}
