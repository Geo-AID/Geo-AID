use std::rc::Rc;

use crate::{script::{unroll::{UnrolledExpression, UnrolledExpressionData, CompileContext, Function, FunctionOverload},
token::{Span, Position}, parser::{Type, PredefinedType}}, span};

/// Point constructor.
fn point_function() -> UnrolledExpression {
    UnrolledExpression {
        ty: Type::Predefined(PredefinedType::Point),
        data: Rc::new(UnrolledExpressionData::FreePoint),
        span: span!(0, 0, 0, 0)
    }
}

pub fn register_point_function(context: &mut CompileContext) {
    context.functions.insert(String::from("Point"), Function {
        name: String::from("Point"),
        overloads: vec![
            FunctionOverload {
                params: Vec::new(),
                returned_type: Type::Predefined(PredefinedType::Point),
                definition_span: None,
                definition: point_function()
            }
        ],
    });
}