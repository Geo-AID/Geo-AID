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

macro_rules! mid_function {
    ($t:expr, $name:ident) => {
        pub fn $name() -> UnrolledExpression {
            UnrolledExpression {
                ty: $t,
                span: span!(0, 0, 0, 0),
                data: Rc::new(UnrolledExpressionData::Average(
                    vec![UnrolledExpression {
                        ty: $t,
                        span: span!(0, 0, 0, 0),
                        data: Rc::new(UnrolledExpressionData::UnrollParameterGroup(0)),
                    }]
                )),
            }
        }
    };
}

mid_function!{ty::POINT, mid_function_point}
mid_function!{ty::DISTANCE, mid_function_distance}
mid_function!{ty::ANGLE, mid_function_angle}
mid_function!{ty::SCALAR, mid_function_scalar}

pub fn register_mid_function(context: &mut CompileContext) {
    context.functions.insert(
        String::from("mid"),
        Function {
            name: String::from("mid"),
            overloads: vec![
                FunctionOverload {
                    returned_type: ty::ANGLE,
                    definition_span: None,
                    definition: mid_function_angle(),
                    params: Vec::new(),
                    param_group: Some(ty::ANGLE)
                },
                FunctionOverload {
                    returned_type: ty::DISTANCE,
                    definition_span: None,
                    definition: mid_function_distance(),
                    params: Vec::new(),
                    param_group: Some(ty::DISTANCE)
                },
                FunctionOverload {
                    returned_type: ty::SCALAR,
                    definition_span: None,
                    definition: mid_function_scalar(),
                    params: Vec::new(),
                    param_group: Some(ty::SCALAR)
                },
                FunctionOverload {
                    returned_type: ty::POINT,
                    definition_span: None,
                    definition: mid_function_point(),
                    params: Vec::new(),
                    param_group: Some(ty::POINT)
                }
            ],
        },
    );
}