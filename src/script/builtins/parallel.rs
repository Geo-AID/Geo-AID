use std::rc::Rc;

use crate::{
    script::{
        token::{Position, Span},
        ty,
        unroll::{
            unroll_parameters, CompileContext, Function, FunctionOverload, UnrolledExpression,
            UnrolledExpressionData, Properties,
        }, compile::PreFigure,
    },
    span,
};

use super::{overload, call};

/// `parallel_through(line, point)` - returns a line parallel to the 1st argument going through point at 2nd argument.
fn parallel_function_line_point(args: &Vec<UnrolledExpression>, figure: &mut PreFigure, display: Option<Properties>) -> UnrolledExpression {
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

pub fn register(context: &mut CompileContext) {
    context.functions.insert(
        String::from("parallel_through"),
        Function {
            name: String::from("parallel_through"),
            overloads: vec![
                overload!((POINT, LINE) -> LINE {
                    |args, figure, _| {
                        call!(figure:parallel_function_line_point(args[1], args[0]))
                    }
                }),
                overload!((LINE, POINT) -> LINE : parallel_function_line_point),
            ],
        },
    );
}
