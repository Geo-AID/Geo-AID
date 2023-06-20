use std::rc::Rc;

use crate::script::{unroll::{CompileContext, Rule, UnrolledExpression, Properties, UnrolledRule}, compile::PreFigure};

use super::macros::overload;

fn pt_lies_on_circle(lhs: &UnrolledExpression, rhs: &UnrolledExpression, figure: &mut PreFigure, context: &mut CompileContext, properties: Option<Properties>) -> Vec<UnrolledRule> {
    context.set_solver.point(lhs).lies_on(rhs)
}

pub fn register(context: &mut CompileContext) {
    context.rule_ops.insert(
        String::from("lies_on"),
        Rc::new(Rule {
            name: String::from("lies_on"),
            overloads: vec![
                overload!(POINT lies_on CIRCLE : pt_lies_on_circle)
            ]
        })
    );
}