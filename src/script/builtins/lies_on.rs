use std::rc::Rc;

use crate::script::{unroll::{CompileContext, Rule, UnrolledExpression, Properties, Library}, builtins::macros::{rule, index, math, angle_expr, number}, parser::Type};

use super::macros::overload;

fn pt_lies_on_circle(lhs: &UnrolledExpression, rhs: &UnrolledExpression, context: &mut CompileContext, _properties: Option<Properties>) {
    let point = context.get_point_by_expr(lhs);
    let circle = context.get_circle_by_expr(rhs);
    context.point_on_circle(point, circle);
}

fn col_lies_on_circle(lhs: &UnrolledExpression, rhs: &UnrolledExpression, context: &mut CompileContext, _properties: Option<Properties>) {
    if let Type::PointCollection(len) = lhs.ty {
        for i in 0..len {
            rule!(context:pt_lies_on_circle(index!(lhs, i), rhs));
        }

        /*
         * For a collection of A_1, A_2, ... A_n, we check if we have the same clockwiseness for triples:
         * (A_1, A_2, A_3), (A_2, A_3, A_4), ... (A_n-1, A_n, A_1)
         */

        for i in 1..(len-1) {
            let last = if i + 2 == len {
                0
            } else {
                i + 2
            };

            rule!(context:>(
                math!(
                    *,
                    angle_expr!(dir index!(lhs, i), index!(lhs, i-1), index!(lhs, i+1)),
                    angle_expr!(dir index!(lhs, i+1), index!(lhs, i), index!(lhs, last))
                ),
                number!(ANGLE 0.01)
            ));
        }
    }
}

pub fn register(library: &mut Library) {
    library.rule_ops.insert(
        String::from("lies_on"),
        Rc::new(Rule {
            name: String::from("lies_on"),
            overloads: vec![
                overload!(POINT lies_on CIRCLE : pt_lies_on_circle),
                overload!(0-P lies_on CIRCLE : col_lies_on_circle)
            ]
        })
    );
}