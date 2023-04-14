use std::{collections::HashMap, rc::Rc, sync::Arc};

use super::{Criteria, figure::{Figure, Point, PointDefinition, LineDefinition}, ScriptError, unroll::{self, Variable, UnrolledExpressionData, UnrolledExpression, UnrolledRuleKind}, HashableRc, Expression, Weighed, parser::{Type, PredefinedType}, CriteriaKind};

fn index_collection(expr: &UnrolledExpression, index: usize) -> &UnrolledExpression {
    match expr.data.as_ref() {
        UnrolledExpressionData::VariableAccess(var) => index_collection(&var.definition, index),
        UnrolledExpressionData::PointCollection(col) => &col.get(index).unwrap().definition,
        UnrolledExpressionData::Boxed(expr) => index_collection(expr, index),
        _ => unreachable!("PointCollection should never be achievable by this expression.")
    }
}

fn compile_expression(
    expr: &UnrolledExpression,
    variables: &mut HashMap<HashableRc<Variable>, CompiledVariable>,
    expressions: &mut HashMap<HashableRc<UnrolledExpressionData>, Arc<Weighed<Expression>>>,
    point_index: &mut usize
) -> Arc<Weighed<Expression>> {
    let key = HashableRc::new(Rc::clone(&expr.data));

    if let Some(v) = expressions.get(&key) {
        return Arc::clone(v)
    }

    let compiled = match expr.data.as_ref() {
        UnrolledExpressionData::VariableAccess(var) => compile_variable(Rc::clone(var), variables, expressions, point_index).assume_compiled().unwrap(),
        UnrolledExpressionData::PointCollection(_) => unreachable!("PointCollection should never be compiled."),
        UnrolledExpressionData::Number(v) => Arc::new(Weighed::one(Expression::Literal(
            *v,
            // Essentially, just copy the unit.
            expr.ty.as_predefined().unwrap().as_scalar().unwrap().as_ref().unwrap().clone()
        ))),
        UnrolledExpressionData::FreePoint => {
            let index = *point_index;
            *point_index += 1;

            Arc::new(Weighed::one(Expression::FreePoint(index)))
        },
        UnrolledExpressionData::Boxed(expr) => compile_expression(expr, variables, expressions, point_index),
        UnrolledExpressionData::Parameter(_) => unreachable!("Parameters should never appear in unroll() output."),
        UnrolledExpressionData::IndexCollection(expr, index) => compile_expression(
            index_collection(expr, *index),
            variables,
            expressions,
            point_index
        ),
        UnrolledExpressionData::LineFromPoints(p1, p2) => Arc::new(Weighed::one(
            Expression::Line(
                compile_expression(p1, variables, expressions, point_index),
                compile_expression(p2, variables, expressions, point_index)
            )
        )),
        UnrolledExpressionData::SetUnit(expr, unit) => Arc::new(Weighed::one(
            Expression::SetUnit(
                compile_expression(expr, variables, expressions, point_index),
                unit.clone()
            )
        )),
        UnrolledExpressionData::PointPointDistance(p1, p2) => Arc::new(Weighed::one(
            Expression::PointPointDistance(
                compile_expression(p1, variables, expressions, point_index),
                compile_expression(p2, variables, expressions, point_index)
            )
        )),
        UnrolledExpressionData::PointLineDistance(p, l) => Arc::new(Weighed::one(
            Expression::PointLineDistance(
                compile_expression(p, variables, expressions, point_index),
                compile_expression(l, variables, expressions, point_index)
            )
        )),
        UnrolledExpressionData::Negate(expr) => Arc::new(Weighed::one(
            Expression::Negation(
                compile_expression(expr, variables, expressions, point_index)
            )
        )),
        UnrolledExpressionData::Add(v1, v2) => Arc::new(Weighed::one(
            Expression::Sum(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index)
            )
        )),
        UnrolledExpressionData::Subtract(v1, v2) => Arc::new(Weighed::one(
            Expression::Difference(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index)
            )
        )),
        UnrolledExpressionData::Multiply(v1, v2) => Arc::new(Weighed::one(
            Expression::Product(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index)
            )
        )),
        UnrolledExpressionData::Divide(v1, v2) => Arc::new(Weighed::one(
            Expression::Quotient(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index)
            )
        )),
    };

    expressions.insert(key, Arc::clone(&compiled));
    compiled
}

fn compile_variable(
    var: Rc<Variable>,
    variables: &mut HashMap<HashableRc<Variable>, CompiledVariable>,
    expressions: &mut HashMap<HashableRc<UnrolledExpressionData>, Arc<Weighed<Expression>>>,
    point_index: &mut usize
) -> CompiledVariable {
    let key = HashableRc::new(Rc::clone(&var));

    if let Some(v) = variables.get(&key) {
        return v.clone()
    }

    let compiled = match &var.definition.ty {
        Type::Predefined(PredefinedType::PointCollection(1)) => CompiledVariable::Compiled(compile_expression(
            index_collection(&var.definition, 0),
            variables,
            expressions,
            point_index
        )),
        Type::Predefined(PredefinedType::PointCollection(_)) => CompiledVariable::Unrolled(var.definition.clone()),
        _ => CompiledVariable::Compiled(compile_expression(&var.definition, variables, expressions, point_index))
    };

    variables.insert(HashableRc::new(Rc::clone(&var)), compiled.clone());
    compiled
}

#[derive(Debug, Clone)]
enum CompiledVariable {
    Compiled(Arc<Weighed<Expression>>),
    Unrolled(UnrolledExpression)
}

impl CompiledVariable {
    fn assume_compiled(self) -> Option<Arc<Weighed<Expression>>> {
        if let Self::Compiled(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

fn get_line_definition(expr: &Expression) -> LineDefinition {
    match expr {
        Expression::Line(p1, p2) => LineDefinition::TwoPoints(
            Box::new(get_point_definition(&p1.object)),
            Box::new(get_point_definition(&p2.object))
        ),
        _ => unreachable!("Value of type line should not be achievable this way.")
    }
}

fn get_point_definition(expr: &Expression) -> PointDefinition {
    match expr {
        Expression::FreePoint(index) => PointDefinition::Indexed(*index),
        Expression::LineCrossing(l1, l2) => PointDefinition::Crossing(
            get_line_definition(&l1.object),
            get_line_definition(&l2.object)
        ),
        _ => unreachable!("Value of type point should not be achieveable this way.")
    }
}

pub fn compile(input: String, canvas_size: (usize, usize)) -> Result<(Vec<Criteria>, Figure, usize), ScriptError> {
    let (unrolled, context) = unroll::unroll(input)?;

    let mut variables  = HashMap::new();
    let mut expressions = HashMap::new();
    let mut point_index = 0;

    for (_, var) in context.variables {
        compile_variable(var, &mut variables, &mut expressions, &mut point_index);
    }

    let criteria = unrolled.into_iter().map(
        |rule| {
            let lhs = compile_expression(&rule.lhs, &mut variables, &mut expressions, &mut point_index);
            let rhs = compile_expression(&rule.rhs, &mut variables, &mut expressions, &mut point_index);

            let crit = match rule.kind {
                UnrolledRuleKind::Eq => Weighed::one(CriteriaKind::Equal(lhs, rhs)),
                UnrolledRuleKind::Gt => Weighed::one(CriteriaKind::Greater(lhs, rhs)),
                UnrolledRuleKind::Lt => Weighed::one(CriteriaKind::Less(lhs, rhs)),
            };

            if rule.inverted {
                Weighed {
                    object: CriteriaKind::Inverse(Box::new(crit.object)),
                    weight: crit.weight
                }
            } else {
                crit
            }
        }
    ).collect();

    let figure = Figure {
        points: variables.into_iter().filter(
            |(key, _)| matches!(
                &key.definition.ty,
                Type::Predefined(PredefinedType::PointCollection(1)) | Type::Predefined(PredefinedType::Point)
            )
        ).map(
            |(key, def)| Point {
                label: key.name.clone(),
                definition: get_point_definition(&match def {
                    CompiledVariable::Compiled(cmp) => cmp,
                    CompiledVariable::Unrolled(_) => unreachable!(),
                }.object),
            }
        ).collect(),
        lines: Vec::new(),
        segments: Vec::new(),
        canvas_size,
    };

    Ok((criteria, figure, point_index))
}