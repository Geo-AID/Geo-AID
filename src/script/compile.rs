use std::{collections::HashMap, rc::Rc, sync::Arc};

use crate::generator;

use super::{
    figure::Figure,
    parser::{PredefinedType, Type},
    unroll::{
        self, PointMeta, UnrolledExpression, UnrolledExpressionData, UnrolledRule,
        UnrolledRuleKind, Variable,
    },
    ComplexUnit, Criteria, CriteriaKind, Error, Expression, HashableRc, SimpleUnit, Weighed,
};

/// Takes the unrolled expression of type `PointCollection` and takes the point at `index`, isolating it out of the entire expression.
fn index_collection(expr: &UnrolledExpression, index: usize) -> &UnrolledExpression {
    match expr.data.as_ref() {
        UnrolledExpressionData::VariableAccess(var) => index_collection(&var.definition, index),
        UnrolledExpressionData::PointCollection(col) => col.get(index).unwrap(),
        UnrolledExpressionData::Boxed(expr) => index_collection(expr, index),
        _ => unreachable!("PointCollection should never be achievable by this expression."),
    }
}

#[allow(clippy::too_many_lines)]
fn compile_expression(
    expr: &UnrolledExpression,
    variables: &mut HashMap<HashableRc<Variable>, CompiledVariable>,
    expressions: &mut HashMap<HashableRc<UnrolledExpressionData>, Arc<Weighed<Expression>>>,
    point_index: &mut usize,
) -> Arc<Weighed<Expression>> {
    // First we have to check if this expression has been compiled already.
    let key = HashableRc::new(Rc::clone(&expr.data));

    if let Some(v) = expressions.get(&key) {
        // If so, we return it.
        return Arc::clone(v);
    }

    // Otherwise we compile.
    let compiled = match expr.data.as_ref() {
        UnrolledExpressionData::VariableAccess(var) => {
            compile_variable(var, variables, expressions, point_index)
                .assume_compiled()
                .unwrap()
        }
        // Critic doesn't support PointCollections, so this code should never be reached.
        UnrolledExpressionData::PointCollection(_) => {
            unreachable!("PointCollection should never be compiled.")
        }
        UnrolledExpressionData::UnrollParameterGroup(_) => {
            unreachable!("UnrollParameterGroup should never be compiled.")
        }
        UnrolledExpressionData::Number(v) => Arc::new(Weighed::one(Expression::Literal(
            *v,
            // Essentially, just copy the unit.
            expr.ty
                .as_predefined()
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_ref()
                .unwrap_or(&ComplexUnit::new(SimpleUnit::Scalar))
                .clone(),
        ))),
        UnrolledExpressionData::FreePoint => {
            let index = *point_index;
            *point_index += 1;

            Arc::new(Weighed::one(Expression::FreePoint(index)))
        }
        UnrolledExpressionData::Boxed(expr) => {
            compile_expression(expr, variables, expressions, point_index)
        }
        UnrolledExpressionData::Parameter(_) => {
            unreachable!("Parameters should never appear in unroll() output.")
        }
        UnrolledExpressionData::IndexCollection(expr, index) => compile_expression(
            index_collection(expr, *index),
            variables,
            expressions,
            point_index,
        ),
        UnrolledExpressionData::LineFromPoints(p1, p2) => Arc::new(Weighed::one(Expression::Line(
            compile_expression(p1, variables, expressions, point_index),
            compile_expression(p2, variables, expressions, point_index),
        ))),
        UnrolledExpressionData::ParallelThrough(p1, p2) => {
            Arc::new(Weighed::one(Expression::ParallelThrough(
                compile_expression(p1, variables, expressions, point_index),
                compile_expression(p2, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::PerpendicularThrough(p1, p2) => {
            Arc::new(Weighed::one(Expression::PerpendicularThrough(
                compile_expression(p1, variables, expressions, point_index),
                compile_expression(p2, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::SetUnit(expr, unit) => Arc::new(Weighed::one(Expression::SetUnit(
            compile_expression(expr, variables, expressions, point_index),
            unit.clone(),
        ))),
        UnrolledExpressionData::PointPointDistance(p1, p2) => {
            Arc::new(Weighed::one(Expression::PointPointDistance(
                compile_expression(p1, variables, expressions, point_index),
                compile_expression(p2, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::PointLineDistance(p, l) => {
            Arc::new(Weighed::one(Expression::PointLineDistance(
                compile_expression(p, variables, expressions, point_index),
                compile_expression(l, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::Negate(expr) => Arc::new(Weighed::one(Expression::Negation(
            compile_expression(expr, variables, expressions, point_index),
        ))),
        UnrolledExpressionData::Add(v1, v2) => Arc::new(Weighed::one(Expression::Sum(
            compile_expression(v1, variables, expressions, point_index),
            compile_expression(v2, variables, expressions, point_index),
        ))),
        UnrolledExpressionData::Subtract(v1, v2) => Arc::new(Weighed::one(Expression::Difference(
            compile_expression(v1, variables, expressions, point_index),
            compile_expression(v2, variables, expressions, point_index),
        ))),
        UnrolledExpressionData::Multiply(v1, v2) => Arc::new(Weighed::one(Expression::Product(
            compile_expression(v1, variables, expressions, point_index),
            compile_expression(v2, variables, expressions, point_index),
        ))),
        UnrolledExpressionData::Divide(v1, v2) => Arc::new(Weighed::one(Expression::Quotient(
            compile_expression(v1, variables, expressions, point_index),
            compile_expression(v2, variables, expressions, point_index),
        ))),
        UnrolledExpressionData::ThreePointAngle(v1, v2, v3) => {
            Arc::new(Weighed::one(Expression::AnglePoint(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index),
                compile_expression(v3, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::AngleBisector(v1, v2, v3) => {
            Arc::new(Weighed::one(Expression::AngleBisector(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index),
                compile_expression(v3, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::TwoLineAngle(v1, v2) => {
            Arc::new(Weighed::one(Expression::AngleLine(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::LineLineIntersection(v1, v2) => {
            Arc::new(Weighed::one(Expression::LineLineIntersection(
                compile_expression(v1, variables, expressions, point_index),
                compile_expression(v2, variables, expressions, point_index),
            )))
        }
        UnrolledExpressionData::Average(exprs) => Arc::new(Weighed::one(Expression::Average(
            exprs
                .iter()
                .map(|expr| compile_expression(expr, variables, expressions, point_index))
                .collect(),
        ))),
    };

    // We insert for memory.
    expressions.insert(key, Arc::clone(&compiled));
    compiled
}

/// Attempts to compile the variable. If the variable is a `PointCollection`, leaves it unrolled. Otherwise everything is compiled properly.
fn compile_variable(
    var: &Rc<Variable>,
    variables: &mut HashMap<HashableRc<Variable>, CompiledVariable>,
    expressions: &mut HashMap<HashableRc<UnrolledExpressionData>, Arc<Weighed<Expression>>>,
    point_index: &mut usize,
) -> CompiledVariable {
    // We first have to see if the variable already exists.
    let key = HashableRc::new(Rc::clone(var));

    if let Some(v) = variables.get(&key) {
        // So we can return it here.
        return v.clone();
    }

    // And otherwise compile it.
    let compiled = match &var.definition.ty {
        Type::Predefined(PredefinedType::PointCollection(1)) => {
            CompiledVariable::Compiled(compile_expression(
                index_collection(&var.definition, 0),
                variables,
                expressions,
                point_index,
            ))
        }
        Type::Predefined(PredefinedType::PointCollection(_)) => {
            CompiledVariable::Unrolled(var.definition.clone())
        }
        _ => CompiledVariable::Compiled(compile_expression(
            &var.definition,
            variables,
            expressions,
            point_index,
        )),
    };

    // We insert for memory
    variables.insert(HashableRc::new(Rc::clone(var)), compiled.clone());
    compiled
}

/// Represents the output of `compile_variable()`.
#[derive(Debug, Clone)]
enum CompiledVariable {
    /// A compiled variable.
    Compiled(Arc<Weighed<Expression>>),
    /// An unrolled variable of type `PointCollection`.
    Unrolled(UnrolledExpression),
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

fn compile_rules(
    unrolled: Vec<UnrolledRule>,
    variables: &mut HashMap<HashableRc<Variable>, CompiledVariable>,
    expressions: &mut HashMap<HashableRc<UnrolledExpressionData>, Arc<Weighed<Expression>>>,
    point_index: &mut usize,
) -> Vec<Criteria> {
    unrolled
        .into_iter()
        .map(|rule| {
            let lhs = compile_expression(&rule.lhs, variables, expressions, point_index);
            let rhs = compile_expression(&rule.rhs, variables, expressions, point_index);

            let crit = match rule.kind {
                UnrolledRuleKind::Eq => Weighed::one(CriteriaKind::Equal(lhs, rhs)),
                UnrolledRuleKind::Gt => Weighed::one(CriteriaKind::Greater(lhs, rhs)),
                UnrolledRuleKind::Lt => Weighed::one(CriteriaKind::Less(lhs, rhs)),
            };

            if rule.inverted {
                Weighed {
                    object: CriteriaKind::Inverse(Box::new(crit.object)),
                    weight: crit.weight,
                }
            } else {
                crit
            }
        })
        .collect()
}

/// Compiles the given script.
///
/// # Errors
/// Exact descriptions of errors are in `ScriptError` documentation.
///
/// # Panics
/// Never
pub fn compile(
    input: &str,
    canvas_size: (usize, usize),
) -> Result<(Vec<Criteria>, Figure, usize, generator::Flags), Error> {
    // First, we have to unroll the script.
    let (unrolled, context) = unroll::unroll(input)?;

    let mut variables = HashMap::new();
    let mut expressions = HashMap::new();
    let mut point_index = 0;

    // We precompile all variables.
    for (_, var) in context.variables {
        compile_variable(&var, &mut variables, &mut expressions, &mut point_index);
    }

    // And compile the rules
    let criteria = compile_rules(unrolled, &mut variables, &mut expressions, &mut point_index);

    let figure = Figure {
        // We're displaying every variable of type Point
        points: variables
            .into_iter()
            .filter(|(key, _)| {
                matches!(
                    &key.definition.ty,
                    Type::Predefined(PredefinedType::PointCollection(1) | PredefinedType::Point)
                )
            })
            .map(|(key, def)| {
                (
                    def.assume_compiled().unwrap(),
                    key.meta
                        .as_point()
                        .unwrap()
                        .meta
                        .clone()
                        .unwrap_or(PointMeta {
                            letter: 'P',
                            primes: 0,
                            index: None,
                        }),
                )
            })
            .collect(),
        lines: Vec::new(),
        angles: Vec::new(),
        canvas_size,
    };

    let flags = generator::Flags {
        optimizations: generator::Optimizations {
            identical_expressions: context.flags["optimizations"].as_set().unwrap()
                ["identical_expressions"]
                .as_bool()
                .unwrap(),
        },
    };

    Ok((criteria, figure, point_index, flags))
}
