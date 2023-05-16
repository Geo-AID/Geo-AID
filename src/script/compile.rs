use std::{collections::HashMap, rc::Rc, sync::Arc, unreachable};

use crate::{
    generator::{
        self,
        expression::{
            expr::{
                AngleBisector, AngleLine, AnglePoint, Average, Difference, FreePoint,
                LineLineIntersection, LinePoint, Literal, Negation, ParallelThrough,
                PerpendicularThrough, PointLineDistance, PointPointDistance, PointX, PointY,
                Product, Quotient, Real, SetUnit, Sum, CenterRadius,
            },
            AnyExpr, Expression, LineExpr, PointExpr, ScalarExpr, Weights, CircleExpr,
        },
        AdjustableTemplate, DistanceLiterals, Flags, Optimizations,
    },
    span,
};

use super::{
    figure::Figure,
    parser::Type,
    token::{Position, Span},
    ty,
    unroll::{
        self, Flag, UnrolledExpression, UnrolledExpressionData, UnrolledRule,
        UnrolledRuleKind, Variable, CompileContext,
    },
    Criteria, CriteriaKind, Error, HashableRc, SimpleUnit, Weighed,
};

trait Mapping<K, T> {
    fn get(&self, key: &HashableRc<K>) -> Option<&Arc<Expression<T>>>;

    fn insert(
        &mut self,
        key: HashableRc<K>,
        value: Arc<Expression<T>>,
    ) -> Option<Arc<Expression<T>>>;
}

#[derive(Debug, Default)]
struct ExpressionRecord {
    points: HashMap<HashableRc<UnrolledExpressionData>, Arc<Expression<PointExpr>>>,
    lines: HashMap<HashableRc<UnrolledExpressionData>, Arc<Expression<LineExpr>>>,
    scalars: HashMap<HashableRc<UnrolledExpressionData>, Arc<Expression<ScalarExpr>>>,
    circles: HashMap<HashableRc<UnrolledExpressionData>, Arc<Expression<CircleExpr>>>
}

#[derive(Debug, Default)]
struct VariableRecord {
    points: HashMap<HashableRc<Variable>, Arc<Expression<PointExpr>>>,
    lines: HashMap<HashableRc<Variable>, Arc<Expression<LineExpr>>>,
    scalars: HashMap<HashableRc<Variable>, Arc<Expression<ScalarExpr>>>,
    circles: HashMap<HashableRc<Variable>, Arc<Expression<CircleExpr>>>,
}

impl Mapping<Variable, PointExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<Variable>) -> Option<&Arc<Expression<PointExpr>>> {
        self.points.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<Variable>,
        value: Arc<Expression<PointExpr>>,
    ) -> Option<Arc<Expression<PointExpr>>> {
        self.points.insert(key, value)
    }
}

impl Mapping<Variable, LineExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<Variable>) -> Option<&Arc<Expression<LineExpr>>> {
        self.lines.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<Variable>,
        value: Arc<Expression<LineExpr>>,
    ) -> Option<Arc<Expression<LineExpr>>> {
        self.lines.insert(key, value)
    }
}

impl Mapping<Variable, ScalarExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<Variable>) -> Option<&Arc<Expression<ScalarExpr>>> {
        self.scalars.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<Variable>,
        value: Arc<Expression<ScalarExpr>>,
    ) -> Option<Arc<Expression<ScalarExpr>>> {
        self.scalars.insert(key, value)
    }
}

impl Mapping<Variable, CircleExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<Variable>) -> Option<&Arc<Expression<CircleExpr>>> {
        self.circles.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<Variable>,
        value: Arc<Expression<CircleExpr>>,
    ) -> Option<Arc<Expression<CircleExpr>>> {
        self.circles.insert(key, value)
    }
}

trait Compile {
    fn compile(
        expr: &UnrolledExpression,
        variables: &mut VariableRecord,
        expressions: &mut ExpressionRecord,
        template: &mut Vec<AdjustableTemplate>,
        dst_var: &Option<Rc<Variable>>,
    ) -> Arc<Self>;
}

/// Takes the unrolled expression of type `PointCollection` and takes the point at `index`, isolating it out of the entire expression.
fn index_collection(expr: &UnrolledExpression, index: usize) -> &UnrolledExpression {
    match expr.data.as_ref() {
        UnrolledExpressionData::VariableAccess(var) => index_collection(&var.definition, index),
        UnrolledExpressionData::PointCollection(col) => col.get(index).unwrap(),
        UnrolledExpressionData::Boxed(expr) => index_collection(expr, index),
        _ => unreachable!("PointCollection should never be achievable by this expression."),
    }
}

#[must_use]
pub fn fix_distance(
    expr: UnrolledExpression,
    power: i8,
    dst_var: &Rc<Variable>,
) -> UnrolledExpression {
    let sp = expr.span;
    let t = expr.ty.clone();

    match power.cmp(&0) {
        std::cmp::Ordering::Less => UnrolledExpression {
            weight: 1.0,
            data: Rc::new(UnrolledExpressionData::Divide(
                fix_distance(expr, power + 1, dst_var),
                UnrolledExpression {
                    weight: 1.0,
                    ty: ty::SCALAR,
                    span: sp,
                    data: Rc::new(UnrolledExpressionData::VariableAccess(Rc::clone(dst_var))),
                },
            )),
            ty: t,
            span: sp,
        },
        std::cmp::Ordering::Equal => expr,
        std::cmp::Ordering::Greater => UnrolledExpression {
            weight: 1.0,
            data: Rc::new(UnrolledExpressionData::Multiply(
                fix_distance(expr, power - 1, dst_var),
                UnrolledExpression {
                    weight: 1.0,
                    ty: ty::SCALAR,
                    span: sp,
                    data: Rc::new(UnrolledExpressionData::VariableAccess(Rc::clone(dst_var))),
                },
            )),
            ty: t,
            span: sp,
        },
    }
}

impl Compile for Expression<PointExpr> {
    fn compile(
        expr: &UnrolledExpression,
        variables: &mut VariableRecord,
        expressions: &mut ExpressionRecord,
        template: &mut Vec<AdjustableTemplate>,
        dst_var: &Option<Rc<Variable>>,
    ) -> Arc<Self> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = expressions.points.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => {
                compile_variable(var, variables, expressions, template, dst_var)
            }
            UnrolledExpressionData::FreePoint => {
                let index = template.len();
                template.push(AdjustableTemplate::Point);

                Arc::new(Expression::new(PointExpr::Free(FreePoint { index }), 1.0))
            }
            UnrolledExpressionData::Boxed(expr) => {
                Self::compile(expr, variables, expressions, template, dst_var)
            }
            UnrolledExpressionData::IndexCollection(expr, index) => Self::compile(
                index_collection(expr, *index),
                variables,
                expressions,
                template,
                dst_var,
            ),
            UnrolledExpressionData::LineLineIntersection(k, l) => Arc::new(Expression::new(
                PointExpr::LineLineIntersection(LineLineIntersection {
                    k: Expression::compile(k, variables, expressions, template, dst_var),
                    l: Expression::compile(l, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::Average(exprs) => Arc::new(Expression::new(
                PointExpr::Average(Average {
                    items: exprs
                        .iter()
                        .map(|expr| Self::compile(expr, variables, expressions, template, dst_var))
                        .collect(),
                }),
                1.0,
            )),
            _ => unreachable!("A point should never be compiled this way"),
        };

        // We insert for memory.
        expressions.points.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl Compile for Expression<LineExpr> {
    fn compile(
        expr: &UnrolledExpression,
        variables: &mut VariableRecord,
        expressions: &mut ExpressionRecord,
        template: &mut Vec<AdjustableTemplate>,
        dst_var: &Option<Rc<Variable>>,
    ) -> Arc<Self> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = expressions.lines.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => {
                compile_variable(var, variables, expressions, template, dst_var)
            }
            UnrolledExpressionData::Boxed(expr) => {
                Expression::compile(expr, variables, expressions, template, dst_var)
            }
            UnrolledExpressionData::LineFromPoints(p1, p2) => Arc::new(Expression::new(
                LineExpr::Line(LinePoint {
                    a: Expression::compile(p1, variables, expressions, template, dst_var),
                    b: Expression::compile(p2, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::ParallelThrough(line, point) => Arc::new(Expression::new(
                LineExpr::ParallelThrough(ParallelThrough {
                    line: Expression::compile(line, variables, expressions, template, dst_var),
                    point: Expression::compile(point, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::PerpendicularThrough(line, point) => Arc::new(Expression::new(
                LineExpr::PerpendicularThrough(PerpendicularThrough {
                    line: Expression::compile(line, variables, expressions, template, dst_var),
                    point: Expression::compile(point, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::AngleBisector(v1, v2, v3) => Arc::new(Expression::new(
                LineExpr::AngleBisector(AngleBisector {
                    arm1: Expression::compile(v1, variables, expressions, template, dst_var),
                    origin: Expression::compile(v2, variables, expressions, template, dst_var),
                    arm2: Expression::compile(v3, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            _ => unreachable!("A line should never be compiled this way"),
        };

        // We insert for memory.
        expressions.lines.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl Compile for Expression<CircleExpr> {
    fn compile(
            expr: &UnrolledExpression,
            variables: &mut VariableRecord,
            expressions: &mut ExpressionRecord,
            template: &mut Vec<AdjustableTemplate>,
            dst_var: &Option<Rc<Variable>>,
        ) -> Arc<Self> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = expressions.circles.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::Circle(center, radius) => Arc::new(Expression::new(
                CircleExpr::CenterRadius(CenterRadius {
                    center: Expression::compile(center, variables, expressions, template, dst_var),
                    radius: Expression::compile(radius, variables, expressions, template, dst_var)
                }),
                1.0
            )),
            _ => unreachable!("A circle should never be compiled this way"),
        };

        // We insert for memory.
        expressions.circles.insert(key, Arc::clone(&compiled));
        compiled
    }
}

fn compile_number(
    expr: &UnrolledExpression,
    v: f64,
    variables: &mut VariableRecord,
    expressions: &mut ExpressionRecord,
    template: &mut Vec<AdjustableTemplate>,
    dst_var: &Option<Rc<Variable>>
) -> Arc<Expression<ScalarExpr>> {
    if expr.ty == ty::SCALAR {
        // If a scalar, we treat it as a standard literal.
        Arc::new(Expression::new(
            ScalarExpr::Literal(Literal { value: v }),
            1.0,
        ))
    } else {
        // Otherwise we pretend it's a scalar literal inside a SetUnit.
        Expression::compile(
            &UnrolledExpression {
                weight: 1.0,
                ty: expr.ty.clone(),
                span: expr.span,
                data: Rc::new(UnrolledExpressionData::SetUnit(
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::SCALAR,
                        span: expr.span,
                        data: expr.data.clone(),
                    },
                    expr.ty
                        .as_scalar()
                        .unwrap()
                        .as_ref()
                        .unwrap()
                        .clone(),
                )),
            },
            variables,
            expressions,
            template,
            dst_var,
        )
    }
}

impl Compile for Expression<ScalarExpr> {
    #[allow(clippy::too_many_lines)]
    fn compile(
        expr: &UnrolledExpression,
        variables: &mut VariableRecord,
        expressions: &mut ExpressionRecord,
        template: &mut Vec<AdjustableTemplate>,
        dst_var: &Option<Rc<Variable>>,
    ) -> Arc<Self> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = expressions.scalars.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => {
                compile_variable(var, variables, expressions, template, dst_var)
            }
            UnrolledExpressionData::Number(v) => compile_number(expr, *v, variables, expressions, template, dst_var),
            UnrolledExpressionData::FreeReal => {
                let index = template.len();
                template.push(AdjustableTemplate::Real);

                Arc::new(Expression::new(ScalarExpr::Real(Real { index }), 1.0))
            }
            UnrolledExpressionData::Boxed(expr) => {
                Self::compile(expr, variables, expressions, template, dst_var)
            }
            UnrolledExpressionData::SetUnit(expr, unit) => Arc::new(Expression::new(
                ScalarExpr::SetUnit(SetUnit {
                    value: Self::compile(
                        &fix_distance(
                            expr.clone(),
                            unit[SimpleUnit::Distance as usize]
                                - match expr.ty.as_scalar().unwrap() {
                                    Some(unit) => unit[SimpleUnit::Distance as usize],
                                    None => 0,
                                },
                            dst_var.as_ref().unwrap(),
                        ),
                        variables,
                        expressions,
                        template,
                        dst_var,
                    ),
                    unit: unit.clone(),
                }),
                1.0,
            )),
            UnrolledExpressionData::PointPointDistance(p1, p2) => Arc::new(Expression::new(
                ScalarExpr::PointPointDistance(PointPointDistance {
                    a: Expression::compile(p1, variables, expressions, template, dst_var),
                    b: Expression::compile(p2, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::PointLineDistance(p, l) => Arc::new(Expression::new(
                ScalarExpr::PointLineDistance(PointLineDistance {
                    point: Expression::compile(p, variables, expressions, template, dst_var),
                    line: Expression::compile(l, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::Negate(expr) => Arc::new(Expression::new(
                ScalarExpr::Negation(Negation {
                    value: Self::compile(expr, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::Add(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Sum(Sum {
                    a: Self::compile(v1, variables, expressions, template, dst_var),
                    b: Self::compile(v2, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::Subtract(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Difference(Difference {
                    a: Self::compile(v1, variables, expressions, template, dst_var),
                    b: Self::compile(v2, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::Multiply(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Product(Product {
                    a: Self::compile(v1, variables, expressions, template, dst_var),
                    b: Self::compile(v2, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::Divide(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Quotient(Quotient {
                    a: Self::compile(v1, variables, expressions, template, dst_var),
                    b: Self::compile(v2, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::ThreePointAngle(v1, v2, v3) => Arc::new(Expression::new(
                ScalarExpr::AnglePoint(AnglePoint {
                    arm1: Expression::compile(v1, variables, expressions, template, dst_var),
                    origin: Expression::compile(v2, variables, expressions, template, dst_var),
                    arm2: Expression::compile(v3, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::TwoLineAngle(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::AngleLine(AngleLine {
                    k: Expression::compile(v1, variables, expressions, template, dst_var),
                    l: Expression::compile(v2, variables, expressions, template, dst_var),
                }),
                1.0,
            )),
            UnrolledExpressionData::Average(exprs) => Arc::new(Expression::new(
                ScalarExpr::Average(Average {
                    items: exprs
                        .iter()
                        .map(|expr| Self::compile(expr, variables, expressions, template, dst_var))
                        .collect(),
                }),
                1.0,
            )),
            _ => unreachable!("A scalar should never be compiled this way"),
        };

        // We insert for memory.
        expressions.scalars.insert(key, Arc::clone(&compiled));
        compiled
    }
}

/// Attempts to compile the variable. If the variable is a `PointCollection`, leaves it unrolled. Otherwise everything is compiled properly.
fn compile_variable<T>(
    var: &Rc<Variable>,
    variables: &mut VariableRecord,
    expressions: &mut ExpressionRecord,
    template: &mut Vec<AdjustableTemplate>,
    dst_var: &Option<Rc<Variable>>,
) -> Arc<Expression<T>>
where
    VariableRecord: Mapping<Variable, T>,
    Expression<T>: Compile,
{
    // We first have to see if the variable already exists.
    let key = HashableRc::new(Rc::clone(var));

    if let Some(v) = variables.get(&key) {
        // So we can return it here.
        return Arc::clone(v);
    }

    // And otherwise compile it.
    let compiled = Expression::compile(&var.definition, variables, expressions, template, dst_var);

    // We insert for memory
    variables.insert(HashableRc::new(Rc::clone(var)), compiled.clone());
    compiled
}

fn compile_rules(
    unrolled: Vec<UnrolledRule>,
    variables: &mut VariableRecord,
    expressions: &mut ExpressionRecord,
    template: &mut Vec<AdjustableTemplate>,
    dst_var: &Option<Rc<Variable>>,
) -> Vec<Criteria> {
    unrolled
        .into_iter()
        .map(|rule| {
            let crit = match rule.kind {
                UnrolledRuleKind::Eq => {
                    if rule.lhs.ty == ty::POINT {
                        let lhs = Expression::compile(
                            &rule.lhs,
                            variables,
                            expressions,
                            template,
                            dst_var,
                        );
                        let rhs = Expression::compile(
                            &rule.rhs,
                            variables,
                            expressions,
                            template,
                            dst_var,
                        );

                        Weighed::one(CriteriaKind::EqualPoint(lhs, rhs))
                    } else {
                        let lhs = Expression::compile(
                            &rule.lhs,
                            variables,
                            expressions,
                            template,
                            dst_var,
                        );
                        let rhs = Expression::compile(
                            &rule.rhs,
                            variables,
                            expressions,
                            template,
                            dst_var,
                        );

                        Weighed::one(CriteriaKind::EqualScalar(lhs, rhs))
                    }
                }
                UnrolledRuleKind::Gt => {
                    let lhs =
                        Expression::compile(&rule.lhs, variables, expressions, template, dst_var);
                    let rhs =
                        Expression::compile(&rule.rhs, variables, expressions, template, dst_var);

                    Weighed::one(CriteriaKind::Greater(lhs, rhs))
                }
                UnrolledRuleKind::Lt => {
                    let lhs =
                        Expression::compile(&rule.lhs, variables, expressions, template, dst_var);
                    let rhs =
                        Expression::compile(&rule.rhs, variables, expressions, template, dst_var);

                    Weighed::one(CriteriaKind::Less(lhs, rhs))
                }
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

fn read_flags(flags: &HashMap<String, Flag>) -> Result<Flags, Error> {
    let distance_literals = &flags["distance_literals"];

    Ok(Flags {
        optimizations: Optimizations {
            identical_expressions: flags["optimizations"].as_set().unwrap()
                ["identical_expressions"]
                .as_bool()
                .unwrap(),
        },
        distance_literals: match distance_literals.as_ident().unwrap().as_str() {
            "none" => DistanceLiterals::None,
            "adjust" => DistanceLiterals::Adjust,
            "solve" => DistanceLiterals::Solve,
            t => {
                return Err(Error::FlagEnumInvalidValue {
                    error_span: distance_literals.get_span().unwrap(),
                    available_values: &["none", "adjust", "solve"],
                    received_value: t.to_string(),
                })
            }
        },
        point_bounds: flags["point_bounds"].as_bool().unwrap(),
    })
}

/// Get the distance variable.
/// 
/// # Errors
/// Returns an error related to the distances flag.
/// 
/// # Panics
/// Should never panic.
pub fn get_dst_variable(context: &mut CompileContext, unrolled: &[UnrolledRule], flags: &Flags) -> Result<Option<Rc<Variable>>, Error> {
    // Check if there's a distance literal in variables or rules.
    // In variables
    let are_literals_present = {
        context
            .variables
            .values()
            .map(|var| var.definition.has_distance_literal())
            .find(Option::is_some)
            .flatten()
            .or_else(|| {
                unrolled
                    .iter()
                    .map(|rule| {
                        rule.lhs
                            .has_distance_literal()
                            .or_else(|| rule.rhs.has_distance_literal())
                    })
                    .find(Option::is_some)
                    .flatten()
            })
    };

    Ok(if let Some(at) = are_literals_present {
        match flags.distance_literals {
            DistanceLiterals::Adjust => {
                // To handle adjusted distance, we create a new adjustable variable that will pretend to be the scale.
                Some(Rc::clone(
                    context
                        .variables
                        .entry(String::from("@distance"))
                        .or_insert_with(|| {
                            Rc::new(Variable {
                                name: String::from("@distance"),
                                definition_span: span!(0, 0, 0, 0),
                                definition: UnrolledExpression {
                                    weight: 0.1, // We reduce the weight of distance to reduce its movement.
                                    data: Rc::new(UnrolledExpressionData::FreeReal),
                                    ty: ty::SCALAR,
                                    span: span!(0, 0, 0, 0),
                                }
                            })
                        }),
                ))
            }
            DistanceLiterals::Solve => {
                return Err(Error::FetureNotSupported {
                    error_span: context
                        .flags
                        .get(&String::from("distance_literals"))
                        .unwrap()
                        .get_span()
                        .unwrap(),
                    feature_name: "solve_distance",
                })
            }
            DistanceLiterals::None => {
                return Err(Error::RequiredFlagNotSet {
                    flag_name: "distance_literals",
                    required_because: at,
                    flagdef_span: None,
                    available_values: &["adjust", "solve"],
                })
            }
        }
    } else {
        None
    })
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
) -> Result<
    (
        Vec<Criteria>,
        Figure,
        Vec<AdjustableTemplate>,
        generator::Flags,
    ),
    Error,
> {
    let mut figure = Figure {
        canvas_size,
        ..Default::default()
    };

    // First, we have to unroll the script.
    let (unrolled, mut context) = unroll::unroll(input, &mut figure)?;

    let flags = read_flags(&context.flags)?;

    let dst_var = get_dst_variable(&mut context, &unrolled, &flags)?;

    // Print variables (debugging)
    // for var in context.variables.values() {
    //     println!("let {} = {}", var.name, var.definition);
    // }

    // Print rules (debugging)
    // for rule in &unrolled {
    //     println!("{rule}");
    // }

    let mut variables = VariableRecord::default();
    let mut expressions = ExpressionRecord::default();
    let mut template = Vec::new();

    // We precompile all variables.
    for (_, var) in context.variables {
        match var.definition.ty {
            Type::Point => {
                compile_variable::<PointExpr>(
                    &var,
                    &mut variables,
                    &mut expressions,
                    &mut template,
                    &dst_var,
                );
            }
            Type::Line => {
                compile_variable::<LineExpr>(
                    &var,
                    &mut variables,
                    &mut expressions,
                    &mut template,
                    &dst_var,
                );
            }
            Type::Scalar(_) => {
                compile_variable::<ScalarExpr>(
                    &var,
                    &mut variables,
                    &mut expressions,
                    &mut template,
                    &dst_var,
                );
            }
            Type::PointCollection(_) => (),
            Type::Circle => {
                compile_variable::<CircleExpr>(
                    &var,
                    &mut variables, 
                    &mut expressions, 
                    &mut template, 
                    &dst_var
                );
            }
            Type::Undefined => unreachable!("Undefined should never be compiled.")
        }
    }

    // And compile the rules
    let mut criteria = compile_rules(
        unrolled,
        &mut variables,
        &mut expressions,
        &mut template,
        &dst_var,
    );

    if let Some(dst) = &dst_var {
        // It's worth noting, that assigning a smaller weight will never be enough. We have to also bias the quality.
        let dst: Arc<Expression<ScalarExpr>> = compile_variable(
            dst,
            &mut variables,
            &mut expressions,
            &mut template,
            &dst_var,
        );
        let dst_any = Arc::new(Expression {
            kind: AnyExpr::Scalar(dst.kind.clone()),
            weights: dst.weights.clone(),
        });

        criteria.push(Weighed {
            object: CriteriaKind::Bias(dst_any),
            weight: 10.0, // The bias.
        });
    }

    // Add standard bounds
    add_bounds(&template, &mut criteria, &flags);

    // Print the compiled (debugging)
    // for rule in &criteria {
    //     println!("{rule:?}");
    // }

    Ok((criteria, figure, template, flags))
}

/// Inequality principle and the point plane limit.
fn add_bounds(
    template: &[AdjustableTemplate],
    criteria: &mut Vec<Weighed<CriteriaKind>>,
    flags: &Flags,
) {
    // Point inequality principle.
    for (i, _) in template.iter().enumerate().filter(|v: _| v.1.is_point()) {
        // For each of the next points, add an inequality rule.
        for (j, _) in template
            .iter()
            .enumerate()
            .skip(i + 1)
            .filter(|v: _| v.1.is_point())
        {
            // println!("{i} != {j}");
            criteria.push(Weighed {
                object: CriteriaKind::Inverse(Box::new(CriteriaKind::EqualPoint(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: PointExpr::Free(FreePoint { index: i }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::one_at(j),
                        kind: PointExpr::Free(FreePoint { index: j }),
                    }),
                ))),
                weight: 1.0,
            });
        }

        if flags.point_bounds {
            // For each point, add a rule limiting its range.
            criteria.push(Weighed {
                object: CriteriaKind::Greater(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointX(PointX {
                            point: Arc::new(Expression {
                                weights: Weights::one_at(i),
                                kind: PointExpr::Free(FreePoint { index: i }),
                            }),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 0.0 }),
                    }),
                ),
                weight: 1.0,
            }); // x > 0

            criteria.push(Weighed {
                object: CriteriaKind::Greater(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointY(PointY {
                            point: Arc::new(Expression {
                                weights: Weights::one_at(i),
                                kind: PointExpr::Free(FreePoint { index: i }),
                            }),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 1.0 }),
                    }),
                ),
                weight: 1.0,
            }); // y > 0

            criteria.push(Weighed {
                object: CriteriaKind::Less(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointX(PointX {
                            point: Arc::new(Expression {
                                weights: Weights::one_at(i),
                                kind: PointExpr::Free(FreePoint { index: i }),
                            }),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 1.0 }),
                    }),
                ),
                weight: 1.0,
            }); // x < 1

            criteria.push(Weighed {
                object: CriteriaKind::Less(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointY(PointY {
                            point: Arc::new(Expression {
                                weights: Weights::one_at(i),
                                kind: PointExpr::Free(FreePoint { index: i }),
                            }),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 1.0 }),
                    }),
                ),
                weight: 1.0,
            }); // y < 1
        }
    }
}
