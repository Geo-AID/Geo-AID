use std::{collections::HashMap, rc::Rc, sync::Arc, unreachable, cell::RefCell, mem};

use crate::{
    generator::{
        self,
        expression::{
            expr::{
                AngleBisector, AngleLine, AnglePoint, Average, CenterRadius, Difference, FreePoint,
                LineLineIntersection, LinePoint, Literal, Negation, ParallelThrough,
                PerpendicularThrough, PointLineDistance, PointPointDistance, PointX, PointY,
                Product, Quotient, SetUnit, Sum, Real, PointOnCircle,
            },
            AnyExpr, CircleExpr, Expression, LineExpr, PointExpr, ScalarExpr, Weights,
        },
        AdjustableTemplate, DistanceLiterals, Flags, Optimizations,
    },
    span,
};

use super::{
    figure::Figure,
    parser::Type,
    ty,
    unroll::{
        self, Flag, UnrolledExpression, UnrolledExpressionData,
        UnrolledRuleKind, Variable, CompileContext, PointMeta, Entity,
        EntScalar, EntCircle, EntLine, EntPoint
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
    circles: HashMap<HashableRc<UnrolledExpressionData>, Arc<Expression<CircleExpr>>>,
}

#[derive(Debug, Default)]
struct VariableRecord {
    points: HashMap<HashableRc<RefCell<Variable>>, Arc<Expression<PointExpr>>>,
    lines: HashMap<HashableRc<RefCell<Variable>>, Arc<Expression<LineExpr>>>,
    scalars: HashMap<HashableRc<RefCell<Variable>>, Arc<Expression<ScalarExpr>>>,
    circles: HashMap<HashableRc<RefCell<Variable>>, Arc<Expression<CircleExpr>>>,
}

impl Mapping<RefCell<Variable>, PointExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<RefCell<Variable>>) -> Option<&Arc<Expression<PointExpr>>> {
        self.points.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable>>,
        value: Arc<Expression<PointExpr>>,
    ) -> Option<Arc<Expression<PointExpr>>> {
        self.points.insert(key, value)
    }
}

impl Mapping<RefCell<Variable>, LineExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<RefCell<Variable>>) -> Option<&Arc<Expression<LineExpr>>> {
        self.lines.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable>>,
        value: Arc<Expression<LineExpr>>,
    ) -> Option<Arc<Expression<LineExpr>>> {
        self.lines.insert(key, value)
    }
}

impl Mapping<RefCell<Variable>, ScalarExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<RefCell<Variable>>) -> Option<&Arc<Expression<ScalarExpr>>> {
        self.scalars.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable>>,
        value: Arc<Expression<ScalarExpr>>,
    ) -> Option<Arc<Expression<ScalarExpr>>> {
        self.scalars.insert(key, value)
    }
}

impl Mapping<RefCell<Variable>, CircleExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<RefCell<Variable>>) -> Option<&Arc<Expression<CircleExpr>>> {
        self.circles.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable>>,
        value: Arc<Expression<CircleExpr>>,
    ) -> Option<Arc<Expression<CircleExpr>>> {
        self.circles.insert(key, value)
    }
}

pub type CompiledPoint = Arc<Expression<PointExpr>>;
pub type CompiledScalar = Arc<Expression<ScalarExpr>>;

#[derive(Debug, Clone)]
pub enum CompiledEntity {
    Point(CompiledPoint),
    Scalar(CompiledScalar),
    None // Used for not-yet compiled entities.
}

impl CompiledEntity {
    pub fn as_point(&self) -> Option<&CompiledPoint> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_scalar(&self) -> Option<&CompiledScalar> {
        if let Self::Scalar(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub struct Compiler<'r> {
    pub variables: VariableRecord,
    pub expressions: ExpressionRecord,
    pub entities: Vec<CompiledEntity>,
    pub dst_var: &'r Option<Rc<RefCell<Variable>>>,
    pub context: &'r CompileContext
}

impl<'r> Compiler<'r> {

}

trait Compile<T> {
    fn compile(&mut self, expr: &UnrolledExpression) -> Arc<Expression<T>>;
}

impl<'r> Compile<PointExpr> for Compiler<'r> {
    fn compile(&mut self, expr: &UnrolledExpression) -> Arc<Self> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.points.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => {
                self.compile_variable(var)
            }
            UnrolledExpressionData::Entity(i) => {
                self.compile_entity(*i).as_point().unwrap().clone()
            }
            UnrolledExpressionData::Boxed(expr) => {
                self.compile(expr)
            }
            UnrolledExpressionData::IndexCollection(expr, index) => self.compile(&index_collection(expr, *index)),
            UnrolledExpressionData::LineLineIntersection(k, l) => Arc::new(Expression::new(
                PointExpr::LineLineIntersection(LineLineIntersection {
                    k: self.compile(k),
                    l: self.compile(l),
                }),
                1.0,
            )),
            UnrolledExpressionData::Average(exprs) => Arc::new(Expression::new(
                PointExpr::Average(Average {
                    items: exprs
                        .iter()
                        .map(|expr| self.compile(expr))
                        .collect(),
                }),
                1.0,
            )),
            _ => unreachable!("A point should never be compiled this way"),
        };

        // We insert for memory.
        self.expressions.points.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl<'r> Compile<LineExpr> for Compiler<'r> {
    fn compile(&mut self, expr: &UnrolledExpression) -> Arc<Expression<LineExpr>> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.lines.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => {
                self.compile_variable(var)
            }
            UnrolledExpressionData::Boxed(expr) => {
                self.compile(expr)
            }
            UnrolledExpressionData::LineFromPoints(p1, p2) => Arc::new(Expression::new(
                LineExpr::Line(LinePoint {
                    a: self.compile(p1),
                    b: self.compile(p2),
                }),
                1.0,
            )),
            UnrolledExpressionData::ParallelThrough(line, point) => Arc::new(Expression::new(
                LineExpr::ParallelThrough(ParallelThrough {
                    line: self.compile(line),
                    point: self.compile(point),
                }),
                1.0,
            )),
            UnrolledExpressionData::PerpendicularThrough(line, point) => Arc::new(Expression::new(
                LineExpr::PerpendicularThrough(PerpendicularThrough {
                    line: self.compile(line),
                    point: self.compile(point),
                }),
                1.0,
            )),
            UnrolledExpressionData::AngleBisector(v1, v2, v3) => Arc::new(Expression::new(
                LineExpr::AngleBisector(AngleBisector {
                    arm1: self.compile(v1),
                    origin: self.compile(v2),
                    arm2: self.compile(v3),
                }),
                1.0,
            )),
            _ => unreachable!("A line should never be compiled this way"),
        };

        // We insert for memory.
        self.expressions.lines.insert(key, Arc::clone(&compiled));
        compiled
    }
}

/// Takes the unrolled expression of type `PointCollection` and takes the point at `index`, isolating it out of the entire expression.
fn index_collection(expr: &UnrolledExpression, index: usize) -> UnrolledExpression {
    match expr.data.as_ref() {
        UnrolledExpressionData::VariableAccess(var) => index_collection(&var.borrow().definition, index),
        UnrolledExpressionData::PointCollection(col) => col.get(index).unwrap().clone(),
        UnrolledExpressionData::Boxed(expr) => index_collection(expr, index),
        _ => unreachable!("PointCollection should never be achievable by this expression."),
    }
}

impl<'r> Compile<CircleExpr> for Compiler<'r> {
    fn compile(&mut self, expr: &UnrolledExpression) -> Arc<Self> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.circles.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::Circle(center, radius) => Arc::new(Expression::new(
                CircleExpr::CenterRadius(CenterRadius {
                    center: self.compile(center),
                    radius: self.compile(radius),
                }),
                1.0,
            )),
            UnrolledExpressionData::Boxed(expr) => {
                self.compile(expr)
            }
            _ => unreachable!("A circle should never be compiled this way"),
        };

        // We insert for memory.
        self.expressions.circles.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl<'r> Compiler<'r> {
    #[must_use]
    pub fn fix_distance(
        expr: UnrolledExpression,
        power: i8,
        dst_var: &Rc<RefCell<Variable>>,
    ) -> UnrolledExpression {
        let sp = expr.span;
        let t = expr.ty;

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

    fn compile_number(
        expr: &UnrolledExpression,
        v: f64,
        variables: &mut VariableRecord,
        expressions: &mut ExpressionRecord,
        entities: &mut Vec<CompiledEntity>,
        dst_var: &Option<Rc<RefCell<Variable>>>,
        context: &CompileContext
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
                    ty: expr.ty,
                    span: expr.span,
                    data: Rc::new(UnrolledExpressionData::SetUnit(
                        UnrolledExpression {
                            weight: 1.0,
                            ty: ty::SCALAR,
                            span: expr.span,
                            data: expr.data.clone(),
                        },
                        *expr.ty
                            .as_scalar()
                            .unwrap()
                            .as_ref()
                            .unwrap()
                    )),
                },
                variables,
                expressions,
                entities,
                dst_var,
                context
            )
        }
    }
}

impl Compile for Expression<ScalarExpr> {
    #[allow(clippy::too_many_lines)]
    fn compile(
        expr: &UnrolledExpression,
        variables: &mut VariableRecord,
        expressions: &mut ExpressionRecord,
        entities: &mut Vec<CompiledEntity>,
        dst_var: &Option<Rc<RefCell<Variable>>>,
        context: &CompileContext
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
                compile_variable(var, variables, expressions, entities, dst_var, context)
            }
            UnrolledExpressionData::Number(v) => {
                compile_number(expr, *v, variables, expressions, entities, dst_var, context)
            }
            UnrolledExpressionData::Entity(i) => {
                match entities[*i].as_scalar().unwrap() {
                    CompiledScalar::Bind(expr) => Self::compile(expr, variables, expressions, entities, dst_var, context),
                    CompiledScalar::Compiled(expr) => Arc::clone(expr)
                }
            }
            UnrolledExpressionData::Boxed(expr) => {
                Self::compile(expr, variables, expressions, entities, dst_var, context)
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
                        entities,
                        dst_var,
                        context
                    ),
                    unit: *unit,
                }),
                1.0,
            )),
            UnrolledExpressionData::PointPointDistance(p1, p2) => Arc::new(Expression::new(
                ScalarExpr::PointPointDistance(PointPointDistance {
                    a: Expression::compile(p1, variables, expressions, entities, dst_var, context),
                    b: Expression::compile(p2, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::PointLineDistance(p, l) => Arc::new(Expression::new(
                ScalarExpr::PointLineDistance(PointLineDistance {
                    point: Expression::compile(p, variables, expressions, entities, dst_var, context),
                    line: Expression::compile(l, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::Negate(expr) => Arc::new(Expression::new(
                ScalarExpr::Negation(Negation {
                    value: Self::compile(expr, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::Add(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Sum(Sum {
                    a: Self::compile(v1, variables, expressions, entities, dst_var, context),
                    b: Self::compile(v2, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::Subtract(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Difference(Difference {
                    a: Self::compile(v1, variables, expressions, entities, dst_var, context),
                    b: Self::compile(v2, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::Multiply(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Product(Product {
                    a: Self::compile(v1, variables, expressions, entities, dst_var, context),
                    b: Self::compile(v2, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::Divide(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Quotient(Quotient {
                    a: Self::compile(v1, variables, expressions, entities, dst_var, context),
                    b: Self::compile(v2, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::ThreePointAngle(v1, v2, v3) => Arc::new(Expression::new(
                ScalarExpr::AnglePoint(AnglePoint {
                    arm1: Expression::compile(v1, variables, expressions, entities, dst_var, context),
                    origin: Expression::compile(v2, variables, expressions, entities, dst_var, context),
                    arm2: Expression::compile(v3, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::TwoLineAngle(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::AngleLine(AngleLine {
                    k: Expression::compile(v1, variables, expressions, entities, dst_var, context),
                    l: Expression::compile(v2, variables, expressions, entities, dst_var, context),
                }),
                1.0,
            )),
            UnrolledExpressionData::Average(exprs) => Arc::new(Expression::new(
                ScalarExpr::Average(Average {
                    items: exprs
                        .iter()
                        .map(|expr| Self::compile(expr, variables, expressions, entities, dst_var, context))
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
    var: &Rc<RefCell<Variable>>,
    variables: &mut VariableRecord,
    expressions: &mut ExpressionRecord,
    entities: &mut Vec<CompiledEntity>,
    dst_var: &Option<Rc<RefCell<Variable>>>,
    context: &CompileContext
) -> Arc<Expression<T>>
where
    VariableRecord: Mapping<RefCell<Variable>, T>,
    Expression<T>: Compile,
{
    // We first have to see if the variable already exists.
    let key = HashableRc::new(Rc::clone(var));

    if let Some(v) = variables.get(&key) {
        // So we can return it here.
        return Arc::clone(v);
    }

    // And otherwise compile it.
    let compiled = Expression::compile(&var.borrow().definition, variables, expressions, entities, dst_var, context);

    // We insert for memory
    variables.insert(HashableRc::new(Rc::clone(var)), compiled.clone());
    compiled
}

/// Compiles the entity by index.
fn compile_entity(
    entity: usize,
    variables: &mut VariableRecord,
    expressions: &mut ExpressionRecord,
    entities: &mut Vec<CompiledEntity>,
    template: &mut Vec<AdjustableTemplate>,
    dst_var: &Option<Rc<RefCell<Variable>>>,
    context: &CompileContext
) -> CompiledEntity {
    // If the expression is compiled, there's no problem
    match entities[entity].clone() {
        CompiledEntity::None => {
            entities[entity] = match context.entities[entity] {
                Entity::Scalar(v) => CompiledEntity::Scalar(match v {
                    EntScalar::Free => {
                        template.push(AdjustableTemplate::Real);
                        Arc::new(Expression::new(
                            ScalarExpr::Real(Real {
                                index: template.len() - 1
                            }),
                            1.0
                        ))
                    }
                    EntScalar::Bind(expr) => Expression::compile(&expr, variables, expressions, entities, dst_var, context),
                }),
                Entity::Point(v) => CompiledEntity::Point(match v {
                    EntPoint::Free => {
                        template.push(AdjustableTemplate::Point);
                        Arc::new(Expression::new(
                            PointExpr::Free(FreePoint {
                                index: template.len() - 1
                            }),
                            1.0
                        ))
                    }
                    EntPoint::OnCircle(circle) => {
                        template.push(AdjustableTemplate::PointOnCircle);
                        Arc::new(Expression::new(
                            PointExpr::OnCircle(PointOnCircle {
                                index: template.len() - 1,
                                circle: Expression::compile(&circle, variables, expressions, entities, dst_var, context)
                            }),
                            1.0
                        ))
                    }
                    EntPoint::Bind(expr) => Expression::compile(&expr, variables, expressions, entities, dst_var, context),
                }),
                Entity::Line(v) => match v {
                    EntLine::Bind(_) => todo!(),
                },
                Entity::Circle(v) => match v {
                    EntCircle::Bind(_) => todo!(),
                    EntCircle::Temporary => unreachable!(),
                },
            };

            entities[entity].clone()
        },
        v => v
    }
}

fn compile_rules(
    variables: &mut VariableRecord,
    expressions: &mut ExpressionRecord,
    entities: &mut Vec<CompiledEntity>,
    dst_var: &Option<Rc<RefCell<Variable>>>,
    context: &CompileContext
) -> Vec<Criteria> {
    context.rules
        .iter()
        .map(|rule| {
            let crit = match rule.kind {
                UnrolledRuleKind::Eq => {
                    if rule.lhs.ty == ty::POINT {
                        let lhs = Expression::compile(
                            &rule.lhs,
                            variables,
                            expressions,
                            entities,
                            dst_var,
                            context
                        );
                        let rhs = Expression::compile(
                            &rule.rhs,
                            variables,
                            expressions,
                            entities,
                            dst_var,
                            context
                        );

                        Weighed::one(CriteriaKind::EqualPoint(lhs, rhs))
                    } else {
                        let lhs = Expression::compile(
                            &rule.lhs,
                            variables,
                            expressions,
                            entities,
                            dst_var,
                            context
                        );
                        let rhs = Expression::compile(
                            &rule.rhs,
                            variables,
                            expressions,
                            entities,
                            dst_var,
                            context
                        );

                        Weighed::one(CriteriaKind::EqualScalar(lhs, rhs))
                    }
                }
                UnrolledRuleKind::Gt => {
                    let lhs =
                        Expression::compile(&rule.lhs, variables, expressions, entities, dst_var, context);
                    let rhs =
                        Expression::compile(&rule.rhs, variables, expressions, entities, dst_var, context);

                    Weighed::one(CriteriaKind::Greater(lhs, rhs))
                }
                UnrolledRuleKind::Lt => {
                    let lhs =
                        Expression::compile(&rule.lhs, variables, expressions, entities, dst_var, context);
                    let rhs =
                        Expression::compile(&rule.rhs, variables, expressions, entities, dst_var, context);

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
pub fn get_dst_variable(
    context: &mut CompileContext,
    flags: &Flags,
) -> Result<Option<Rc<RefCell<Variable>>>, Error> {
    // Check if there's a distance literal in variables or rules.
    // In variables
    let are_literals_present = {
        context
            .variables
            .values()
            .map(|var| var.borrow().definition.has_distance_literal())
            .find(Option::is_some)
            .flatten()
            .or_else(|| {
                context.rules
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
                let i = context.add_scalar();
                Some(Rc::clone(
                    context
                        .variables
                        .entry(String::from("@distance"))
                        .or_insert_with(|| {
                            Rc::new(RefCell::new(Variable {
                                name: String::from("@distance"),
                                definition_span: span!(0, 0, 0, 0),
                                definition: UnrolledExpression {
                                    weight: 0.1, // We reduce the weight of distance to reduce its movement.
                                    data: Rc::new(UnrolledExpressionData::Entity(i)),
                                    ty: ty::SCALAR,
                                    span: span!(0, 0, 0, 0),
                                }
                            }))
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

/// A figure before expression compilation.
#[derive(Debug, Clone, Default)]
pub struct PreFigure {
    /// Points of the figure.
    pub points: Vec<(UnrolledExpression, PointMeta)>,
    /// Lines in the figure.
    pub lines: Vec<UnrolledExpression>,
    /// Segments in the figure.
    pub segments: Vec<(UnrolledExpression, UnrolledExpression)>,
    /// Rays in the figure
    pub rays: Vec<(UnrolledExpression, UnrolledExpression)>,
    /// Circles in the figure
    pub circles: Vec<UnrolledExpression>
}

impl PreFigure {
    /// Builds an actual figure.
    fn build(
        self,
        canvas_size: (usize, usize),
        variables: &mut VariableRecord,
        expressions: &mut ExpressionRecord,
        entities: &Vec<CompiledEntity>,
        dst_var: &Option<Rc<RefCell<Variable>>>,
        context: &CompileContext
    ) -> Figure {
        Figure {
            canvas_size,
            points: self.points.into_iter().map(|(expr, meta)| (Expression::compile(
                &expr,
                variables,
                expressions,
                entities,
                dst_var,
                context
            ), meta)).collect(),
            lines: self.lines.into_iter().map(|expr| Expression::compile(
                &expr,
                variables,
                expressions,
                entities,
                dst_var,
                context
            )).collect(),
            segments: self.segments.into_iter().map(|(a, b)| (
                Expression::compile(
                    &a,
                    variables,
                    expressions,
                    entities,
                    dst_var,
                    context
                ),
                Expression::compile(
                    &b,
                    variables,
                    expressions,
                    entities,
                    dst_var,
                    context
                )
            )).collect(),
            rays: self.rays.into_iter().map(|(a, b)| (
                Expression::compile(
                    &a,
                    variables,
                    expressions,
                    entities,
                    dst_var,
                    context
                ),
                Expression::compile(
                    &b,
                    variables,
                    expressions,
                    entities,
                    dst_var,
                    context
                )
            )).collect(),
            circles: self.circles.into_iter().map(|expr| Expression::compile(
                &expr,
                variables,
                expressions,
                entities,
                dst_var,
                context
            )).collect(),
            ..Default::default()
        }
    }
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
    // First, we have to unroll the script.
    let mut context = unroll::unroll(input)?;

    let flags = read_flags(&context.flags)?;

    let dst_var = get_dst_variable(&mut context, &flags)?;

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
    let mut entities = Vec::new();

    let mut template = Vec::new();

    for entity in &context.entities {
        let compiled = match entity {
            Entity::Scalar(v) => CompiledEntity::Scalar(match v {
                EntScalar::Free => {
                    template.push(AdjustableTemplate::Real);
                    CompiledScalar::Compiled(
                        Arc::new(Expression::new(
                            ScalarExpr::Real(Real {
                                index: template.len() - 1
                            }),
                            1.0
                        ))
                    )
                }
                EntScalar::Bind(expr) => CompiledScalar::Bind(expr.clone()),
            }),
            Entity::Point(v) => CompiledEntity::Point(match v {
                EntPoint::Free => {
                    template.push(AdjustableTemplate::Point);
                    CompiledPoint::Compiled(
                        Arc::new(Expression::new(
                            PointExpr::Free(FreePoint {
                                index: template.len() - 1
                            }),
                            1.0
                        ))
                    )
                }
                EntPoint::OnCircle(circle) => {
                    template.push(AdjustableTemplate::PointOnCircle);
                    CompiledPoint::Compiled(
                        Arc::new(Expression::new(
                            PointExpr::OnCircle(PointOnCircle {
                                index: template.len() - 1
                            }),
                            1.0
                        ))
                    )
                }
                EntPoint::Bind(expr) => CompiledPoint::Bind(expr.clone()),
            }),
            Entity::Line(v) => match v {
                EntLine::Bind(_) => todo!(),
            },
            Entity::Circle(v) => match v {
                EntCircle::Bind(_) => todo!(),
                EntCircle::Temporary => unreachable!(),
            },
        };

        entities.push(compiled);
    }

    let entities = entities;

    // We precompile all variables.
    for (_, var) in &context.variables {
        match var.borrow().definition.ty {
            Type::Point => {
                compile_variable::<PointExpr>(
                    var,
                    &mut variables,
                    &mut expressions,
                    &entities,
                    &dst_var,
                    &context
                );
            }
            Type::Line => {
                compile_variable::<LineExpr>(
                    var,
                    &mut variables,
                    &mut expressions,
                    &entities,
                    &dst_var,
                    &context
                );
            }
            Type::Scalar(_) => {
                compile_variable::<ScalarExpr>(
                    var,
                    &mut variables,
                    &mut expressions,
                    &entities,
                    &dst_var,
                    &context
                );
            }
            Type::PointCollection(_) | Type::Bundle(_) => (),
            Type::Circle => {
                compile_variable::<CircleExpr>(
                    var,
                    &mut variables,
                    &mut expressions,
                    &entities,
                    &dst_var,
                    &context
                );
            }
            Type::Undefined => unreachable!("Undefined should never be compiled."),
        }
    }

    // And compile the rules
    let mut criteria = compile_rules(
        &mut variables,
        &mut expressions,
        &entities,
        &dst_var,
        &context
    );

    if let Some(dst) = &dst_var {
        // It's worth noting, that assigning a smaller weight will never be enough. We have to also bias the quality.
        let dst: Arc<Expression<ScalarExpr>> = compile_variable(
            dst,
            &mut variables,
            &mut expressions,
            &entities,
            &dst_var,
            &context
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

    let figure = mem::take(&mut context.figure);

    Ok((criteria, figure.build(canvas_size, &mut variables, &mut expressions, &entities, &dst_var, &context), template, flags))
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
