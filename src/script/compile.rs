use std::{collections::HashMap, rc::Rc, sync::Arc, unreachable, cell::RefCell, mem};

use crate::{
    generator::{
        self,
        expression::{
            expr::{
                AngleBisector, AngleLine, AnglePoint, Average, CenterRadius, Difference, FreePoint,
                LineLineIntersection, LinePoint, Literal, Negation, ParallelThrough,
                PerpendicularThrough, PointLineDistance, PointPointDistance, PointX, PointY,
                Product, Quotient, SetUnit, Sum, Real, PointOnCircle, CircleCenter, CircleRadius, AnglePointDir,
            },
            AnyExpr, CircleExpr, Expression, LineExpr, PointExpr, ScalarExpr, Weights,
        },
        AdjustableTemplate, Flags, Optimizations,
    },
    span,
};

use super::{
    figure::Figure,
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
pub type CompiledCircle = Arc<Expression<CircleExpr>>;
pub type CompiledLine = Arc<Expression<LineExpr>>;

#[derive(Debug, Clone)]
pub enum CompiledEntity {
    Point(CompiledPoint),
    Scalar(CompiledScalar),
    Circle(CompiledCircle),
    Line(CompiledLine),
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

    /// Returns `true` if the compiled entity is [`None`].
    ///
    /// [`None`]: CompiledEntity::None
    #[must_use]
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

struct Compiler {
    variables: VariableRecord,
    expressions: ExpressionRecord,
    entities: Vec<CompiledEntity>,
    dst_var: Rc<RefCell<Variable>>,
    context: CompileContext,
    template: Vec<AdjustableTemplate>
}

impl Compiler {
    #[must_use]
    pub fn new(mut context: CompileContext) -> Self {
        let dst_var = context.add_scalar();

        let dst_var = Rc::clone(
        context
            .variables
            .entry(String::from("@distance"))
            .or_insert_with(|| {
                Rc::new(RefCell::new(Variable {
                    name: String::from("@distance"),
                    definition_span: span!(0, 0, 0, 0),
                    definition: UnrolledExpression {
                        weight: 0.1, // We reduce the weight of distance to reduce its changes.
                        data: Rc::new(UnrolledExpressionData::Entity(dst_var)),
                        ty: ty::SCALAR,
                        span: span!(0, 0, 0, 0),
                    }
                }))
            }),
        );

        let mut entities = Vec::new();
        entities.resize(context.entities.len(), CompiledEntity::None);
        
        Self {
            variables: VariableRecord::default(),
            expressions: ExpressionRecord::default(),
            dst_var,
            entities,
            context,
            template: Vec::new()
        }
    }
}

trait Compile<T> {
    fn compile(&mut self, expr: &UnrolledExpression) -> Arc<Expression<T>>;
}

impl Compile<PointExpr> for Compiler {
    fn compile(&mut self, expr: &UnrolledExpression) -> CompiledPoint {
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
            UnrolledExpressionData::CircleCenter(circle) => Arc::new(Expression::new(
                PointExpr::CircleCenter(CircleCenter {
                    circle: self.compile(circle)
                }),
                1.0
            )),
            _ => unreachable!("A point should never be compiled this way"),
        };

        // We insert for memory.
        self.expressions.points.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl Compile<LineExpr> for Compiler {
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
pub fn index_collection(expr: &UnrolledExpression, index: usize) -> UnrolledExpression {
    match expr.data.as_ref() {
        UnrolledExpressionData::VariableAccess(var) => index_collection(&var.borrow().definition, index),
        UnrolledExpressionData::PointCollection(col) => col.get(index).unwrap().clone(),
        UnrolledExpressionData::Boxed(expr) => index_collection(expr, index),
        _ => unreachable!("PointCollection should never be achievable by this expression."),
    }
}

impl Compile<CircleExpr> for Compiler {
    fn compile(&mut self, expr: &UnrolledExpression) -> CompiledCircle {
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

impl Compiler {
    #[must_use]
    pub fn fix_distance(
        &self,
        expr: UnrolledExpression,
        power: i8
    ) -> UnrolledExpression {
        let sp = expr.span;
        let t = expr.ty;

        match power.cmp(&0) {
            std::cmp::Ordering::Less => UnrolledExpression {
                weight: 1.0,
                data: Rc::new(UnrolledExpressionData::Divide(
                    self.fix_distance(expr, power + 1),
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::SCALAR,
                        span: sp,
                        data: Rc::new(UnrolledExpressionData::VariableAccess(Rc::clone(&self.dst_var))),
                    },
                )),
                ty: t,
                span: sp,
            },
            std::cmp::Ordering::Equal => expr,
            std::cmp::Ordering::Greater => UnrolledExpression {
                weight: 1.0,
                data: Rc::new(UnrolledExpressionData::Multiply(
                    self.fix_distance(expr, power - 1),
                    UnrolledExpression {
                        weight: 1.0,
                        ty: ty::SCALAR,
                        span: sp,
                        data: Rc::new(UnrolledExpressionData::VariableAccess(Rc::clone(&self.dst_var))),
                    },
                )),
                ty: t,
                span: sp,
            },
        }
    }

    fn compile_number(
        &mut self,
        expr: &UnrolledExpression,
        v: f64
    ) -> Arc<Expression<ScalarExpr>> {
        if expr.ty == ty::SCALAR {
            // If a scalar, we treat it as a standard literal.
            Arc::new(Expression::new(
                ScalarExpr::Literal(Literal { value: v }),
                1.0,
            ))
        } else {
            // Otherwise we pretend it's a scalar literal inside a SetUnit.
            self.compile(
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
                }
            )
        }
    }

    /// Attempts to compile the variable. If the variable is a `PointCollection`, leaves it unrolled. Otherwise everything is compiled properly.
    fn compile_variable<T>(
        &mut self,
        var: &Rc<RefCell<Variable>>
    ) -> Arc<Expression<T>>
    where
        VariableRecord: Mapping<RefCell<Variable>, T>,
        Self: Compile<T>,
    {
        // We first have to see if the variable already exists.
        let key = HashableRc::new(Rc::clone(var));

        if let Some(v) = self.variables.get(&key) {
            // So we can return it here.
            return Arc::clone(v);
        }

        // And otherwise compile it.
        let compiled = self.compile(&var.borrow().definition);

        // We insert for memory
        self.variables.insert(HashableRc::new(Rc::clone(var)), compiled.clone());
        compiled
    }

    /// Compiles the entity by index.
    fn compile_entity(
        &mut self,
        entity: usize
    ) -> CompiledEntity {
        // If the expression is compiled, there's no problem
        match self.entities[entity].clone() {
            CompiledEntity::None => {
                let ent = self.context.entities[entity].clone();

                self.entities[entity] = match &ent {
                    Entity::Scalar(v) => CompiledEntity::Scalar(match v {
                        EntScalar::Free => {
                            self.template.push(AdjustableTemplate::Real);
                            Arc::new(Expression::new(
                                ScalarExpr::Real(Real {
                                    index: self.template.len() - 1
                                }),
                                1.0
                            ))
                        }
                        EntScalar::Bind(expr) => self.compile(expr),
                    }),
                    Entity::Point(v) => CompiledEntity::Point(match v {
                        EntPoint::Free => {
                            self.template.push(AdjustableTemplate::Point);
                            Arc::new(Expression::new(
                                PointExpr::Free(FreePoint {
                                    index: self.template.len() - 1
                                }),
                                1.0
                            ))
                        }
                        EntPoint::OnCircle(circle) => {
                            self.template.push(AdjustableTemplate::PointOnCircle);
                            Arc::new(Expression::new(
                                PointExpr::OnCircle(PointOnCircle {
                                    index: self.template.len() - 1,
                                    circle: self.compile(&circle)
                                }),
                                1.0
                            ))
                        }
                        EntPoint::Bind(expr) => self.compile(&expr),
                    }),
                    Entity::Line(v) => CompiledEntity::Line(match v {
                        EntLine::Bind(expr) => self.compile(&expr),
                    }),
                    Entity::Circle(v) => CompiledEntity::Circle(match v {
                        EntCircle::Bind(expr) => self.compile(&expr),
                        EntCircle::Temporary => unreachable!(),
                    }),
                };
    
                self.entities[entity].clone()
            },
            v => v
        }
    }

    fn compile_rules(&mut self) -> Vec<Criteria> {
        let rules = mem::take(&mut self.context.rules);

        rules
            .iter()
            .map(|rule| {
                let crit = match rule.kind {
                    UnrolledRuleKind::Eq => {
                        if rule.lhs.ty == ty::POINT {
                            let lhs = self.compile(
                                &rule.lhs
                            );
                            let rhs = self.compile(
                                &rule.rhs
                            );

                            Weighed::one(CriteriaKind::EqualPoint(lhs, rhs))
                        } else {
                            let lhs = self.compile(
                                &rule.lhs
                            );
                            let rhs = self.compile(
                                &rule.rhs
                            );

                            Weighed::one(CriteriaKind::EqualScalar(lhs, rhs))
                        }
                    }
                    UnrolledRuleKind::Gt => {
                        let lhs =
                            self.compile(&rule.lhs);
                        let rhs =
                            self.compile(&rule.rhs);

                        Weighed::one(CriteriaKind::Greater(lhs, rhs))
                    }
                    UnrolledRuleKind::Lt => {
                        let lhs =
                            self.compile(&rule.lhs);
                        let rhs =
                            self.compile(&rule.rhs);

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

    /// Builds an actual figure.
    fn build_figure(
        &mut self,
        canvas_size: (usize, usize)
    ) -> Figure {
        let figure = mem::take(&mut self.context.figure);

        Figure {
            canvas_size,
            points: figure.points.into_iter().map(|(expr, meta)| (self.compile(
                &expr
            ), meta)).collect(),
            lines: figure.lines.into_iter().map(|expr| self.compile(
                &expr
            )).collect(),
            segments: figure.segments.into_iter().map(|(a, b)| (
                self.compile(
                    &a
                ),
                self.compile(
                    &b
                )
            )).collect(),
            rays: figure.rays.into_iter().map(|(a, b)| (
                self.compile(
                    &a
                ),
                self.compile(
                    &b
                )
            )).collect(),
            circles: figure.circles.into_iter().map(|expr| self.compile(
                &expr
            )).collect(),
            ..Default::default()
        }
    }
}

impl Compile<ScalarExpr> for Compiler {
    #[allow(clippy::too_many_lines)]
    fn compile(
        &mut self,
        expr: &UnrolledExpression
    ) -> CompiledScalar {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.scalars.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => {
                self.compile_variable(var)
            }
            UnrolledExpressionData::Number(v) => {
                self.compile_number(expr, *v)
            }
            UnrolledExpressionData::Entity(i) => self.compile_entity(*i).as_scalar().unwrap().clone(),
            UnrolledExpressionData::Boxed(expr) => {
                self.compile(expr)
            }
            UnrolledExpressionData::SetUnit(expr, unit) => Arc::new(Expression::new(
                ScalarExpr::SetUnit(SetUnit {
                    value: self.compile(
                        &self.fix_distance(
                            expr.clone(),
                            unit[SimpleUnit::Distance as usize]
                                - match expr.ty.as_scalar().unwrap() {
                                    Some(unit) => unit[SimpleUnit::Distance as usize],
                                    None => 0,
                                },
                        )
                    ),
                    unit: *unit,
                }),
                1.0,
            )),
            UnrolledExpressionData::PointPointDistance(p1, p2) => Arc::new(Expression::new(
                ScalarExpr::PointPointDistance(PointPointDistance {
                    a: self.compile(p1),
                    b: self.compile(p2),
                }),
                1.0,
            )),
            UnrolledExpressionData::PointLineDistance(p, l) => Arc::new(Expression::new(
                ScalarExpr::PointLineDistance(PointLineDistance {
                    point: self.compile(p),
                    line: self.compile(l),
                }),
                1.0,
            )),
            UnrolledExpressionData::Negate(expr) => Arc::new(Expression::new(
                ScalarExpr::Negation(Negation {
                    value: self.compile(expr),
                }),
                1.0,
            )),
            UnrolledExpressionData::Add(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Sum(Sum {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                1.0,
            )),
            UnrolledExpressionData::Subtract(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Difference(Difference {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                1.0,
            )),
            UnrolledExpressionData::Multiply(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Product(Product {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                1.0,
            )),
            UnrolledExpressionData::Divide(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Quotient(Quotient {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                1.0,
            )),
            UnrolledExpressionData::ThreePointAngle(v1, v2, v3) => Arc::new(Expression::new(
                ScalarExpr::AnglePoint(AnglePoint {
                    arm1: self.compile(v1),
                    origin: self.compile(v2),
                    arm2: self.compile(v3),
                }),
                1.0,
            )),
            UnrolledExpressionData::ThreePointAngleDir(v1, v2, v3) => Arc::new(Expression::new(
                ScalarExpr::AnglePointDir(AnglePointDir {
                    arm1: self.compile(v1),
                    origin: self.compile(v2),
                    arm2: self.compile(v3),
                }),
                1.0
            )),
            UnrolledExpressionData::TwoLineAngle(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::AngleLine(AngleLine {
                    k: self.compile(v1),
                    l: self.compile(v2),
                }),
                1.0,
            )),
            UnrolledExpressionData::Average(exprs) => Arc::new(Expression::new(
                ScalarExpr::Average(Average {
                    items: exprs
                        .iter()
                        .map(|expr| self.compile(expr))
                        .collect(),
                }),
                1.0,
            )),
            UnrolledExpressionData::CircleRadius(circle) => Arc::new(Expression::new(
                ScalarExpr::CircleRadius(CircleRadius {
                    circle: self.compile(circle)
                }),
                1.0
            )),
            x => unreachable!("A scalar should never be compiled this way {x}"),
        };

        // We insert for memory.
        self.expressions.scalars.insert(key, Arc::clone(&compiled));
        compiled
    }
}

fn read_flags(flags: &HashMap<String, Flag>) -> Result<Flags, Error> {
    Ok(Flags {
        optimizations: Optimizations {
            identical_expressions: flags["optimizations"].as_set().unwrap()
                ["identical_expressions"]
                .as_bool()
                .unwrap(),
        },
        point_bounds: flags["point_bounds"].as_bool().unwrap(),
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
    let context = unroll::unroll(input)?;

    let flags = read_flags(&context.flags)?;

    // Print variables (debugging)
    // for var in context.variables.values() {
    //     println!("let {} = {}", var.name, var.definition);
    // }

    // Print rules (debugging)
    // for rule in &unrolled {
    //     println!("{rule}");
    // }

    let mut compiler = Compiler::new(context);

    // And compile the rules
    let mut criteria = compiler.compile_rules();

    // Check if dst_var is ever used.
    if let CompiledEntity::Scalar(dst) = compiler.entities.last().unwrap() {
        // It's worth noting, that assigning a smaller weight will never be enough. We have to also bias the quality.
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
    add_bounds(&compiler.template, &mut criteria, &flags);

    // Print the compiled (debugging)
    // for rule in &criteria {
    //     println!("{rule:?}");
    // }

    let figure = compiler.build_figure(canvas_size);
    Ok((criteria, figure, compiler.template, flags))
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
