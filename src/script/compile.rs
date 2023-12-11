/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc, unreachable};

use crate::generator::expression::expr::PointOnLine;
use crate::generator::fast_float::FastFloat;
use crate::script::unit;
use crate::script::unroll::{
    AnyExpr as UnrolledAny, Circle, ConvertFrom, Definition, Generic, Line, Point, Scalar,
    ScalarData,
};
use crate::{
    generator::{
        self,
        expression::{
            expr::{
                AngleBisector, AngleLine, AnglePoint, AnglePointDir, Average, CenterRadius,
                CircleCenter, CircleRadius, Difference, FreePoint, LineLineIntersection, LinePoint,
                Literal, Negation, ParallelThrough, PerpendicularThrough, PointLineDistance,
                PointOnCircle, PointPointDistance, PointX, PointY, Product, Quotient, Real,
                SetUnit, Sum,
            },
            AnyExpr, CircleExpr, Expression, LineExpr, PointExpr, ScalarExpr, Weights,
        },
        AdjustableTemplate, Flags, Optimizations,
    },
    span,
};

use super::unroll::{UnrolledRule, CollectionNode, Node, Displayed, CloneWithNode};
use super::{
    figure::Figure,
    unroll::{
        self, CompileContext, EntCircle, EntLine, EntPoint, EntScalar, Entity, Expr, Flag,
        UnrolledRuleKind, Variable
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
    points: HashMap<HashableRc<Point>, Arc<Expression<PointExpr>>>,
    lines: HashMap<HashableRc<Line>, Arc<Expression<LineExpr>>>,
    scalars: HashMap<HashableRc<Scalar>, Arc<Expression<ScalarExpr>>>,
    circles: HashMap<HashableRc<Circle>, Arc<Expression<CircleExpr>>>,
}

#[derive(Debug, Default)]
struct VariableRecord {
    points: HashMap<HashableRc<RefCell<Variable<Point>>>, Arc<Expression<PointExpr>>>,
    lines: HashMap<HashableRc<RefCell<Variable<Line>>>, Arc<Expression<LineExpr>>>,
    scalars: HashMap<HashableRc<RefCell<Variable<Scalar>>>, Arc<Expression<ScalarExpr>>>,
    circles: HashMap<HashableRc<RefCell<Variable<Circle>>>, Arc<Expression<CircleExpr>>>,
}

impl Mapping<RefCell<Variable<Point>>, PointExpr> for VariableRecord {
    fn get(
        &self,
        key: &HashableRc<RefCell<Variable<Point>>>,
    ) -> Option<&Arc<Expression<PointExpr>>> {
        self.points.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable<Point>>>,
        value: Arc<Expression<PointExpr>>,
    ) -> Option<Arc<Expression<PointExpr>>> {
        self.points.insert(key, value)
    }
}

impl Mapping<RefCell<Variable<Line>>, LineExpr> for VariableRecord {
    fn get(&self, key: &HashableRc<RefCell<Variable<Line>>>) -> Option<&Arc<Expression<LineExpr>>> {
        self.lines.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable<Line>>>,
        value: Arc<Expression<LineExpr>>,
    ) -> Option<Arc<Expression<LineExpr>>> {
        self.lines.insert(key, value)
    }
}

impl Mapping<RefCell<Variable<Scalar>>, ScalarExpr> for VariableRecord {
    fn get(
        &self,
        key: &HashableRc<RefCell<Variable<Scalar>>>,
    ) -> Option<&Arc<Expression<ScalarExpr>>> {
        self.scalars.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable<Scalar>>>,
        value: Arc<Expression<ScalarExpr>>,
    ) -> Option<Arc<Expression<ScalarExpr>>> {
        self.scalars.insert(key, value)
    }
}

impl Mapping<RefCell<Variable<Circle>>, CircleExpr> for VariableRecord {
    fn get(
        &self,
        key: &HashableRc<RefCell<Variable<Circle>>>,
    ) -> Option<&Arc<Expression<CircleExpr>>> {
        self.circles.get(key)
    }

    fn insert(
        &mut self,
        key: HashableRc<RefCell<Variable<Circle>>>,
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
    None, // Used for not-yet compiled entities.
}

impl CompiledEntity {
    #[must_use]
    pub fn as_point(&self) -> Option<&CompiledPoint> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_scalar(&self) -> Option<&CompiledScalar> {
        if let Self::Scalar(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_line(&self) -> Option<&CompiledLine> {
        if let Self::Line(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_circle(&self) -> Option<&CompiledCircle> {
        if let Self::Circle(v) = self {
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

pub struct Compiler {
    variables: VariableRecord,
    expressions: ExpressionRecord,
    entities: Vec<CompiledEntity>,
    dst_var: Expr<Scalar>,
    context: CompileContext,
    template: Vec<AdjustableTemplate>,
    // Specifically for bounds.
    adjustable_points: Vec<Arc<Expression<PointExpr>>>,
}

impl Compiler {
    #[must_use]
    pub fn new(mut context: CompileContext) -> Self {
        let dst_var = context.add_scalar();

        let dst_var = Expr {
            weight: FastFloat::Other(0.1), // We reduce the weight of distance to reduce its changes.
            data: Rc::new(Scalar {
                unit: Some(unit::DISTANCE),
                data: ScalarData::Entity(dst_var),
            }),
            span: span!(0, 0, 0, 0),
            node: None
        };

        let mut entities = Vec::new();
        entities.resize(context.entities.len(), CompiledEntity::None);

        Self {
            variables: VariableRecord::default(),
            expressions: ExpressionRecord::default(),
            dst_var,
            entities,
            context,
            template: Vec::new(),
            adjustable_points: Vec::new(),
        }
    }

    fn compile_generic<T, U>(&mut self, generic: &Generic<T>) -> Arc<Expression<U>>
    where
        T: Definition + ConvertFrom<UnrolledAny>,
        Compiler: Compile<T, U>,
        VariableRecord: Mapping<RefCell<Variable<T>>, U>,
    {
        match generic {
            Generic::VariableAccess(var) => self.compile_variable(var),
            Generic::Boxed(expr) => self.compile(expr),
            Generic::Dummy => unreachable!("dummy expression appeared in compile step")
        }
    }
}

pub trait Compile<T: Displayed, U> {
    fn compile(&mut self, expr: &Expr<T>) -> Arc<Expression<U>>;
}

impl Compile<Point, PointExpr> for Compiler {
    fn compile(&mut self, expr: &Expr<Point>) -> CompiledPoint {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.points.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            Point::Generic(generic) => self.compile_generic(generic),
            Point::Entity(i) => self.compile_entity(*i).as_point().unwrap().clone(),
            Point::LineLineIntersection(k, l) => Arc::new(Expression::new(
                PointExpr::LineLineIntersection(LineLineIntersection {
                    k: self.compile(k),
                    l: self.compile(l),
                }),
                FastFloat::One,
            )),
            Point::Average(exprs) => Arc::new(Expression::new(
                PointExpr::Average(Average {
                    items: exprs.iter().map(|expr| self.compile(expr)).collect(),
                }),
                FastFloat::One,
            )),
            Point::CircleCenter(circle) => Arc::new(Expression::new(
                PointExpr::CircleCenter(CircleCenter {
                    circle: self.compile(circle),
                }),
                FastFloat::One,
            )),
        };

        // We insert for memory.
        self.expressions.points.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl Compile<Line, LineExpr> for Compiler {
    fn compile(&mut self, expr: &Expr<Line>) -> Arc<Expression<LineExpr>> {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.lines.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match expr.data.as_ref() {
            Line::Generic(generic) => self.compile_generic(generic),
            Line::Entity(i) => self.compile_entity(*i).as_line().unwrap().clone(),
            Line::LineFromPoints(p1, p2) => Arc::new(Expression::new(
                LineExpr::Line(LinePoint {
                    a: self.compile(p1),
                    b: self.compile(p2),
                }),
                FastFloat::One,
            )),
            Line::ParallelThrough(line, point) => Arc::new(Expression::new(
                LineExpr::ParallelThrough(ParallelThrough {
                    line: self.compile(line),
                    point: self.compile(point),
                }),
                FastFloat::One,
            )),
            Line::PerpendicularThrough(line, point) => Arc::new(Expression::new(
                LineExpr::PerpendicularThrough(PerpendicularThrough {
                    line: self.compile(line),
                    point: self.compile(point),
                }),
                FastFloat::One,
            )),
            Line::AngleBisector(v1, v2, v3) => Arc::new(Expression::new(
                LineExpr::AngleBisector(AngleBisector {
                    arm1: self.compile(v1),
                    origin: self.compile(v2),
                    arm2: self.compile(v3),
                }),
                FastFloat::One,
            )),
        };

        // We insert for memory.
        self.expressions.lines.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl Compile<Circle, CircleExpr> for Compiler {
    fn compile(&mut self, expr: &Expr<Circle>) -> CompiledCircle {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.circles.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        let compiled = match expr.data.as_ref() {
            Circle::Generic(generic) => self.compile_generic(generic),
            Circle::Circle(center, radius) => Arc::new(Expression::new(
                CircleExpr::CenterRadius(CenterRadius {
                    center: self.compile(center),
                    radius: self.compile(radius),
                }),
                FastFloat::One,
            )),
            Circle::Entity(i) => self.compile_entity(*i).as_circle().unwrap().clone(),
        };

        // We insert for memory.
        self.expressions.circles.insert(key, Arc::clone(&compiled));
        compiled
    }
}

impl Compiler {
    #[must_use]
    pub fn fix_distance(&self, expr: Expr<Scalar>, power: i8) -> Expr<Scalar> {
        let sp = expr.span;
        let u = expr.data.unit;

        match power.cmp(&0) {
            std::cmp::Ordering::Less =>
                Expr {
                    weight: FastFloat::One,
                    data: Rc::new(Scalar {
                        unit: u,
                        data: ScalarData::Divide(
                            self.fix_distance(expr, power + 1),
                            self.dst_var.clone_without_node(),
                        ),
                    }),
                    span: sp,
                    node: None
                },
            std::cmp::Ordering::Equal => expr,
            std::cmp::Ordering::Greater => 
                Expr {
                    weight: FastFloat::One,
                    data: Rc::new(Scalar {
                        unit: u,
                        data: ScalarData::Multiply(
                            self.fix_distance(expr, power - 1),
                            self.dst_var.clone_without_node(),
                        ),
                    }),
                    span: sp,
                    node: None
                },
        }
    }

    fn compile_number(&mut self, expr: &Expr<Scalar>, v: f64) -> Arc<Expression<ScalarExpr>> {
        if expr.data.unit == Some(unit::SCALAR) {
            // If a scalar, we treat it as a standard literal.
            Arc::new(Expression::new(
                ScalarExpr::Literal(Literal { value: v }),
                FastFloat::One,
            ))
        } else {
            // Otherwise we pretend it's a scalar literal inside a SetUnit.
            self.compile(&Expr {
                weight: FastFloat::One,
                span: expr.span,
                data: Rc::new(Scalar {
                    unit: expr.data.unit,
                    data: ScalarData::SetUnit(
                        Expr {
                            weight: FastFloat::One,
                            span: expr.span,
                            data: Rc::new(Scalar {
                                unit: Some(unit::SCALAR),
                                data: expr.data.data.clone_without_node(),
                            }),
                            node: None
                        },
                        expr.data.unit.unwrap(),
                    ),
                }),
                node: None
            })
        }
    }

    /// Attempts to compile the variable. If the variable is a `PointCollection`, leaves it unrolled. Otherwise everything is compiled properly.
    fn compile_variable<T: Displayed, U>(&mut self, var: &Rc<RefCell<Variable<T>>>) -> Arc<Expression<U>>
    where
        VariableRecord: Mapping<RefCell<Variable<T>>, U>,
        Self: Compile<T, U>,
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
        self.variables
            .insert(HashableRc::new(Rc::clone(var)), compiled.clone());
        compiled
    }

    /// Compiles the entity by index.
    #[must_use]
    fn compile_entity(&mut self, entity: usize) -> CompiledEntity {
        // If the expression is compiled, there's no problem
        match self.entities[entity].clone() {
            CompiledEntity::None => {
                let ent = self.context.entities[entity].clone_without_node();

                self.entities[entity] = match &ent {
                    Entity::Scalar(v) => CompiledEntity::Scalar(match v {
                        EntScalar::Free => {
                            self.template.push(AdjustableTemplate::Real);
                            Arc::new(Expression::new(
                                ScalarExpr::Real(Real {
                                    index: self.template.len() - 1,
                                }),
                                FastFloat::One,
                            ))
                        }
                        EntScalar::Bind(expr) => self.compile(expr),
                    }),
                    Entity::Point(v) => CompiledEntity::Point(match v {
                        EntPoint::Free => {
                            self.template.push(AdjustableTemplate::Point);
                            let expr = Arc::new(Expression::new(
                                PointExpr::Free(FreePoint {
                                    index: self.template.len() - 1,
                                }),
                                FastFloat::One,
                            ));
                            self.adjustable_points.push(Arc::clone(&expr));
                            expr
                        }
                        EntPoint::OnCircle(circle) => {
                            self.template.push(AdjustableTemplate::PointOnCircle);
                            let expr = Arc::new(Expression::new(
                                PointExpr::OnCircle(PointOnCircle {
                                    index: self.template.len() - 1,
                                    circle: self.compile(circle),
                                }),
                                FastFloat::One,
                            ));
                            self.adjustable_points.push(Arc::clone(&expr));
                            expr
                        }
                        EntPoint::OnLine(line) => {
                            self.template.push(AdjustableTemplate::PointOnLine);
                            let expr = Arc::new(Expression::new(
                                PointExpr::OnLine(PointOnLine {
                                    index: self.template.len() - 1,
                                    line: self.compile(line),
                                }),
                                FastFloat::One,
                            ));
                            self.adjustable_points.push(Arc::clone(&expr));
                            expr
                        }
                        EntPoint::Bind(expr) => self.compile(expr),
                    }),
                    Entity::Line(v) => CompiledEntity::Line(match v {
                        EntLine::Bind(expr) => self.compile(expr),
                    }),
                    Entity::Circle(v) => CompiledEntity::Circle(match v {
                        EntCircle::Bind(expr) => self.compile(expr),
                        EntCircle::Temporary => unreachable!(),
                    }),
                };

                self.entities[entity].clone()
            }
            v => v,
        }
    }

    fn compile_rule_vec(&mut self, rules: &[UnrolledRule]) -> Vec<Criteria> {
        rules.iter().flat_map(|rule| self.compile_rule(rule)).collect()
    }

    fn compile_rule(&mut self, rule: &UnrolledRule) -> Option<Criteria> {
        let crit = match &rule.kind {
            UnrolledRuleKind::PointEq(lhs, rhs) => {
                let lhs = self.compile(lhs);
                let rhs = self.compile(rhs);

                Weighed::one(CriteriaKind::EqualPoint(lhs, rhs))
            }
            UnrolledRuleKind::ScalarEq(lhs, rhs) => {
                let lhs = self.compile(lhs);
                let rhs = self.compile(rhs);

                Weighed::one(CriteriaKind::EqualScalar(lhs, rhs))
            }
            UnrolledRuleKind::Gt(lhs, rhs) => {
                let lhs = self.compile(lhs);
                let rhs = self.compile(rhs);

                Weighed::one(CriteriaKind::Greater(lhs, rhs))
            }
            UnrolledRuleKind::Lt(lhs, rhs) => {
                let lhs = self.compile(lhs);
                let rhs = self.compile(rhs);

                Weighed::one(CriteriaKind::Less(lhs, rhs))
            }
            UnrolledRuleKind::Alternative(rules) => {
                Weighed::one(CriteriaKind::Alternative(self.compile_rule_vec(rules)))
            }
            UnrolledRuleKind::Display => return None
        };

        Some(if rule.inverted {
            Weighed {
                object: CriteriaKind::Inverse(Box::new(crit.object)),
                weight: crit.weight,
            }
        } else {
            crit
        })
    }

    #[must_use]
    fn compile_rules(&mut self) -> Vec<Criteria> {
        let rules = self.context.take_rules();

        self.compile_rule_vec(&rules)
    }

    /// Builds an actual figure.
    fn build_figure(&mut self, figure: CollectionNode, canvas_size: (usize, usize)) -> Figure {
        let mut compiled = Figure {
            canvas_size,
            ..Default::default()
        };

        figure.build(self, &mut compiled);

        compiled
    }
}

impl Compile<Scalar, ScalarExpr> for Compiler {
    #[allow(clippy::too_many_lines)]
    fn compile(&mut self, expr: &Expr<Scalar>) -> CompiledScalar {
        // First we have to check if this expression has been compiled already.
        let key = HashableRc::new(Rc::clone(&expr.data));

        if let Some(v) = self.expressions.scalars.get(&key) {
            // If so, we return it.
            return Arc::clone(v);
        }

        // Otherwise we compile.
        let compiled = match &expr.data.data {
            ScalarData::Generic(generic) => self.compile_generic(generic),
            ScalarData::Number(v) => self.compile_number(expr, *v),
            ScalarData::DstLiteral(v) => Arc::new(Expression::new(
                ScalarExpr::SetUnit(SetUnit {
                    unit: unit::DISTANCE,
                    value: Arc::new(Expression::new(
                        ScalarExpr::Literal(Literal { value: *v }),
                        FastFloat::One,
                    )),
                }),
                FastFloat::One,
            )),
            ScalarData::Entity(i) => self.compile_entity(*i).as_scalar().unwrap().clone(),
            ScalarData::SetUnit(expr, unit) => Arc::new(Expression::new(
                ScalarExpr::SetUnit(SetUnit {
                    value: self.compile(&self.fix_distance(
                        expr.clone_without_node(),
                        unit[SimpleUnit::Distance as usize]
                            - match expr.data.unit {
                                Some(unit) => unit[SimpleUnit::Distance as usize],
                                None => 0,
                            },
                    )),
                    unit: *unit,
                }),
                FastFloat::One,
            )),
            ScalarData::PointPointDistance(p1, p2) => Arc::new(Expression::new(
                ScalarExpr::PointPointDistance(PointPointDistance {
                    a: self.compile(p1),
                    b: self.compile(p2),
                }),
                FastFloat::One,
            )),
            ScalarData::PointLineDistance(p, l) => Arc::new(Expression::new(
                ScalarExpr::PointLineDistance(PointLineDistance {
                    point: self.compile(p),
                    line: self.compile(l),
                }),
                FastFloat::One,
            )),
            ScalarData::Negate(expr) => Arc::new(Expression::new(
                ScalarExpr::Negation(Negation {
                    value: self.compile(expr),
                }),
                FastFloat::One,
            )),
            ScalarData::Add(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Sum(Sum {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                FastFloat::One,
            )),
            ScalarData::Subtract(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Difference(Difference {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                FastFloat::One,
            )),
            ScalarData::Multiply(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Product(Product {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                FastFloat::One,
            )),
            ScalarData::Divide(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::Quotient(Quotient {
                    a: self.compile(v1),
                    b: self.compile(v2),
                }),
                FastFloat::One,
            )),
            ScalarData::ThreePointAngle(v1, v2, v3) => Arc::new(Expression::new(
                ScalarExpr::AnglePoint(AnglePoint {
                    arm1: self.compile(v1),
                    origin: self.compile(v2),
                    arm2: self.compile(v3),
                }),
                FastFloat::One,
            )),
            ScalarData::ThreePointAngleDir(v1, v2, v3) => Arc::new(Expression::new(
                ScalarExpr::AnglePointDir(AnglePointDir {
                    arm1: self.compile(v1),
                    origin: self.compile(v2),
                    arm2: self.compile(v3),
                }),
                FastFloat::One,
            )),
            ScalarData::TwoLineAngle(v1, v2) => Arc::new(Expression::new(
                ScalarExpr::AngleLine(AngleLine {
                    k: self.compile(v1),
                    l: self.compile(v2),
                }),
                FastFloat::One,
            )),
            ScalarData::Average(exprs) => Arc::new(Expression::new(
                ScalarExpr::Average(Average {
                    items: exprs.iter().map(|expr| self.compile(expr)).collect(),
                }),
                FastFloat::One,
            )),
            ScalarData::CircleRadius(circle) => Arc::new(Expression::new(
                ScalarExpr::CircleRadius(CircleRadius {
                    circle: self.compile(circle),
                }),
                FastFloat::One,
            )),
        };

        // We insert for memory.
        self.expressions.scalars.insert(key, Arc::clone(&compiled));
        compiled
    }
}

fn read_flags(flags: &HashMap<String, Flag>) -> Flags {
    Flags {
        optimizations: Optimizations {
            identical_expressions: flags["optimizations"].as_set().unwrap()
                ["identical_expressions"]
                .as_bool()
                .unwrap(),
        },
        point_bounds: flags["point_bounds"].as_bool().unwrap(),
    }
}

#[derive(Debug)]
pub struct CompileResult {
    pub criteria: Vec<Criteria>,
    pub figure: Figure,
    pub template: Vec<AdjustableTemplate>,
    pub flags: generator::Flags
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
    CompileResult,
    Vec<Error>
> {
    // First, we have to unroll the script.
    let (context, figure) = unroll::unroll(input)?;

    let flags = read_flags(&context.flags);

    // Print rules (debugging)
    // for rule in &context.rules {
    //     println!("{}: {rule}", rule.inverted);
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
            weight: FastFloat::Other(10.0), // The bias.
        });
    }

    // println!("{:#?}", criteria);

    // Add standard bounds
    add_bounds(
        &compiler.template,
        &compiler.adjustable_points,
        &mut criteria,
        &flags,
    );

    // Print the compiled (debugging)
    // for rule in &criteria {
    //     println!("{rule:?}");
    // }

    let figure = compiler.build_figure(figure, canvas_size);
    Ok(CompileResult {
        criteria,
        figure,
        template: compiler.template,
        flags
    })
}

/// Inequality principle and the point plane limit.
fn add_bounds(
    template: &[AdjustableTemplate],
    points: &[Arc<Expression<PointExpr>>],
    criteria: &mut Vec<Weighed<CriteriaKind>>,
    flags: &Flags,
) {
    // Point inequality principle.
    for (i, _) in template.iter().enumerate().filter(|v: _| v.1.is_point()) {
        let point = Arc::clone(&points[i]);

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
                    Arc::clone(&point),
                    Arc::clone(&points[j]),
                ))),
                weight: FastFloat::One,
            });
        }

        if flags.point_bounds {
            // For each point, add a rule limiting its range.
            criteria.push(Weighed {
                object: CriteriaKind::Greater(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointX(PointX {
                            point: Arc::clone(&point),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 0.0 }),
                    }),
                ),
                weight: FastFloat::One,
            }); // x > 0

            criteria.push(Weighed {
                object: CriteriaKind::Greater(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointY(PointY {
                            point: Arc::clone(&point),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 1.0 }),
                    }),
                ),
                weight: FastFloat::One,
            }); // y > 0

            criteria.push(Weighed {
                object: CriteriaKind::Less(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointX(PointX {
                            point: Arc::clone(&point),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 1.0 }),
                    }),
                ),
                weight: FastFloat::One,
            }); // x < 1

            criteria.push(Weighed {
                object: CriteriaKind::Less(
                    Arc::new(Expression {
                        weights: Weights::one_at(i),
                        kind: ScalarExpr::PointY(PointY {
                            point: Arc::clone(&point),
                        }),
                    }),
                    Arc::new(Expression {
                        weights: Weights::empty(),
                        kind: ScalarExpr::Literal(Literal { value: 1.0 }),
                    }),
                ),
                weight: FastFloat::One,
            }); // y < 1
        }
    }
}
