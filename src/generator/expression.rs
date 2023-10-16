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

use std::{
    cell::RefCell,
    ops::{Add, AddAssign, Mul},
    sync::Arc,
};

use crate::generator::expression::expr::PointOnLine;
use crate::generator::fast_float::FastFloat;
use serde::Serialize;

use self::expr::{
    AngleBisector, AngleLine, AnglePoint, AnglePointDir, Average, CenterRadius, CircleCenter,
    CircleRadius, Difference, FreePoint, LineLineIntersection, LinePoint, Literal, Negation,
    ParallelThrough, PerpendicularThrough, PointLineDistance, PointOnCircle, PointPointDistance,
    PointX, PointY, Product, Quotient, Real, SetUnit, Sum,
};

use super::{critic::EvaluationArgs, Complex};

#[derive(Debug, Clone)]
pub struct ExprCache {
    pub value: Value,
    pub generation: u64,
}

/// A utility for `Weights`.
#[derive(Debug, Clone, Serialize)]
pub struct Weights(pub Vec<FastFloat>);

impl Weights {
    #[must_use]
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    #[must_use]
    pub fn one_at(index: usize) -> Self {
        let mut v = Vec::new();
        v.resize(index + 1, FastFloat::Zero);

        v[index] = FastFloat::One;

        Self(v)
    }

    pub fn set(&mut self, index: usize, weight: FastFloat) {
        self.0.resize(index + 1, FastFloat::Zero);
        self.0[index] = weight;
    }
}

impl Add<&Weights> for Weights {
    type Output = Weights;

    fn add(mut self, rhs: &Self) -> Self::Output {
        self.add_assign(rhs);
        self
    }
}

impl AddAssign<&Weights> for Weights {
    fn add_assign(&mut self, rhs: &Self) {
        for (i, w) in rhs.0.iter().enumerate() {
            if let Some(v) = self.0.get_mut(i) {
                *v += *w;
            } else {
                self.set(i, *w);
            }
        }
    }
}

impl Mul<FastFloat> for Weights {
    type Output = Weights;

    fn mul(self, rhs: FastFloat) -> Self::Output {
        Weights(self.0.into_iter().map(|v| v * rhs).collect())
    }
}

pub trait Kind {
    fn collect(&self, exprs: &mut Vec<usize>);

    /// Trivial expressions are ones that don't require any calculations being made.
    /// Trivial expressions should not be cached, as it is much faster to evaluate them
    /// than to get their caches.
    #[must_use]
    fn is_trivial(&self) -> bool;
}

/// An expression is a base construct of Geo-AID. Contains a cache, saved weights and the expression kind itself.
#[derive(Debug, Clone, Serialize)]
pub struct Expression<T> {
    /// Saved weights
    pub weights: Weights,
    /// Expression kind.
    pub kind: T,
}

impl<T: Evaluate + Kind> Expression<T>
where
    T::Output: From<Value> + Into<Value> + Clone,
{
    #[must_use]
    pub fn new(expr: T, weight: FastFloat) -> Self {
        Self {
            weights: expr.evaluate_weights() * weight,
            kind: expr,
        }
    }

    /// Gets the expressions address.
    #[must_use]
    pub fn get_address(self: &Arc<Self>) -> usize {
        Arc::downgrade(self).as_ptr().cast::<()>() as usize
    }

    /// Evaluates the expression.
    ///
    /// # Errors
    /// Any errors related to failure when evaluating.
    pub fn evaluate(self: &Arc<Self>, args: &EvaluationArgs) -> T::Output {
        let mut mutptr: Option<*mut ExprCache> = None;

        if args.flags.optimizations.identical_expressions && Arc::strong_count(self) > 1 {
            if let Some(mut cache) = args.cache.map(RefCell::borrow_mut) {
                if let Some(expr_cache) = cache.get_mut(&self.get_address()) {
                    if expr_cache.generation == args.generation {
                        return expr_cache.value.clone().into();
                    }

                    mutptr = Some(expr_cache);
                }
            }
        }

        let v = self.kind.evaluate(args);

        match mutptr {
            Some(cache) => unsafe {
                let cache = &mut *cache;
                cache.generation = args.generation;

                let cloned = v.clone();
                cache.value = v.into();

                cloned
            },
            None => v,
        }
    }

    /// Collects expressions that should be cached.
    pub fn collect(self: &Arc<Self>, exprs: &mut Vec<usize>) {
        if self.is_trivial() && Arc::strong_count(self) > 1 {
            exprs.push(self.get_address());
        }

        self.kind.collect(exprs);
    }

    /// Trivial expressions are ones that don't require any calculations being made.
    /// Trivial expressions should not be cached, as it is much faster to evaluate them
    /// than to get their caches.
    #[must_use]
    pub fn is_trivial(&self) -> bool {
        self.kind.is_trivial()
    }
}

trait Zero {
    fn zero() -> Self;
}

impl Zero for f64 {
    fn zero() -> Self {
        0.0
    }
}

impl Zero for Complex {
    fn zero() -> Self {
        Complex::new(0.0, 0.0)
    }
}

/// Represents a line in a 2D euclidean space.
#[derive(Debug, Clone, Copy)]
pub struct Line {
    /// Line's origin as a complex number.
    pub origin: Complex,
    /// A normalized direction vector.
    pub direction: Complex,
}

impl Line {
    #[must_use]
    pub fn new(origin: Complex, direction: Complex) -> Self {
        Self { origin, direction }
    }
}

/// Represents a circle in a 2D euclidean space.
#[derive(Debug, Clone, Copy)]
pub struct Circle {
    /// Circle's center.
    pub center: Complex,
    /// Its radius
    pub radius: f64,
}

/// An evaluated value.
#[derive(Debug, Clone)]
pub enum Value {
    Point(Complex),
    Line(Line),
    Scalar(f64),
    Circle(Circle),
}

impl Value {
    //noinspection DuplicatedCode
    #[must_use]
    pub fn as_point(&self) -> Option<&Complex> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_line(&self) -> Option<&Line> {
        if let Self::Line(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Creates a scalar value.
    #[must_use]
    pub fn scalar(value: f64) -> Self {
        Self::Scalar(value)
    }

    /// Creates a point value.
    #[must_use]
    pub fn point(position: Complex) -> Self {
        Self::Point(position)
    }

    /// Creates a line value.
    #[must_use]
    pub fn line(origin: Complex, direction: Complex) -> Self {
        Self::Line(Line { origin, direction })
    }

    #[must_use]
    pub fn as_scalar(&self) -> Option<&f64> {
        if let Self::Scalar(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the value is [`Point`].
    ///
    /// [`Point`]: Value::Point
    #[must_use]
    pub fn is_point(&self) -> bool {
        matches!(self, Self::Point(..))
    }

    /// Returns a circle, if the value's a circle.
    #[must_use]
    pub fn as_circle(&self) -> Option<&Circle> {
        if let Self::Circle(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Scalar(value)
    }
}

impl From<Value> for f64 {
    fn from(value: Value) -> Self {
        value.as_scalar().copied().unwrap()
    }
}

impl From<Complex> for Value {
    fn from(value: Complex) -> Self {
        Self::Point(value)
    }
}

impl From<Value> for Complex {
    fn from(value: Value) -> Self {
        value.as_point().copied().unwrap()
    }
}

impl From<Line> for Value {
    fn from(value: Line) -> Self {
        Self::Line(value)
    }
}

impl From<Value> for Line {
    fn from(value: Value) -> Self {
        value.as_line().copied().unwrap()
    }
}

impl From<Circle> for Value {
    fn from(value: Circle) -> Self {
        Self::Circle(value)
    }
}

impl From<Value> for Circle {
    fn from(value: Value) -> Self {
        value.as_circle().copied().unwrap()
    }
}

/// Marks everything that can be evaluated.
pub trait Evaluate {
    type Output;

    /// Evaluates the thing.
    ///
    /// # Errors
    /// Any errors related to evaluation.
    fn evaluate(&self, args: &EvaluationArgs) -> Self::Output;

    /// Evaluates weights.
    fn evaluate_weights(&self) -> Weights;
}

/// All possible expressions.
pub mod expr {
    use std::{
        f64::consts::PI,
        ops::{AddAssign, Div},
        sync::Arc,
    };

    use serde::Serialize;

    use crate::{
        generator::{critic::EvaluationArgs, geometry, Complex},
        script::ComplexUnit,
    };

    use super::{
        Circle, CircleExpr, Evaluate, Expression, Kind, Line, LineExpr, PointExpr, ScalarExpr,
        Value, Weights, Zero,
    };

    #[derive(Debug, Clone, Serialize)]
    pub struct PointPointDistance {
        pub a: Arc<Expression<PointExpr>>,
        pub b: Arc<Expression<PointExpr>>,
    }

    impl Evaluate for PointPointDistance {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            // Evaluate the two points
            let p1 = self.a.evaluate(args);
            let p2 = self.b.evaluate(args);

            // Pythagorean theorem
            (p1 - p2).mangitude()
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    pub struct PointLineDistance {
        pub point: Arc<Expression<PointExpr>>,
        pub line: Arc<Expression<LineExpr>>,
    }

    impl Evaluate for PointLineDistance {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            // Evaluate the two points
            let point = self.point.evaluate(args);
            let line = self.line.evaluate(args);

            // Make the point coordinates relative to the origin and rotate.
            let point_rot = (point - line.origin) / line.direction;

            // Now we can just get the imaginary part. We have to take the absolute value here.
            point_rot.imaginary.abs()
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone() + &self.line.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 3 points.
    pub struct AnglePoint {
        pub arm1: Arc<Expression<PointExpr>>,
        pub origin: Arc<Expression<PointExpr>>,
        pub arm2: Arc<Expression<PointExpr>>,
    }

    impl Evaluate for AnglePoint {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            // Evaluate the two points
            let arm1 = self.arm1.evaluate(args);
            let origin = self.origin.evaluate(args);
            let arm2 = self.arm2.evaluate(args);

            geometry::get_angle(arm1, origin, arm2)
        }

        fn evaluate_weights(&self) -> Weights {
            self.arm1.weights.clone() + &self.origin.weights + &self.arm2.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 3 points.
    pub struct AnglePointDir {
        pub arm1: Arc<Expression<PointExpr>>,
        pub origin: Arc<Expression<PointExpr>>,
        pub arm2: Arc<Expression<PointExpr>>,
    }

    impl Evaluate for AnglePointDir {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            // Evaluate the two points
            let arm1 = self.arm1.evaluate(args);
            let origin = self.origin.evaluate(args);
            let arm2 = self.arm2.evaluate(args);

            geometry::get_angle_directed(arm1, origin, arm2)
        }

        fn evaluate_weights(&self) -> Weights {
            self.arm1.weights.clone() + &self.origin.weights + &self.arm2.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 2 lines.
    pub struct AngleLine {
        pub k: Arc<Expression<LineExpr>>,
        pub l: Arc<Expression<LineExpr>>,
    }

    impl Evaluate for AngleLine {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            // Evaluate the two points
            let line1 = self.k.evaluate(args);
            let line2 = self.l.evaluate(args);

            // Divide direction vectors and get the arg.
            let div = line1.direction / line2.direction;

            div.arg()
        }

        fn evaluate_weights(&self) -> Weights {
            self.k.weights.clone() + &self.l.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 2 lines.
    pub struct Literal {
        pub value: f64,
    }

    impl Evaluate for Literal {
        type Output = f64;

        fn evaluate(&self, _args: &EvaluationArgs) -> f64 {
            self.value
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::empty()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A free adjustable point.
    pub struct FreePoint {
        pub index: usize,
    }

    impl Evaluate for FreePoint {
        type Output = Complex;

        fn evaluate(&self, args: &EvaluationArgs) -> Complex {
            args.adjustables[self.index].0.as_point().copied().unwrap()
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::one_at(self.index)
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A point on a circle.
    pub struct PointOnCircle {
        pub circle: Arc<Expression<CircleExpr>>,
        pub index: usize,
    }

    impl Evaluate for PointOnCircle {
        type Output = Complex;

        fn evaluate(&self, args: &EvaluationArgs) -> Complex {
            let circle = self.circle.evaluate(args);
            let theta = args.adjustables[self.index].0.as_clip1d().copied().unwrap() * 2.0 * PI;

            let point_rel = Complex::new(theta.cos(), theta.sin());
            circle.center + point_rel * circle.radius
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::one_at(self.index)
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A point on a aine.
    pub struct PointOnLine {
        pub line: Arc<Expression<LineExpr>>,
        pub index: usize,
    }

    impl Evaluate for PointOnLine {
        type Output = Complex;

        fn evaluate(&self, args: &EvaluationArgs) -> Complex {
            let line = self.line.evaluate(args);
            let mag = args.adjustables[self.index].0.as_clip1d().copied().unwrap();

            line.origin + line.direction * mag
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::one_at(self.index)
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A line defined with two points.
    pub struct LinePoint {
        pub a: Arc<Expression<PointExpr>>,
        pub b: Arc<Expression<PointExpr>>,
    }

    impl Evaluate for LinePoint {
        type Output = Line;

        fn evaluate(&self, args: &EvaluationArgs) -> Line {
            let origin = self.a.evaluate(args);

            Line::new(origin, (self.b.evaluate(args) - origin).normalize())
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Line-line intersection.
    pub struct LineLineIntersection {
        pub k: Arc<Expression<LineExpr>>,
        pub l: Arc<Expression<LineExpr>>,
    }

    impl Evaluate for LineLineIntersection {
        type Output = Complex;

        fn evaluate(&self, args: &EvaluationArgs) -> Complex {
            let k = self.k.evaluate(args);
            let l = self.l.evaluate(args);

            geometry::get_intersection(k, l)
        }

        fn evaluate_weights(&self) -> Weights {
            self.l.weights.clone() + &self.k.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Line-line intersection.
    pub struct CircleCenter {
        pub circle: Arc<Expression<CircleExpr>>,
    }

    impl Evaluate for CircleCenter {
        type Output = Complex;

        fn evaluate(&self, args: &EvaluationArgs) -> Complex {
            self.circle.evaluate(args).center
        }

        fn evaluate_weights(&self) -> Weights {
            self.circle.weights.clone()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Line-line intersection.
    pub struct CircleRadius {
        pub circle: Arc<Expression<CircleExpr>>,
    }

    impl Evaluate for CircleRadius {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            self.circle.evaluate(args).radius
        }

        fn evaluate_weights(&self) -> Weights {
            self.circle.weights.clone()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Changes the scalar's unit.
    pub struct SetUnit {
        pub value: Arc<Expression<ScalarExpr>>,
        pub unit: ComplexUnit,
    }

    impl Evaluate for SetUnit {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            self.value.evaluate(args)
        }

        fn evaluate_weights(&self) -> Weights {
            self.value.weights.clone()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a + b.
    pub struct Sum {
        pub a: Arc<Expression<ScalarExpr>>,
        pub b: Arc<Expression<ScalarExpr>>,
    }

    impl Evaluate for Sum {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            let a = self.a.evaluate(args);
            let b = self.b.evaluate(args);

            a + b
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a - b.
    pub struct Difference {
        pub a: Arc<Expression<ScalarExpr>>,
        pub b: Arc<Expression<ScalarExpr>>,
    }

    impl Evaluate for Difference {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            let a = self.a.evaluate(args);
            let b = self.b.evaluate(args);

            a - b
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a * b.
    pub struct Product {
        pub a: Arc<Expression<ScalarExpr>>,
        pub b: Arc<Expression<ScalarExpr>>,
    }

    impl Evaluate for Product {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            let a = self.a.evaluate(args);
            let b = self.b.evaluate(args);

            a * b
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a / b.
    pub struct Quotient {
        pub a: Arc<Expression<ScalarExpr>>,
        pub b: Arc<Expression<ScalarExpr>>,
    }

    impl Evaluate for Quotient {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            let a = self.a.evaluate(args);
            let b = self.b.evaluate(args);

            a / b
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// -v.
    pub struct Negation {
        pub value: Arc<Expression<ScalarExpr>>,
    }

    impl Evaluate for Negation {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            let v = self.value.evaluate(args);

            -v
        }

        fn evaluate_weights(&self) -> Weights {
            self.value.weights.clone()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 3 points.
    pub struct AngleBisector {
        pub arm1: Arc<Expression<PointExpr>>,
        pub origin: Arc<Expression<PointExpr>>,
        pub arm2: Arc<Expression<PointExpr>>,
    }

    impl Evaluate for AngleBisector {
        type Output = Line;

        fn evaluate(&self, args: &EvaluationArgs) -> Line {
            // Evaluate the two points
            let arm1 = self.arm1.evaluate(args);
            let origin = self.origin.evaluate(args);
            let arm2 = self.arm2.evaluate(args);

            // Make the system relative to origin.
            let a = arm1 - origin;
            let b = arm2 - origin;

            // Get the bisector using the geometric mean.
            let bi_dir = (a * b).sqrt_norm();

            Line::new(origin, bi_dir)
        }

        fn evaluate_weights(&self) -> Weights {
            self.arm1.weights.clone() + &self.origin.weights + &self.arm2.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Gets the average value.
    pub struct Average<T> {
        pub items: Vec<Arc<Expression<T>>>,
    }

    impl<T: Evaluate + Kind> Evaluate for Average<T>
    where
        T::Output: From<Value>
            + Into<Value>
            + Clone
            + Zero
            + AddAssign<T::Output>
            + Div<f64, Output = T::Output>,
    {
        type Output = T::Output;

        fn evaluate(&self, args: &EvaluationArgs) -> Self::Output {
            let mut sum = T::Output::zero();

            for item in &self.items {
                sum += item.evaluate(args);
            }

            #[allow(clippy::cast_precision_loss)]
            let x = sum / self.items.len() as f64;

            x
        }

        fn evaluate_weights(&self) -> Weights {
            let mut ws = Weights::empty();

            for w in &self.items {
                ws += &w.weights;
            }

            ws
        }
    }

    #[derive(Debug, Clone, Serialize)]
    pub struct PerpendicularThrough {
        pub point: Arc<Expression<PointExpr>>,
        pub line: Arc<Expression<LineExpr>>,
    }

    impl Evaluate for PerpendicularThrough {
        type Output = Line;

        fn evaluate(&self, args: &EvaluationArgs) -> Line {
            // Evaluate the two points
            let point = self.point.evaluate(args);

            Line::new(point, self.line.evaluate(args).direction.mul_i())
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone() + &self.line.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    pub struct ParallelThrough {
        pub point: Arc<Expression<PointExpr>>,
        pub line: Arc<Expression<LineExpr>>,
    }

    impl Evaluate for ParallelThrough {
        type Output = Line;

        fn evaluate(&self, args: &EvaluationArgs) -> Line {
            // Evaluate the two points
            let point = self.point.evaluate(args);
            let line = self.line.evaluate(args);

            Line::new(point, line.direction)
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone() + &self.line.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A free adjustable real.
    pub struct Real {
        pub index: usize,
    }

    impl Evaluate for Real {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            args.adjustables[self.index].0.as_real().copied().unwrap()
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::one_at(self.index)
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// X coordinate of a point.
    pub struct PointX {
        pub point: Arc<Expression<PointExpr>>,
    }

    impl Evaluate for PointX {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            self.point.evaluate(args).real
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Y coordinate of a point.
    pub struct PointY {
        pub point: Arc<Expression<PointExpr>>,
    }

    impl Evaluate for PointY {
        type Output = f64;

        fn evaluate(&self, args: &EvaluationArgs) -> f64 {
            self.point.evaluate(args).imaginary
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone()
        }
    }

    /// The center and the radius of the circle.
    #[derive(Debug, Clone, Serialize)]
    pub struct CenterRadius {
        pub center: Arc<Expression<PointExpr>>,
        pub radius: Arc<Expression<ScalarExpr>>,
    }

    impl Evaluate for CenterRadius {
        type Output = Circle;

        fn evaluate(&self, args: &EvaluationArgs) -> Self::Output {
            Circle {
                center: self.center.evaluate(args),
                radius: self.radius.evaluate(args),
            }
        }

        fn evaluate_weights(&self) -> Weights {
            self.center.weights.clone() + &self.radius.weights
        }
    }
}

/// Defines a point expression.
#[derive(Debug, Clone, Serialize)]
pub enum PointExpr {
    /// An adjustable indexed point in euclidean space
    Free(FreePoint),
    /// A point on a circle.
    OnCircle(PointOnCircle),
    /// A point on a line.
    OnLine(PointOnLine),
    /// Takes the average value (arithmetic mean)
    Average(Average<PointExpr>),
    /// The point where two lines cross.
    LineLineIntersection(LineLineIntersection),
    /// The centre of a circle.
    CircleCenter(CircleCenter),
}

/// Defines a circle expression.
#[derive(Debug, Clone, Serialize)]
pub enum CircleExpr {
    /// A circle given the center and the radius.
    CenterRadius(CenterRadius),
}

impl Evaluate for CircleExpr {
    type Output = Circle;

    fn evaluate(&self, args: &EvaluationArgs) -> Self::Output {
        match self {
            Self::CenterRadius(v) => v.evaluate(args),
        }
    }

    fn evaluate_weights(&self) -> Weights {
        match self {
            Self::CenterRadius(v) => v.evaluate_weights(),
        }
    }
}

impl Kind for CircleExpr {
    fn collect(&self, exprs: &mut Vec<usize>) {
        match self {
            Self::CenterRadius(CenterRadius { center, radius }) => {
                center.collect(exprs);
                radius.collect(exprs);
            }
        }
    }

    fn is_trivial(&self) -> bool {
        false
    }
}

impl Evaluate for PointExpr {
    type Output = Complex;

    fn evaluate(&self, args: &EvaluationArgs) -> Complex {
        match self {
            Self::Free(v) => v.evaluate(args),
            Self::OnCircle(v) => v.evaluate(args),
            Self::OnLine(v) => v.evaluate(args),
            Self::LineLineIntersection(v) => v.evaluate(args),
            Self::Average(v) => v.evaluate(args),
            Self::CircleCenter(v) => v.evaluate(args),
        }
    }

    fn evaluate_weights(&self) -> Weights {
        match self {
            Self::Free(v) => v.evaluate_weights(),
            Self::OnCircle(v) => v.evaluate_weights(),
            Self::OnLine(v) => v.evaluate_weights(),
            Self::LineLineIntersection(v) => v.evaluate_weights(),
            Self::Average(v) => v.evaluate_weights(),
            Self::CircleCenter(v) => v.evaluate_weights(),
        }
    }
}

impl Kind for PointExpr {
    fn collect(&self, exprs: &mut Vec<usize>) {
        match self {
            Self::LineLineIntersection(LineLineIntersection { k, l }) => {
                k.collect(exprs);
                l.collect(exprs);
            }
            Self::Average(v) => {
                for x in &v.items {
                    x.collect(exprs);
                }
            }
            Self::Free(_) => (),
            Self::OnCircle(v) => v.circle.collect(exprs),
            Self::OnLine(v) => v.line.collect(exprs),
            Self::CircleCenter(v) => v.circle.collect(exprs),
        }
    }

    fn is_trivial(&self) -> bool {
        matches!(self, Self::Free(_) | Self::CircleCenter(_))
    }
}

/// Defines a line expression.
#[derive(Debug, Clone, Serialize)]
pub enum LineExpr {
    /// A line in euclidean space. defined by two points.
    Line(LinePoint),
    /// An angle bisector.
    AngleBisector(AngleBisector),
    /// Generates a line perpendicular to $1 going through $2
    PerpendicularThrough(PerpendicularThrough),
    /// Generates a line parallel to $1 going through $2
    ParallelThrough(ParallelThrough),
}

impl Evaluate for LineExpr {
    type Output = Line;

    fn evaluate(&self, args: &EvaluationArgs) -> Line {
        match self {
            Self::Line(v) => v.evaluate(args),
            Self::AngleBisector(v) => v.evaluate(args),
            Self::PerpendicularThrough(v) => v.evaluate(args),
            Self::ParallelThrough(v) => v.evaluate(args),
        }
    }

    fn evaluate_weights(&self) -> Weights {
        match self {
            Self::Line(v) => v.evaluate_weights(),
            Self::AngleBisector(v) => v.evaluate_weights(),
            Self::PerpendicularThrough(v) => v.evaluate_weights(),
            Self::ParallelThrough(v) => v.evaluate_weights(),
        }
    }
}

impl Kind for LineExpr {
    fn collect(&self, exprs: &mut Vec<usize>) {
        match self {
            Self::Line(LinePoint { a, b }) => {
                a.collect(exprs);
                b.collect(exprs);
            }
            Self::AngleBisector(AngleBisector { arm1, origin, arm2 }) => {
                arm1.collect(exprs);
                origin.collect(exprs);
                arm2.collect(exprs);
            }
            Self::PerpendicularThrough(PerpendicularThrough { point, line })
            | Self::ParallelThrough(ParallelThrough { point, line }) => {
                point.collect(exprs);
                line.collect(exprs);
            }
        }
    }

    fn is_trivial(&self) -> bool {
        false
    }
}

/// Defines a scalar expression.
#[derive(Debug, Clone, Serialize)]
pub enum ScalarExpr {
    /// Euclidean distance between two points.
    PointPointDistance(PointPointDistance),
    /// Euclidean distance between a point and its rectangular projection onto a line.
    PointLineDistance(PointLineDistance),
    /// An angle defined with 3 points.
    AnglePoint(AnglePoint),
    /// A directed angle defined with 3 points.
    AnglePointDir(AnglePointDir),
    /// An angle defined with 2 lines.
    AngleLine(AngleLine),
    /// A real literal.
    Literal(Literal),
    /// Changes the unit
    SetUnit(SetUnit),
    /// Adds two values
    Sum(Sum),
    /// Subtracts two values
    Difference(Difference),
    /// Multiplies two values
    Product(Product),
    /// Divides two values
    Quotient(Quotient),
    /// Changes the sign
    Negation(Negation),
    /// An adjusted real value
    Real(Real),
    /// X coordinate of a point,
    PointX(PointX),
    /// Y coordinate of a point.
    PointY(PointY),
    /// Average
    Average(Average<Self>),
    /// Radius of a circle
    CircleRadius(CircleRadius),
}

impl Kind for ScalarExpr {
    fn collect(&self, exprs: &mut Vec<usize>) {
        match self {
            Self::PointPointDistance(PointPointDistance { a, b }) => {
                a.collect(exprs);
                b.collect(exprs);
            }
            Self::Sum(Sum { a, b })
            | Self::Difference(Difference { a, b })
            | Self::Product(Product { a, b })
            | Self::Quotient(Quotient { a, b }) => {
                a.collect(exprs);
                b.collect(exprs);
            }
            Self::AnglePoint(AnglePoint { arm1, origin, arm2 })
            | Self::AnglePointDir(AnglePointDir { arm1, origin, arm2 }) => {
                arm1.collect(exprs);
                origin.collect(exprs);
                arm2.collect(exprs);
            }
            Self::AngleLine(AngleLine { k, l }) => {
                k.collect(exprs);
                l.collect(exprs);
            }
            Self::Negation(Negation { value }) | Self::SetUnit(SetUnit { value, unit: _ }) => {
                value.collect(exprs);
            }
            Self::PointY(PointY { point }) | Self::PointX(PointX { point }) => {
                point.collect(exprs);
            }
            Self::Average(v) => {
                for x in &v.items {
                    x.collect(exprs);
                }
            }
            Self::PointLineDistance(PointLineDistance { point, line }) => {
                point.collect(exprs);
                line.collect(exprs);
            }
            Self::Real(_) | Self::Literal(_) => (),
            Self::CircleRadius(CircleRadius { circle }) => {
                circle.collect(exprs);
            }
        }
    }

    fn is_trivial(&self) -> bool {
        match self {
            Self::PointPointDistance(_)
            | Self::PointLineDistance(_)
            | Self::AnglePoint(_)
            | Self::AnglePointDir(_)
            | Self::AngleLine(_)
            | Self::Sum(_)
            | Self::Difference(_)
            | Self::Product(_)
            | Self::Quotient(_)
            | Self::Negation(_)
            | Self::Average(_) => false,
            Self::Literal(_)
            | Self::SetUnit(_)
            | Self::Real(_)
            | Self::CircleRadius(_)
            | Self::PointX(_)
            | Self::PointY(_) => true,
        }
    }
}

impl Evaluate for ScalarExpr {
    type Output = f64;

    fn evaluate(&self, args: &EvaluationArgs) -> f64 {
        match self {
            Self::PointPointDistance(v) => v.evaluate(args),
            Self::PointLineDistance(v) => v.evaluate(args),
            Self::AnglePoint(v) => v.evaluate(args),
            Self::AnglePointDir(v) => v.evaluate(args),
            Self::AngleLine(v) => v.evaluate(args),
            Self::Literal(v) => v.evaluate(args),
            Self::SetUnit(v) => v.evaluate(args),
            Self::Sum(v) => v.evaluate(args),
            Self::Difference(v) => v.evaluate(args),
            Self::Product(v) => v.evaluate(args),
            Self::Quotient(v) => v.evaluate(args),
            Self::Negation(v) => v.evaluate(args),
            Self::Real(v) => v.evaluate(args),
            Self::PointX(v) => v.evaluate(args),
            Self::PointY(v) => v.evaluate(args),
            Self::Average(v) => v.evaluate(args),
            Self::CircleRadius(v) => v.evaluate(args),
        }
    }

    fn evaluate_weights(&self) -> Weights {
        match self {
            Self::PointPointDistance(v) => v.evaluate_weights(),
            Self::PointLineDistance(v) => v.evaluate_weights(),
            Self::AnglePoint(v) => v.evaluate_weights(),
            Self::AnglePointDir(v) => v.evaluate_weights(),
            Self::AngleLine(v) => v.evaluate_weights(),
            Self::Literal(v) => v.evaluate_weights(),
            Self::SetUnit(v) => v.evaluate_weights(),
            Self::Sum(v) => v.evaluate_weights(),
            Self::Difference(v) => v.evaluate_weights(),
            Self::Product(v) => v.evaluate_weights(),
            Self::Quotient(v) => v.evaluate_weights(),
            Self::Negation(v) => v.evaluate_weights(),
            Self::Real(v) => v.evaluate_weights(),
            Self::PointX(v) => v.evaluate_weights(),
            Self::PointY(v) => v.evaluate_weights(),
            Self::Average(v) => v.evaluate_weights(),
            Self::CircleRadius(v) => v.evaluate_weights(),
        }
    }
}

/// Defines a point expression.
#[derive(Debug, Clone, Serialize)]
pub enum AnyExpr {
    /// A scalar.
    Scalar(ScalarExpr),
    /// A point.
    Point(PointExpr),
    /// A line
    Line(LineExpr),
}

impl Kind for AnyExpr {
    fn collect(&self, exprs: &mut Vec<usize>) {
        match self {
            Self::Line(line) => line.collect(exprs),
            Self::Point(point) => point.collect(exprs),
            Self::Scalar(scalar) => scalar.collect(exprs),
        }
    }

    fn is_trivial(&self) -> bool {
        match self {
            Self::Line(line) => line.is_trivial(),
            Self::Point(point) => point.is_trivial(),
            Self::Scalar(scalar) => scalar.is_trivial(),
        }
    }
}

impl Evaluate for AnyExpr {
    type Output = Value;

    fn evaluate(&self, args: &EvaluationArgs) -> Self::Output {
        match self {
            Self::Line(line) => line.evaluate(args).into(),
            Self::Point(point) => point.evaluate(args).into(),
            Self::Scalar(scalar) => scalar.evaluate(args).into(),
        }
    }

    fn evaluate_weights(&self) -> Weights {
        match self {
            Self::Line(line) => line.evaluate_weights(),
            Self::Point(point) => point.evaluate_weights(),
            Self::Scalar(scalar) => scalar.evaluate_weights(),
        }
    }
}
