use std::{ops::{Add, Mul, AddAssign}, sync::Arc, cell::RefCell};

use serde::Serialize;

use crate::script::{ComplexUnit, HashableWeakArc};

use self::expr::{PointPointDistance, PointLineDistance, AnglePoint, AngleLine, Literal, FreePoint, LinePoint, LineLineIntersection, SetUnit, Sum, Difference, Product, Quotient, Negation, AngleBisector, Average, PerpendicularThrough, ParallelThrough, Real, PointX, PointY};

use super::{Complex, critic::EvaluationArgs, EvaluationError};

#[derive(Debug, Clone)]
pub struct ExprCache {
    pub value: Value,
    pub generation: u64,
}

/// A utility for `Weights`.
#[derive(Debug, Clone, Serialize)]
pub struct Weights(pub Vec<f64>);

impl Weights {
    #[must_use]
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    #[must_use]
    pub fn one_at(index: usize) -> Self {
        let mut v = Vec::new();
        v.resize(index + 1, 0.0);

        v[index] = 1.0;

        Self(v)
    }

    pub fn set(&mut self, index: usize, weight: f64) {
        self.0.resize(index + 1, 0.0);
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
                *v += w;
            } else {
                self.set(i, *w);
            }
        }
    }
}

impl Mul<f64> for Weights {
    type Output = Weights;

    fn mul(self, rhs: f64) -> Self::Output {
        Weights(self.0.into_iter().map(|v|  v * rhs).collect())
    }
}

/// An expression is a base construct of Geo-AID. Contains a cache, saved weights and the expression kind itself.
#[derive(Debug, Clone, Serialize)]
pub struct Expression {
    /// Saved weights
    pub weights: Weights,
    /// Expression kind.
    pub kind: ExprKind
}

impl Expression {
    #[must_use]
    pub fn new(expr: ExprKind, weight: f64) -> Self {
        Self {
            weights: expr.evaluate_weights() * weight,
            kind: expr
        }
    }

    /// Evaluates the expression.
    /// 
    /// # Errors
    /// Any errors related to failure when evaluating.
    pub fn evaluate(self: &Arc<Self>, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
        let mut mutptr: Option<*mut ExprCache> = None;

        if args.flags.optimizations.identical_expressions && Arc::strong_count(self) > 1 {
            if let Some(mut cache) = args.cache.map(RefCell::borrow_mut) {
                if let Some(expr_cache) = cache.get_mut(&HashableWeakArc::new(Arc::downgrade(self))) {
                    if expr_cache.generation == args.generation {
                        return Ok(expr_cache.value.clone());
                    }

                    mutptr = Some(expr_cache); 
                }
            }
        }

        let v = self.kind.evaluate(args)?;

        Ok(match mutptr {
            Some(cache) => {
                unsafe {
                    let cache = &mut *cache;
                    cache.generation = args.generation;
                    cache.value = v;

                    cache.value.clone()
                }
            }
            None => v,
        })
    }

    pub fn collect<'r>(self: &'r Arc<Self>, exprs: &mut Vec<&'r Arc<Expression>>) {
        exprs.push(self);

        match &self.kind {
            ExprKind::PointPointDistance(PointPointDistance { a, b })
            | ExprKind::Line(LinePoint { a, b })
            | ExprKind::Sum(Sum { a, b })
            | ExprKind::Difference(Difference { a, b })
            | ExprKind::Product(Product { a, b })
            | ExprKind::Quotient(Quotient { a, b }) => {
                a.collect(exprs);
                b.collect(exprs);
            }
            ExprKind::AnglePoint(AnglePoint { arm1, origin, arm2 })
            | ExprKind::AngleBisector(AngleBisector { arm1, origin, arm2 }) => {
                arm1.collect(exprs);
                origin.collect(exprs);
                arm2.collect(exprs);
            }
            ExprKind::AngleLine(AngleLine { k, l })
            | ExprKind::LineLineIntersection(LineLineIntersection { k, l }) => {
                k.collect(exprs);
                l.collect(exprs);
            }
            ExprKind::Negation(Negation { value })
            | ExprKind::SetUnit(SetUnit { value, unit: _ })
            | ExprKind::PointY(PointY { point: value })
            | ExprKind::PointX(PointX { point: value }) => {
                value.collect(exprs);
            }
            ExprKind::Average(v) => {
                for x in &v.items {
                    x.collect(exprs);
                }
            }
            ExprKind::PerpendicularThrough(PerpendicularThrough { point, line })
            | ExprKind::ParallelThrough(ParallelThrough { point, line })
            | ExprKind::PointLineDistance(PointLineDistance { point, line }) => {
                point.collect(exprs);
                line.collect(exprs);
            }
            ExprKind::Real(_)
            | ExprKind::FreePoint(_)
            | ExprKind::Literal(_) => (),
        }
    }

    /// Trivial expressions are ones that don't require any calculations being made.
    /// Trivial expressions should not be cached, as it is much faster to evaluate them
    /// than to get their caches.
    #[must_use]
    pub fn is_trivial(&self) -> bool {
        match &self.kind {
            ExprKind::PointPointDistance(_) | ExprKind::PointLineDistance(_) | ExprKind::AnglePoint(_)
            | ExprKind::AngleLine(_) | ExprKind::Line(_) | ExprKind::LineLineIntersection(_)
            | ExprKind::Sum(_) | ExprKind::Difference(_) | ExprKind::Product(_)
            | ExprKind::Quotient(_) | ExprKind::Negation(_) | ExprKind::AngleBisector(_)
            | ExprKind::Average(_) | ExprKind::PerpendicularThrough(_) | ExprKind::ParallelThrough(_) => false,
            ExprKind::Literal(_) | ExprKind::FreePoint(_) | ExprKind::SetUnit(_) | ExprKind::Real(_)
            | ExprKind::PointX(_) | ExprKind::PointY(_) => true,
            
        }
    }
}

/// Represents a point in a 2D euclidean space.
#[derive(Debug, Clone, Copy)]
pub struct Point {
    /// Point's position as a complex number.
    pub position: Complex
}

/// Represents a line in a 2D euclidean space.
#[derive(Debug, Clone, Copy)]
pub struct Line {
    /// Line's origin as a complex number.
    pub origin: Complex,
    /// A normalized direction vector.
    pub direction: Complex
}

/// Represents a scalar with a unit.
#[derive(Debug, Clone)]
pub struct Scalar {
    /// The value.
    pub value: f64,
    /// The unit.
    pub unit: ComplexUnit
}

/// An evaluated value.
#[derive(Debug, Clone)]
pub enum Value {
    Point(Point),
    Line(Line),
    Scalar(Scalar)
}

impl Value {
    #[must_use]
    pub fn as_point(&self) -> Option<&Point> {
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
    pub fn scalar(value: f64, unit: ComplexUnit) -> Self {
        Self::Scalar(Scalar {
            value,
            unit
        })
    }

    /// Creates a point value.
    #[must_use]
    pub fn point(position: Complex) -> Self {
        Self::Point(Point {
            position
        })
    }

    /// Creates a line value.
    #[must_use]
    pub fn line(origin: Complex, direction: Complex) -> Self {
        Self::Line(Line {
            origin,
            direction
        })
    }

    #[must_use]
    pub fn as_scalar(&self) -> Option<&Scalar> {
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
}

/// Marks everything that can be evaluated.
pub trait Evaluate {
    /// Evaluates the thing.
    /// 
    /// # Errors
    /// Any errors related to evaluation.
    fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError>;

    /// Evaluates weights.
    fn evaluate_weights(&self) -> Weights;
}

/// All possible expressions.
pub mod expr {
    use std::sync::Arc;

    use serde::Serialize;

    use crate::{generator::{critic::EvaluationArgs, EvaluationError, geometry, Complex}, script::{unit, ComplexUnit}};

    use super::{Expression, Evaluate, Value, Weights};

    #[derive(Debug, Clone, Serialize)]
    pub struct PointPointDistance {
        pub a: Arc<Expression>,
        pub b: Arc<Expression>
    }

    impl Evaluate for PointPointDistance {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let p1 = self.a.evaluate(args)?.as_point().unwrap().position;
            let p2 = self.b.evaluate(args)?.as_point().unwrap().position;

            // Pythagorean theorem
            let distance = (p1 - p2).mangitude();
            Ok(Value::scalar(distance, unit::DISTANCE))
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    pub struct PointLineDistance {
        pub point: Arc<Expression>,
        pub line: Arc<Expression>
    }

    impl Evaluate for PointLineDistance {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let point = self.point.evaluate(args)?.as_point().unwrap().position;
            let v = self.line.evaluate(args)?;
            let line = v.as_line().unwrap();

            // Make the point coordinates relative to the origin and rotate.
            let point_rot = (point - line.origin) / line.direction;

            // Now we can just get the imaginary part. We have to take the absolute value here.
            let distance = point_rot.imaginary.abs();

            Ok(Value::scalar(distance, unit::DISTANCE))
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone() + &self.line.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 3 points.
    pub struct AnglePoint {
        pub arm1: Arc<Expression>,
        pub origin: Arc<Expression>,
        pub arm2: Arc<Expression>,
    }

    impl Evaluate for AnglePoint {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let arm1 = self.arm1.evaluate(args)?.as_point().unwrap().position;
            let origin = self.origin.evaluate(args)?.as_point().unwrap().position;
            let arm2 = self.arm2.evaluate(args)?.as_point().unwrap().position;

            Ok(Value::scalar(
                geometry::get_angle(arm1, origin, arm2),
                unit::ANGLE
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.arm1.weights.clone() + &self.origin.weights + &self.arm2.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 2 lines.
    pub struct AngleLine {
        pub k: Arc<Expression>,
        pub l: Arc<Expression>
    }

    impl Evaluate for AngleLine {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let line1_v = self.k.evaluate(args)?;
            let line1 = line1_v.as_line().unwrap();
            let line2_v = self.l.evaluate(args)?;
            let line2 = line2_v.as_line().unwrap();

            // Divide direction vectors and get the arg.
            let div = line1.direction / line2.direction;

            Ok(Value::scalar(
                div.arg(),
                unit::ANGLE
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.k.weights.clone() + &self.l.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 2 lines.
    pub struct Literal {
        pub value: f64,
        pub unit: ComplexUnit
    }

    impl Evaluate for Literal {
        fn evaluate(&self, _args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            Ok(Value::scalar(
                self.value,
                self.unit.clone()
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::empty()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A free adjustable point.
    pub struct FreePoint {
        pub index: usize
    }

    impl Evaluate for FreePoint {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            Ok(Value::point(
                args.adjustables[self.index].0.as_point().copied().unwrap()
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::one_at(self.index)
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A line defined with two points.
    pub struct LinePoint {
        pub a: Arc<Expression>,
        pub b: Arc<Expression>
    }

    impl Evaluate for LinePoint {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let origin = self.a.evaluate(args)?.as_point().unwrap().position;

            Ok(Value::line(
                origin,
                (self.b.evaluate(args)?.as_point().unwrap().position - origin).normalize()
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Line-line intersection.
    pub struct LineLineIntersection {
        pub k: Arc<Expression>,
        pub l: Arc<Expression>
    }

    impl Evaluate for LineLineIntersection {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let k_v = self.k.evaluate(args)?;
            let k = k_v.as_line().unwrap();
            let l_v = self.l.evaluate(args)?;
            let l = l_v.as_line().unwrap();

            Ok(Value::point(
                geometry::get_intersection(*k, *l)?
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.l.weights.clone() + &self.k.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Changes the scalar's unit.
    pub struct SetUnit {
        pub value: Arc<Expression>,
        pub unit: ComplexUnit
    }

    impl Evaluate for SetUnit {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let l = self.value.evaluate(args)?.as_scalar().unwrap().value;

            Ok(Value::scalar(
                l,
                self.unit.clone()
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.value.weights.clone()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a + b.
    pub struct Sum {
        pub a: Arc<Expression>,
        pub b: Arc<Expression>
    }

    impl Evaluate for Sum {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let a_v = self.a.evaluate(args)?;
            let a = a_v.as_scalar().unwrap();
            let b_v = self.b.evaluate(args)?;
            let b = b_v.as_scalar().unwrap();

            Ok(Value::scalar(
                a.value + b.value,
                a.unit.clone()
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a - b.
    pub struct Difference {
        pub a: Arc<Expression>,
        pub b: Arc<Expression>
    }

    impl Evaluate for Difference {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let a_v = self.a.evaluate(args)?;
            let a = a_v.as_scalar().unwrap();
            let b_v = self.b.evaluate(args)?;
            let b = b_v.as_scalar().unwrap();

            Ok(Value::scalar(
                a.value - b.value,
                a.unit.clone()
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a * b.
    pub struct Product {
        pub a: Arc<Expression>,
        pub b: Arc<Expression>
    }

    impl Evaluate for Product {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let a_v = self.a.evaluate(args)?;
            let a = a_v.as_scalar().unwrap();
            let b_v = self.b.evaluate(args)?;
            let b = b_v.as_scalar().unwrap();

            Ok(Value::scalar(
                a.value * b.value,
                a.unit.clone() * &b.unit
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// a / b.
    pub struct Quotient {
        pub a: Arc<Expression>,
        pub b: Arc<Expression>
    }

    impl Evaluate for Quotient {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let a_v = self.a.evaluate(args)?;
            let a = a_v.as_scalar().unwrap();
            let b_v = self.b.evaluate(args)?;
            let b = b_v.as_scalar().unwrap();

            Ok(Value::scalar(
                a.value / b.value,
                a.unit.clone() / &b.unit
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights.clone() + &self.b.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// -v.
    pub struct Negation {
        pub value: Arc<Expression>
    }

    impl Evaluate for Negation {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let value_v = self.value.evaluate(args)?;
            let v = value_v.as_scalar().unwrap();

            Ok(Value::scalar(
                -v.value,
                v.unit.clone()
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.value.weights.clone()
        }
    }
    
    #[derive(Debug, Clone, Serialize)]
    /// An angle defined with 3 points.
    pub struct AngleBisector {
        pub arm1: Arc<Expression>,
        pub origin: Arc<Expression>,
        pub arm2: Arc<Expression>,
    }

    impl Evaluate for AngleBisector {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let arm1 = self.arm1.evaluate(args)?.as_point().unwrap().position;
            let origin = self.origin.evaluate(args)?.as_point().unwrap().position;
            let arm2 = self.arm2.evaluate(args)?.as_point().unwrap().position;

            // Make the system relative to origin.
            let a = arm1 - origin;
            let b = arm2 - origin;

            // Get the bisector using the geometric mean.
            let bi_dir = (a * b).sqrt_norm();

            Ok(Value::line(
                origin,
                bi_dir
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.arm1.weights.clone() + &self.origin.weights + &self.arm2.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Gets the average value.
    pub struct Average {
        pub items: Vec<Arc<Expression>>
    }

    impl Evaluate for Average {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            let mut unit = None;
            let mut sum_f = 0.0;
            let mut sum_c = Complex::default();

            if let Some(first) = self.items.first() {
                let ev = first.evaluate(args)?;

                if let Some(pt) = ev.as_point() {
                    sum_c += pt.position;
                } else {
                    let sc = ev.as_scalar().unwrap();
                    sum_f = sc.value;
                    unit = Some(sc.unit.clone());
                }
            }
            
            if let Some(unit) = unit {
                for v in self.items.iter().skip(1) {
                    sum_f += v.evaluate(args)?.as_scalar().unwrap().value;
                }

                #[allow(clippy::cast_precision_loss)]
                Ok(Value::scalar(sum_f / self.items.len() as f64, unit))
            } else {
                for v in self.items.iter().skip(1) {
                    sum_c += v.evaluate(args)?.as_point().unwrap().position;
                }

                #[allow(clippy::cast_precision_loss)]
                Ok(Value::point(sum_c / self.items.len() as f64))
            }
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
        pub point: Arc<Expression>,
        pub line: Arc<Expression>
    }

    impl Evaluate for PerpendicularThrough {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let point = self.point.evaluate(args)?.as_point().unwrap().position;

            Ok(Value::line(point, self.line.evaluate(args)?.as_line().unwrap().direction.mul_i()))
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone() + &self.line.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    pub struct ParallelThrough {
        pub point: Arc<Expression>,
        pub line: Arc<Expression>
    }

    impl Evaluate for ParallelThrough {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let point = self.point.evaluate(args)?.as_point().unwrap().position;
            let line_v = self.line.evaluate(args)?;
            let line = line_v.as_line().unwrap();

            Ok(Value::line(point, line.direction))
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone() + &self.line.weights
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// A free adjustable real.
    pub struct Real {
        pub index: usize
    }

    impl Evaluate for Real {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            Ok(Value::scalar(
                args.adjustables[self.index].0.as_real().copied().unwrap(),
                unit::SCALAR
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::one_at(self.index)
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// X coordinate of a point.
    pub struct PointX {
        pub point: Arc<Expression>
    }

    impl Evaluate for PointX {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            Ok(Value::scalar(
                self.point.evaluate(args)?.as_point().unwrap().position.real,
                unit::SCALAR
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone()
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// Y coordinate of a point.
    pub struct PointY {
        pub point: Arc<Expression>
    }

    impl Evaluate for PointY {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            Ok(Value::scalar(
                self.point.evaluate(args)?.as_point().unwrap().position.imaginary,
                unit::SCALAR
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights.clone()
        }
    }
}

/// Defines an expression kind.
#[derive(Debug, Clone, Serialize)]
pub enum ExprKind {
    /// Euclidean distance between two points.
    PointPointDistance(PointPointDistance),
    /// Euclidean distance between a point and its rectangular projection onto a line.
    PointLineDistance(PointLineDistance),
    /// An angle defined with 3 points.
    AnglePoint(AnglePoint),
    /// An angle defined with 2 lines.
    AngleLine(AngleLine),
    /// A real literal.
    Literal(Literal),
    /// An adjustable indexed point in euclidean space
    FreePoint(FreePoint),
    /// A line in euclidean space. defined by two points.
    Line(LinePoint),
    /// The point where two lines cross.
    LineLineIntersection(LineLineIntersection),
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
    /// An angle bisector.
    AngleBisector(AngleBisector),
    /// Takes the average value (arithmetic mean)
    Average(Average),
    /// Generates a line perpendicular to $1 going through $2
    PerpendicularThrough(PerpendicularThrough),
    /// Generates a line parallel to $1 going through $2
    ParallelThrough(ParallelThrough),
    /// An adjusted real value
    Real(Real),
    /// X coordinate of a point,
    PointX(PointX),
    /// Y coordinate of a point.
    PointY(PointY)
}

impl Evaluate for ExprKind {
    fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
        match self {
            Self::PointPointDistance(v) => v.evaluate(args),
            Self::PointLineDistance(v) => v.evaluate(args),
            Self::AnglePoint(v) => v.evaluate(args),
            Self::AngleLine(v) => v.evaluate(args),
            Self::Literal(v) => v.evaluate(args),
            Self::FreePoint(v) => v.evaluate(args),
            Self::Line(v) => v.evaluate(args),
            Self::LineLineIntersection(v) => v.evaluate(args),
            Self::SetUnit(v) => v.evaluate(args),
            Self::Sum(v) => v.evaluate(args),
            Self::Difference(v) => v.evaluate(args),
            Self::Product(v) => v.evaluate(args),
            Self::Quotient(v) => v.evaluate(args),
            Self::Negation(v) => v.evaluate(args),
            Self::AngleBisector(v) => v.evaluate(args),
            Self::Average(v) => v.evaluate(args),
            Self::PerpendicularThrough(v) => v.evaluate(args),
            Self::ParallelThrough(v) => v.evaluate(args),
            Self::Real(v) => v.evaluate(args),
            Self::PointX(v) => v.evaluate(args),
            Self::PointY(v) => v.evaluate(args),
        }
    }

    fn evaluate_weights(&self) -> Weights {
        match self {
            Self::PointPointDistance(v) => v.evaluate_weights(),
            Self::PointLineDistance(v) => v.evaluate_weights(),
            Self::AnglePoint(v) => v.evaluate_weights(),
            Self::AngleLine(v) => v.evaluate_weights(),
            Self::Literal(v) => v.evaluate_weights(),
            Self::FreePoint(v) => v.evaluate_weights(),
            Self::Line(v) => v.evaluate_weights(),
            Self::LineLineIntersection(v) => v.evaluate_weights(),
            Self::SetUnit(v) => v.evaluate_weights(),
            Self::Sum(v) => v.evaluate_weights(),
            Self::Difference(v) => v.evaluate_weights(),
            Self::Product(v) => v.evaluate_weights(),
            Self::Quotient(v) => v.evaluate_weights(),
            Self::Negation(v) => v.evaluate_weights(),
            Self::AngleBisector(v) => v.evaluate_weights(),
            Self::Average(v) => v.evaluate_weights(),
            Self::PerpendicularThrough(v) => v.evaluate_weights(),
            Self::ParallelThrough(v) => v.evaluate_weights(),
            Self::Real(v) => v.evaluate_weights(),
            Self::PointX(v) => v.evaluate_weights(),
            Self::PointY(v) => v.evaluate_weights()
        }
    }
}