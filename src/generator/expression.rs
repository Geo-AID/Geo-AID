use std::{sync::Arc, ops::{Add, Mul, AddAssign}};

use crate::script::{ComplexUnit, unit};

use self::expr::{PointPointDistance, PointLineDistance, AnglePoint, AngleLine, Literal, FreePoint};

use super::{Complex, critic::EvaluationArgs, EvaluationError};

#[derive(Debug, Clone)]
pub struct ExprCache {
    pub value: Complex,
    pub unit: ComplexUnit,
    pub generation: u64,
}

/// A utility for `Weights`.
#[derive(Debug, Clone)]
pub struct Weights(Vec<f64>);

impl Weights {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

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
        for (i, w) in rhs.0.into_iter().enumerate() {
            if let Some(v) = self.0.get_mut(i) {
                *v += w
            } else {
                self.set(i, w);
            }
        }

        self
    }
}

impl AddAssign<&Weights> for Weights {
    fn add_assign(&mut self, rhs: &Self) {
        *self = *self + rhs;
    }
}

impl Mul<f64> for Weights {
    type Output = Weights;

    fn mul(self, rhs: f64) -> Self::Output {
        Weights(self.0.into_iter().map(|v|  v * rhs).collect())
    }
}

/// An expression is a base construct of Geo-AID. Contains a cache, saved weights and the expression kind itself.
#[derive(Debug, Clone)]
pub struct Expression {
    /// Saved weights
    pub weights: Weights,
    /// Cached value for the expression.
    pub cache: ExprCache,
    /// Expression kind.
    pub kind: ExprKind
}

impl Expression {
    pub fn new(expr: ExprKind, weight: f64) -> Self {
        Self {
            weights: expr.evaluate_weights() * weight,
            cache: ExprCache {
                value: Complex::zero(),
                unit: unit::SCALAR,
                generation: 0
            },
            kind: expr
        }
    }
}

/// Represents a point in a 2D euclidean space.
#[derive(Debug, Clone)]
pub struct Point {
    /// Point's position as a complex number.
    pub position: Complex
}

/// Represents a line in a 2D euclidean space.
#[derive(Debug, Clone)]
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
pub enum Value {
    Point(Point),
    Line(Line),
    Scalar(Scalar)
}

impl Value {
    pub fn as_point(&self) -> Option<&Point> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_line(&self) -> Option<&Line> {
        if let Self::Line(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Creates a scalar value.
    pub fn scalar(value: f64, unit: ComplexUnit) -> Self {
        Self::Scalar(Scalar {
            value,
            unit
        })
    }

    /// Creates a point value.
    pub fn point(position: Complex) -> Self {
        Self::Point(Point {
            position
        })
    }
}

/// Marks everything that can be evaluated.
pub trait Evaluate {
    /// Evaluates the thing.
    fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError>;

    /// Evaluates weights.
    fn evaluate_weights(&self) -> Weights;
}

impl Evaluate for Expression {
    fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
        self.kind.evaluate(args)
    }

    fn evaluate_weights(&self) -> Weights {
        self.weights.clone()
    }
}

/// All possible expressions.
pub mod expr {
    use std::sync::Arc;

    use crate::{generator::{critic::EvaluationArgs, EvaluationError, geometry}, script::{unit, ComplexUnit}};

    use super::{Expression, Evaluate, Value, Weights};

    #[derive(Debug, Clone)]
    pub struct PointPointDistance {
        pub a: Arc<Expression>,
        pub b: Arc<Expression>
    }

    impl Evaluate for PointPointDistance {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let p1 = self.a.evaluate(args)?.as_point().unwrap();
            let p2 = self.b.evaluate(args)?.as_point().unwrap();

            // Pythagorean theorem
            let distance = (p1.position - p2.position).mangitude();
            Ok(Value::scalar(distance, unit::DISTANCE))
        }

        fn evaluate_weights(&self) -> Weights {
            self.a.weights + &self.b.weights
        }
    }

    #[derive(Debug, Clone)]
    pub struct PointLineDistance {
        pub point: Arc<Expression>,
        pub line: Arc<Expression>
    }

    impl Evaluate for PointLineDistance {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let point = self.point.evaluate(args)?.as_point().unwrap();
            let line = self.line.evaluate(args)?.as_line().unwrap();

            // Rotate the point and line's origin so that the line can be trated as a horizontal one.
            let point_rot = point.position / line.direction;
            let origin_rot = line.origin / line.direction;

            // Now we can just get the difference in the imaginary parts.
            let distance = point_rot.imaginary - origin_rot.imaginary;

            Ok(Value::scalar(distance, unit::DISTANCE))
        }

        fn evaluate_weights(&self) -> Weights {
            self.point.weights + &self.line.weights
        }
    }

    #[derive(Debug, Clone)]
    /// An angle defined with 3 points.
    pub struct AnglePoint {
        pub arm1: Arc<Expression>,
        pub origin: Arc<Expression>,
        pub arm2: Arc<Expression>,
    }

    impl Evaluate for AnglePoint {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let arm1 = self.arm1.evaluate(args)?.as_point().unwrap();
            let origin = self.origin.evaluate(args)?.as_point().unwrap();
            let arm2 = self.arm2.evaluate(args)?.as_point().unwrap();

            Ok(Value::scalar(
                geometry::get_angle(arm1.position, origin.position, arm2.position),
                unit::ANGLE
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.arm1.weights + &self.origin.weights + &self.arm2.weights
        }
    }

    #[derive(Debug, Clone)]
    /// An angle defined with 2 lines.
    pub struct AngleLine {
        pub line1: Arc<Expression>,
        pub line2: Arc<Expression>
    }

    impl Evaluate for AngleLine {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            // Evaluate the two points
            let line1 = self.line1.evaluate(args)?.as_line().unwrap();
            let line2 = self.line2.evaluate(args)?.as_line().unwrap();

            // Divide direction vectors and get the arg.
            let div = line1.direction / line2.direction;

            Ok(Value::scalar(
                div.arg(),
                unit::ANGLE
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            self.line1.weights + &self.line2.weights
        }
    }

    #[derive(Debug, Clone)]
    /// An angle defined with 2 lines.
    pub struct Literal {
        pub value: f64,
        pub unit: ComplexUnit
    }

    impl Evaluate for Literal {
        fn evaluate(&self, args: &EvaluationArgs) -> Result<Value, EvaluationError> {
            Ok(Value::scalar(
                self.value,
                self.unit
            ))
        }

        fn evaluate_weights(&self) -> Weights {
            Weights::empty()
        }
    }

    #[derive(Debug, Clone)]
    /// An angle defined with 2 lines.
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
}

/// Defines an expression kind.
#[derive(Debug, Clone)]
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
    Line(Arc<Expression>, Arc<Expression>),
    /// The point where two lines cross.
    LineLineIntersection(Arc<Expression>, Arc<Expression>),
    /// Changes the unit
    SetUnit(Arc<Expression>, ComplexUnit),
    /// Adds two values
    Sum(Arc<Expression>, Arc<Expression>),
    /// Subtracts two values
    Difference(Arc<Expression>, Arc<Expression>),
    /// Multiplies two values
    Product(Arc<Expression>, Arc<Expression>),
    /// Divides two values
    Quotient(Arc<Expression>, Arc<Expression>),
    /// Changes the sign
    Negation(Arc<Expression>),
    /// An angle bisector.
    AngleBisector(
        Arc<Expression>,
        Arc<Expression>,
        Arc<Expression>,
    ),
    /// Takes the average value (arithmetic mean)
    Average(Vec<Arc<Expression>>),
    /// Generates a line perpendicular to $1 going through $2
    PerpendicularThrough(Arc<Expression>, Arc<Expression>),
    /// Generates a line parallel to $1 going through $2
    ParallelThrough(Arc<Expression>, Arc<Expression>),
    /// An adjusted real value
    Real(usize)
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
        }
    }

    fn evaluate_weights(&self) -> Weights {
        match self {
            Self::PointPointDistance(v) => v.evaluate_weigts(),
            Self::PointLineDistance(v) => v.evaluate_weigts(),
            Self::AnglePoint(v) => v.evaluate_weigts(),
            Self::AngleLine(v) => v.evaluate_weigts(),
            Self::Literal(v) => v.evaluate_weigts(),
            Self::FreePoint(v) => v.evaluate_weigts(),
            Self::Line(v) => v.evaluate_weigts(),
            Self::LineLineIntersection(v) => v.evaluate_weigts(),
            Self::SetUnit(v) => v.evaluate_weigts(),
            Self::Sum(v) => v.evaluate_weigts(),
            Self::Difference(v) => v.evaluate_weigts(),
            Self::Product(v) => v.evaluate_weigts(),
            Self::Quotient(v) => v.evaluate_weigts(),
            Self::Negation(v) => v.evaluate_weigts(),
            Self::AngleBisector(v) => v.evaluate_weigts(),
            Self::Average(v) => v.evaluate_weigts(),
            Self::PerpendicularThrough(v) => v.evaluate_weigts(),
            Self::ParallelThrough(v) => v.evaluate_weigts(),
            Self::Real(v) => v.evaluate_weigts(),
        }
    }
}