use std::{sync::Arc, ops::{Mul, Deref, DerefMut, Div}, hash::Hash};

pub mod figure;

/// Defines an object with assigned weight
#[derive(Debug)]
pub struct Weighed<T> {
    pub object: T,
    pub weight: f64
}

/// Defines a simple unit.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SimpleUnit {
    Distance,
    Point,
    Angle,
    Line,
    Scalar
}

impl Mul for SimpleUnit {
    type Output = ComplexUnit;

    fn mul(self, rhs: Self) -> Self::Output {
        let complex = ComplexUnit::new(self);
        
        complex * rhs
    }
}

/// Defines a complex unit: a product of simple units.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ComplexUnit([i8; SimpleUnit::Scalar as usize]);

impl ComplexUnit {
    pub fn new(simple: SimpleUnit) -> Self {
        let mut arr = [0; SimpleUnit::Scalar as usize];

        match simple {
            SimpleUnit::Scalar => (),
            _ => arr[simple as usize] = 1
        }

        Self(arr)
    }
}

impl Mul<SimpleUnit> for ComplexUnit {
    type Output = ComplexUnit;

    fn mul(mut self, rhs: SimpleUnit) -> Self::Output {
        match rhs {
            SimpleUnit::Scalar => (),
            // Clippy doesn't like exponentiation. Thanks, Clippy
            #[allow(clippy::suspicious_arithmetic_impl)]
            _ => self[rhs as usize] += 1
        }
        self
    }
}

impl Mul for ComplexUnit {
    type Output = ComplexUnit;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self.iter_mut().enumerate().map(|(i, x)| *x += rhs[i]).for_each(drop);
        self
    }
}

impl Div<SimpleUnit> for ComplexUnit {
    type Output = ComplexUnit;

    fn div(mut self, rhs: SimpleUnit) -> Self::Output {
        match rhs {
            SimpleUnit::Scalar => (),
            // Oh, c'mon, Clippy
            #[allow(clippy::suspicious_arithmetic_impl)]
            _ => self[rhs as usize] -= 1
        }
        self
    }
}

impl Div for ComplexUnit {
    type Output = ComplexUnit;

    fn div(mut self, rhs: Self) -> Self::Output {
        self.iter_mut().enumerate().map(|(i, x)| *x -= rhs[i]).for_each(drop);
        self
    }
}

impl Deref for ComplexUnit {
    type Target = [i8; SimpleUnit::Scalar as usize];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ComplexUnit {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Defines an expression.
#[derive(Debug)]
pub enum Expression {
    /// Euclidean distance between two points.
    PointPointDistance(Box<Weighed<Expression>>, Box<Weighed<Expression>>),
    /// Euclidean distance between a point and its rectangular projection onto a line.
    PointLineDistance(Box<Weighed<Expression>>, Box<Weighed<Expression>>),
    /// An angle defined with 3 points.
    AnglePoint(Box<Weighed<Expression>>, Box<Weighed<Expression>>, Box<Weighed<Expression>>),
    /// A real literal.
    Literal(f64, ComplexUnit),
    /// An adjustable indexed point in euclidean space
    FreePoint(usize),
    /// A line in euclidean space. defined by two points.
    Line(Box<Weighed<Expression>>, Box<Weighed<Expression>>),
    /// The point where two lines cross.
    LineCrossing(Box<Weighed<Expression>>, Box<Weighed<Expression>>)
}

/// Defines the kind and information about criteria the figure must obey.
#[derive(Debug)]
pub enum CriteriaKind {
    /// Equality. Quality rises quickly as two values approach each other, drops quickly as their difference grows.
    Equal(Box<Weighed<Expression>>, Box<Weighed<Expression>>),
    /// Less. Quality starts rising on equality.
    Less(Box<Weighed<Expression>>, Box<Weighed<Expression>>),
    /// Greater. Quality starts rising on equality.
    Greater(Box<Weighed<Expression>>, Box<Weighed<Expression>>),
    /// Inverts the criteria. The quality is calculated as 1 - the quality of the inverted criteria.
    Inverse(Box<CriteriaKind>)
}

/// Defines a weighed piece of criteria the figure must obey.
pub type Criteria = Weighed<CriteriaKind>;

pub struct HashableArc<T>(pub Arc<T>);

impl<T> Hash for HashableArc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}

impl<T> PartialEq for HashableArc<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.0) == Arc::as_ptr(&other.0)
    }
}

impl<T> Eq for HashableArc<T> {
    fn assert_receiver_is_total_eq(&self) {
        
    }
}