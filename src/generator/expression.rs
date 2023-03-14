use std::{sync::Arc, ops::{Add, Mul, AddAssign}};

use crate::script::{ComplexUnit, unit};

use super::Complex;

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

/// Defines an expression kind.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Euclidean distance between two points.
    PointPointDistance(Arc<Expression>, Arc<Expression>),
    /// Euclidean distance between a point and its rectangular projection onto a line.
    PointLineDistance(Arc<Expression>, Arc<Expression>),
    /// An angle defined with 3 points.
    AnglePoint(
        Arc<Expression>,
        Arc<Expression>,
        Arc<Expression>,
    ),
    /// An angle defined with 2 lines.
    AngleLine(Arc<Expression>, Arc<Expression>),
    /// A real literal.
    Literal(f64, ComplexUnit),
    /// An adjustable indexed point in euclidean space
    FreePoint(usize),
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

impl ExprKind {
    // pub fn collect<'r>(&'r self, into: &mut Vec<&'r Arc<Expression>>) {
    //     match self {
    //         ExprKind::PointPointDistance(e1, e2)
    //         | ExprKind::AngleLine(e1, e2)
    //         | ExprKind::Line(e1, e2)
    //         | ExprKind::LineLineIntersection(e1, e2)
    //         | ExprKind::Sum(e1, e2)
    //         | ExprKind::Difference(e1, e2)
    //         | ExprKind::Product(e1, e2)
    //         | ExprKind::Quotient(e1, e2)
    //         | ExprKind::PerpendicularThrough(e1, e2)
    //         | ExprKind::ParallelThrough(e1, e2)
    //         | ExprKind::PointLineDistance(e1, e2) => {
    //             into.push(e1);
    //             e1.object.collect(into);
    //             into.push(e2);
    //             e2.object.collect(into);
    //         }
    //         ExprKind::AnglePoint(e1, e2, e3) | ExprKind::AngleBisector(e1, e2, e3) => {
    //             into.push(e1);
    //             e1.object.collect(into);
    //             into.push(e2);
    //             e2.object.collect(into);
    //             into.push(e3);
    //             e3.object.collect(into);
    //         }
    //         ExprKind::Literal(_, _) | ExprKind::FreePoint(_) | ExprKind::Real(_) => (),
    //         ExprKind::SetUnit(e, _) | ExprKind::Negation(e) => {
    //             into.push(e);
    //             e.object.collect(into);
    //         }
    //         ExprKind::Average(v) => {
    //             for e in v {
    //                 into.push(e);
    //                 e.object.collect(into);
    //             }
    //         }
    //     }
    // }

    pub fn evaluate_weights(&self) -> Weights {
        match self {
            Self::PointPointDistance(e1, e2)
            | Self::AngleLine(e1, e2)
            | Self::Line(e1, e2)
            | Self::LineLineIntersection(e1, e2)
            | Self::Sum(e1, e2)
            | Self::Difference(e1, e2)
            | Self::Product(e1, e2)
            | Self::Quotient(e1, e2)
            | Self::PerpendicularThrough(e1, e2)
            | Self::ParallelThrough(e1, e2)
            | Self::PointLineDistance(e1, e2) => {
                e1.weights.clone() + &e2.weights
            }
            Self::AnglePoint(e1, e2, e3)
            | Self::AngleBisector(e1, e2, e3) => {
                e1.weights.clone() + &e2.weights + &e3.weights
            }
            Self::Literal(_, _) => Weights::empty(),
            Self::FreePoint(i) | Self::Real(i) => Weights::one_at(*i),
            Self::SetUnit(e, _) | Self::Negation(e) => {
                e.weights.clone()
            }
            Self::Average(v) => {
                let mut ws = Weights::empty();

                for e in v {
                    ws += &e.weights;
                }

                ws
            }
        }
    }
}