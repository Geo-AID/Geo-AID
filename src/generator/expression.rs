use std::sync::Arc;

use crate::script::{ComplexUnit, Weighed};

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
    pub fn new(expr: ExprKind) -> Self {
        Self {

        }
    }
}

/// Defines an expression kind.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Euclidean distance between two points.
    PointPointDistance(Arc<Weighed<ExprKind>>, Arc<Weighed<ExprKind>>),
    /// Euclidean distance between a point and its rectangular projection onto a line.
    PointLineDistance(Arc<Weighed<ExprKind>>, Arc<Weighed<ExprKind>>),
    /// An angle defined with 3 points.
    AnglePoint(
        Arc<Weighed<ExprKind>>,
        Arc<Weighed<ExprKind>>,
        Arc<Weighed<ExprKind>>,
    ),
    /// An angle defined with 2 lines.
    AngleLine(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// A real literal.
    Literal(f64, ComplexUnit),
    /// An adjustable indexed point in euclidean space
    FreePoint(usize),
    /// A line in euclidean space. defined by two points.
    Line(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// The point where two lines cross.
    LineLineIntersection(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Changes the unit
    SetUnit(Arc<Weighed<Expression>>, ComplexUnit),
    /// Adds two values
    Sum(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Subtracts two values
    Difference(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Multiplies two values
    Product(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Divides two values
    Quotient(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Changes the sign
    Negation(Arc<Weighed<Expression>>),
    /// An angle bisector.
    AngleBisector(
        Arc<Weighed<Expression>>,
        Arc<Weighed<Expression>>,
        Arc<Weighed<Expression>>,
    ),
    /// Takes the average value (arithmetic mean)
    Average(Vec<Arc<Weighed<Expression>>>),
    /// Generates a line perpendicular to $1 going through $2
    PerpendicularThrough(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Generates a line parallel to $1 going through $2
    ParallelThrough(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// An adjusted real value
    Real(usize)
}

impl ExprKind {
    pub fn collect<'r>(&'r self, into: &mut Vec<&'r Arc<Weighed<ExprKind>>>) {
        match self {
            ExprKind::PointPointDistance(e1, e2)
            | ExprKind::AngleLine(e1, e2)
            | ExprKind::Line(e1, e2)
            | ExprKind::LineLineIntersection(e1, e2)
            | ExprKind::Sum(e1, e2)
            | ExprKind::Difference(e1, e2)
            | ExprKind::Product(e1, e2)
            | ExprKind::Quotient(e1, e2)
            | ExprKind::PerpendicularThrough(e1, e2)
            | ExprKind::ParallelThrough(e1, e2)
            | ExprKind::PointLineDistance(e1, e2) => {
                into.push(e1);
                e1.object.collect(into);
                into.push(e2);
                e2.object.collect(into);
            }
            ExprKind::AnglePoint(e1, e2, e3) | ExprKind::AngleBisector(e1, e2, e3) => {
                into.push(e1);
                e1.object.collect(into);
                into.push(e2);
                e2.object.collect(into);
                into.push(e3);
                e3.object.collect(into);
            }
            ExprKind::Literal(_, _) | ExprKind::FreePoint(_) | ExprKind::Real(_) => (),
            ExprKind::SetUnit(e, _) | ExprKind::Negation(e) => {
                into.push(e);
                e.object.collect(into);
            }
            ExprKind::Average(v) => {
                for e in v {
                    into.push(e);
                    e.object.collect(into);
                }
            }
        }
    }

    pub fn evaluate_weights(&self) -> Weights {
        match self {
            ExprKind::PointPointDistance(_, _) => todo!(),
            ExprKind::PointLineDistance(_, _) => todo!(),
            ExprKind::AnglePoint(_, _, _) => todo!(),
            ExprKind::AngleLine(_, _) => todo!(),
            ExprKind::Literal(_, _) => todo!(),
            ExprKind::FreePoint(_) => todo!(),
            ExprKind::Line(_, _) => todo!(),
            ExprKind::LineLineIntersection(_, _) => todo!(),
            ExprKind::SetUnit(_, _) => todo!(),
            ExprKind::Sum(_, _) => todo!(),
            ExprKind::Difference(_, _) => todo!(),
            ExprKind::Product(_, _) => todo!(),
            ExprKind::Quotient(_, _) => todo!(),
            ExprKind::Negation(_) => todo!(),
            ExprKind::AngleBisector(_, _, _) => todo!(),
            ExprKind::Average(_) => todo!(),
            ExprKind::PerpendicularThrough(_, _) => todo!(),
            ExprKind::ParallelThrough(_, _) => todo!(),
            ExprKind::Real(_) => todo!(),
        }
    }
}