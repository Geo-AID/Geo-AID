/*
 Copyright (c) 2024 Michał Wilczek, Michał Margos

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

use std::cell::OnceCell;
use std::collections::HashMap;
use std::iter::Peekable;
use std::mem;
use std::ops::{Deref, DerefMut, SubAssign};
use derive_recursive::Recursive;
use num_traits::{FromPrimitive, Zero};

use crate::generator::AdjustableTemplate;
use crate::script::figure::Item;
use crate::script::math::optimizations::ZeroLineDst;
use crate::script::token::number::{CompExponent, ProcNum};
use crate::script::unroll::figure::Node;

use super::{figure::Figure, unroll::{self, Displayed, Expr as Unrolled, UnrolledRule, UnrolledRuleKind,
                                     Point as UnrolledPoint, Line as UnrolledLine, Circle as UnrolledCircle, ScalarData as UnrolledScalar}, Error, ComplexUnit, SimpleUnit};

mod optimizations;

trait HandleEntity {
    fn contains_entity(&self, entity: usize) -> bool;

    fn map_entity(&mut self, with: &[usize]);
}

trait Var {
    fn var(id: usize) -> Self;
}

trait HasMeta {
    type Meta;
}

impl<T: HasMeta> HasMeta for Vec<T> {
    type Meta = T::Meta;
}

impl<T: HasMeta> HasMeta for Box<T> {
    type Meta = T::Meta;
}

trait MapMeta<Dst>: HasMeta {
    type Output;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, f: F) -> Self::Output;
}

impl<Dst, T: MapMeta<Dst>> MapMeta<Dst> for Vec<T> {
    type Output = Vec<T::Output>;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, f: F) -> Self::Output {
        self.into_iter().map(|x| x.map_meta(f)).collect()
    }
}

impl<Dst, T: MapMeta<Dst>> MapMeta<Dst> for Box<T> {
    type Output = Box<T::Output>;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, f: F) -> Self::Output {
        Box::new((*self).map_meta(f))
    }
}

trait FromUnrolled<T: Displayed> {
    fn load(expr: &Unrolled<T>, math: &mut Expand) -> Self;
}

trait Normalize {
    fn normalize(&mut self);
}

pub trait LoadsTo {
    type Output;
}

impl LoadsTo for UnrolledPoint {
    type Output = PointExpr<()>;
}

impl LoadsTo for UnrolledLine {
    type Output = LineExpr<()>;
}

impl LoadsTo for UnrolledCircle {
    type Output = CircleExpr<()>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive)]
#[recursive(
    impl<M> HandleEntity for Self<M> {
        fn contains_entity(&self, entity: EntityId) -> bool {
            aggregate = ||
        }
    }
)]
pub enum Point<M> {
    Var {
        id: usize
    },
    /// k and l must be ordered
    LineLineIntersection {
        k: LineExpr<M>,
        l: LineExpr<M>
    },
    /// items must be sorted
    Average {
        items: Vec<PointExpr<M>>
    },
    CircleCenter {
        circle: CircleExpr<M>
    },
    Entity {
        id: usize
    }
}

pub type PointExpr<M> = Expr<Point<M>, M>;

impl FromUnrolled<UnrolledPoint> for PointExpr<()> {
    fn load(expr: &Unrolled<UnrolledPoint>, math: &mut Expand) -> Self {
        let kind = match expr.get_data() {
            UnrolledPoint::LineLineIntersection(a, b) => Point::LineLineIntersection {
                k: math.load(a),
                l: math.load(b)
            },
            UnrolledPoint::Average(exprs) => Point::Average {
                items: exprs.iter().map(|x| math.load(x)).collect()
            },
            UnrolledPoint::CircleCenter(circle) => {
                match circle.get_data() {
                    UnrolledCircle::Circle(center, _) => return math.load(center),
                    _ => unreachable!()
                }
            },
            UnrolledPoint::Free => Point::Entity { id: math.add_point() },
            _ => unreachable!()
        };

        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

impl<M: Ord> Normalize for Point<M> {
    fn normalize(&mut self) {
        match self {
            Point::Entity { .. }
            | Point::Var { .. } => (),
            Point::LineLineIntersection { k, l } => {
                k.normalize();
                l.normalize();

                if k > l {
                    mem::swap(k, l);
                }
            }
            Point::Average { items } => {
                for item in items {
                    item.normalize();
                }

                items.sort();
            }
            Point::CircleCenter { circle } => circle.normalize(),
        }
    }
}

/// Normalized if has sorted parameters and potential additional conditions are met.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Number<M> {
    Var {
        id: usize
    },
    Entity {
        id: usize
    },
    /// plus and minus must be sorted and must not contain other sums. An aggregated constant, if any, must be at the end.
    Sum {
        plus: Vec<NumberExpr<M>>,
        minus: Vec<NumberExpr<M>>
    },
    /// times and by must be sorted and must not contain other sums. An aggregated constant, if any, must be at the end.
    Product {
        times: Vec<NumberExpr<M>>,
        by: Vec<NumberExpr<M>>
    },
    Const {
        value: ProcNum
    },
    Power {
        value: NumberExpr<M>,
        exponent: CompExponent
    },
    /// p and q must be ordered
    PointPointDistance {
        p: PointExpr<M>,
        q: PointExpr<M>
    },
    PointLineDistance {
        p: PointExpr<M>,
        k: LineExpr<M>
    },
    /// p and r must be ordered
    ThreePointAngle {
        p: PointExpr<M>,
        q: PointExpr<M>,
        r: PointExpr<M>
    },
    /// p and r must be ordered
    ThreePointAngleDir {
        p: PointExpr<M>,
        q: PointExpr<M>,
        r: PointExpr<M>
    },
    /// k and l must be ordered
    TwoLineAngle {
        k: LineExpr<M>,
        l: LineExpr<M>
    },
    PointX {
        point: PointExpr<M>
    },
    PointY {
        point: PointExpr<M>
    }
}

pub type NumberExpr<M> = Expr<Number<M>, M>;

impl<M> HandleEntity for Number<M> {
    fn contains_entity(&self, entity: usize) -> bool {
        match self {
            Number::Const { .. }
            | Number::Var { .. } => false,
            Number::Entity { id } => *id == entity,
            Number::Sum { plus: a, minus: b }
            | Number::Product { times: a, by: b } => {
                a.iter().any(|x| x.contains_entity(entity))
                || b.iter().any(|x| x.contains_entity(entity))
            }
            Number::Power { value: v, .. } => v.contains_entity(entity),
            Number::PointPointDistance { p, q } => {
                p.contains_entity(entity) || q.contains_entity(entity)
            }
            Number::PointLineDistance { p, k } => {
                p.contains_entity(entity) || k.contains_entity(entity)
            }
            Number::ThreePointAngle { p, q, r }
            | Number::ThreePointAngleDir { p, q, r } => {
                p.contains_entity(entity)
                || q.contains_entity(entity)
                || r.contains_entity(entity)
            }
            Number::TwoLineAngle { k, l } => {
                k.contains_entity(entity) || l.contains_entity(entity)
            }
            Number::PointX { point }
            | Number::PointY { point } => point.contains_entity(entity)
        }
    }
}

impl<M> Var for Number<M> {
    fn var(id: usize) -> Self {
        Self::Var { id }
    }
}

fn fix_dst(expr: NumberExpr<()>, unit: Option<ComplexUnit>, math: &mut Expand) -> NumberExpr<()> {
    match unit {
        None => expr,
        Some(unit) => {
            if unit.0[SimpleUnit::Distance as usize].is_zero() {
                expr
            } else {
                Expr::new(Number::Product {
                    times: vec![expr, Expr::new(Number::Power {
                        value: math.get_dst_var(),
                        exponent: unit.0[SimpleUnit::Distance as usize]
                    })],
                    by: Vec::new()
                })
            }
        }
    }
}

impl FromUnrolled<unroll::Scalar> for NumberExpr<()> {
    fn load(expr: &Unrolled<unroll::Scalar>, math: &mut Expand) -> Self {
        let mut kind = match expr.get_data() {
            UnrolledScalar::Add(a, b) => Number::Sum {
                plus: vec![math.load(a), math.load(b)],
                minus: Vec::new()
            },
            UnrolledScalar::Subtract(a, b) => Number::Sum {
                plus: vec![math.load(a)],
                minus: vec![math.load(b)]
            },
            UnrolledScalar::Multiply(a, b) => Number::Product {
                times: vec![math.load(a), math.load(b)],
                by: Vec::new()
            },
            UnrolledScalar::Divide(a, b) => Number::Product {
                times: vec![math.load(a)],
                by: vec![math.load(b)]
            },
            UnrolledScalar::Average(exprs) => Number::Product {
                times: vec![Expr::new(Number::Sum {
                    plus: exprs.iter().map(|x| math.load(x)).collect(),
                    minus: Vec::new()
                })],
                by: vec![Expr::new(Number::Const { value: ProcNum::from_usize(exprs.len()).unwrap() })]
            },
            UnrolledScalar::CircleRadius(circle) => {
                match circle.get_data() {
                    UnrolledCircle::Circle(_, radius) => return math.load(radius),
                    _ => unreachable!()
                }
            }
            UnrolledScalar::Free => Number::Entity { id: math.add_real() },
            UnrolledScalar::Number(x) => return fix_dst(Expr::new(Number::Const { value: x.clone() }), expr.data.unit, math),
            UnrolledScalar::DstLiteral(x) => Number::Const { value: x.clone() },
            UnrolledScalar::SetUnit(x, unit) => return fix_dst(math.load(x), Some(*unit), math),
            UnrolledScalar::PointPointDistance(p, q) => Number::PointPointDistance {
                p: math.load(p),
                q: math.load(q)
            },
            UnrolledScalar::PointLineDistance(p, k) => Number::PointLineDistance {
                p: math.load(p),
                k: math.load(k)
            },
            UnrolledScalar::Negate(x) => Number::Sum {
                plus: Vec::new(),
                minus: vec![math.load(x)]
            },
            UnrolledScalar::ThreePointAngle(p, q, r) => Number::ThreePointAngle {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r)
            },
            UnrolledScalar::ThreePointAngleDir(p, q, r) => Number::ThreePointAngleDir {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r)
            },
            UnrolledScalar::TwoLineAngle(k, l) => Number::TwoLineAngle {
                k: math.load(k),
                l: math.load(l)
            },
            UnrolledScalar::Pow(base, exponent) => Number::Power {
                value: math.load(base),
                exponent: exponent.clone()
            },
            UnrolledScalar::PointX(point) => Number::PointX {
                point: math.load(point)
            },
            UnrolledScalar::PointY(point) => Number::PointY {
                point: math.load(point)
            },
            _ => unreachable!()
        };

        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Merge<T, I, J>
    where T: Ord, I: Iterator<Item = T>, J: Iterator<Item = T> {
    i: Peekable<I>,
    j: Peekable<J>
}

impl<T: Ord, I: Iterator<Item = T>, J: Iterator<Item = T>> Merge<T, I, J> {
    #[must_use]
    pub fn new<A: IntoIterator<IntoIter = I>, B: IntoIterator<IntoIter = J>>(a: A, b: B) -> Self {
        Self {
            i: a.into_iter().peekable(),
            j: b.into_iter().peekable()
        }
    }

    #[must_use]
    pub fn empty() -> Self {
        Self::new(None, None)
    }

    #[must_use]
    pub fn merge_with<It: IntoIterator<Item = T>>(self, other: It) -> Merge<T, Self, It::IntoIter> {
        Merge::new(self, other)
    }
}

impl<T: Ord, I: Iterator<Item = T>, J: Iterator<Item = T>> Iterator for Merge<T, I, J> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(i_item) = self.i.peek() {
            if let Some(j_item) = self.j.peek() {
                if j_item > i_item {
                    self.i.next()
                } else {
                    self.j.next()
                }
            } else {
                self.i.next()
            }
        } else {
            self.j.next()
        }
    }
}

fn normalize_sum(plus: &mut Vec<NumberExpr<()>>, minus: &mut Vec<NumberExpr<()>>) {
    let plus_v = mem::take(plus);
    let minus_v = mem::take(minus);

    let mut constant = ProcNum::zero();

    let mut plus_final = Vec::new();
    let mut minus_final = Vec::new();

    for mut item in plus_v {
        item.normalize();

        match *item.kind {
            Number::Sum {
                plus, minus
            } => {
                plus_final = Merge::new(plus_final, plus).collect();
                minus_final = Merge::new(minus_final, minus).collect();
            }
            Number::Const { value } => constant += value,
            kind => {
                plus_final = Merge::new(plus_final, Some(Expr::new(kind))).collect();
            }
        }
    }

    for item in minus_v {
        match *item.kind {
            Number::Sum {
                plus, minus
            } => {
                plus_final = Merge::new(plus_final, minus).collect();
                minus_final = Merge::new(minus_final, plus).collect();
            }
            Number::Const { value } => constant -= value,
            kind => {
                minus_final = Merge::new(minus_final, Some(Expr::new(kind))).collect();
            }
        }
    }

    if !constant.is_zero() {
        plus_final.push(Expr::new(Number::Const { value: constant }));
    }

    *plus = plus_final;
    *minus = minus_final;
}

fn normalize_product(times: &mut Vec<NumberExpr<()>>, by: &mut Vec<NumberExpr<()>>) {
    let times_v = mem::take(times);
    let by_v = mem::take(by);

    let mut constant = ProcNum::one();

    let mut times_final = Vec::new();
    let mut by_final = Vec::new();

    for mut item in times_v {
        item.normalize();

        match *item.kind {
            Number::Product {
                times, by
            } => {
                times_final = Merge::new(times_final, times).collect();
                by_final = Merge::new(by_final, by).collect();
            }
            Number::Const { value } => constant *= value,
            kind => {
                times_final = Merge::new(times_final, Some(Expr::new(kind))).collect();
            }
        }
    }

    for item in by_v {
        match *item.kind {
            Number::Product {
                times, by
            } => {
                times_final = Merge::new(times_final, by).collect();
                by_final = Merge::new(by_final, times).collect();
            }
            Number::Const { value } => constant /= value,
            kind => {
                by_final = Merge::new(by_final, Some(Expr::new(kind))).collect();
            }
        }
    }

    if !constant.is_one() {
        times_final.push(Expr::new(Number::Const { value: constant }));
    }

    *times = times_final;
    *by = by_final;
}

impl<M: Ord> Normalize for Number<M> {
    fn normalize(&mut self) {
        match self {
            Self::Var { .. }
            | Self::Const { .. }
            | Self::Entity { .. } => (),
            Self::Sum { plus, minus } => {
                normalize_sum(plus, minus);
            }
            Self::Product { times, by } => {
                normalize_product(times, by);
            }
            Self::Power { value, .. } => value.normalize(),
            Self::PointPointDistance { p, q } => {
                p.normalize();
                q.normalize();

                if p > q {
                    mem::swap(p, q);
                }
            }
            Self::PointLineDistance { p, k } => {
                p.normalize();
                k.normalize();
            }
            Self::ThreePointAngle { p, q, r }
            | Self::ThreePointAngleDir { p, q, r } => {
                p.normalize();
                q.normalize();
                r.normalize();

                if p > r {
                    mem::swap(p, r);
                }
            }
            Self::TwoLineAngle { k, l } => {
                k.normalize();
                l.normalize();

                if k > l {
                    mem::swap(k, l);
                }
            }
            Self::PointX { point }
            | Self::PointY { point } => point.normalize(),
        }
    }
}

/// Beyond their specific conditions, all variants are
/// only normalized if their operands are normalized.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Line<M> {
    /// Always normalized
    Var {
        id: usize
    },
    /// Normalized iff `p` and `q` are in ascending order
    PointPoint {
        p: PointExpr<M>,
        q: PointExpr<M>
    },
    /// Normalized iff `a` and `c` are in ascending order (`b` must stay in the middle)
    AngleBisector {
        a: PointExpr<M>,
        b: PointExpr<M>,
        c: PointExpr<M>
    },
    /// Always normalized
    ParallelThrough {
        point: PointExpr<M>,
        line: LineExpr<M>
    },
    /// Always normalized
    PerpendicularThrough {
        point: PointExpr<M>,
        line: LineExpr<M>
    }
}

pub type LineExpr<M> = Expr<Line<M>, M>;

impl<M> HandleEntity for Line<M> {
    fn contains_entity(&self, entity: usize) -> bool {
        match self {
            Line::Var { .. } => false,
            Line::PointPoint { p, q } => {
                p.contains_entity(entity) || q.contains_entity(entity)
            }
            Line::AngleBisector { a, b, c } => {
                a.contains_entity(entity) || b.contains_entity(entity) || c.contains_entity(entity)
            }
            Line::ParallelThrough { point, line }
            | Line::PerpendicularThrough { point, line } => {
                point.contains_entity(entity) || line.contains_entity(entity)
            }
        }
    }
}

impl<M> Var for Line<M> {
    fn var(id: usize) -> Self {
        Self::Var { id }
    }
}

impl FromUnrolled<UnrolledLine> for LineExpr<()> {
    fn load(expr: &Unrolled<UnrolledLine>, math: &mut Expand) -> Self {
        let kind = match expr.get_data() {
            UnrolledLine::LineFromPoints(a, b) => Line::PointPoint {
                p: math.load(a),
                q: math.load(b)
            },
            UnrolledLine::AngleBisector(a, b, c) => Line::AngleBisector {
                a: math.load(a),
                b: math.load(b),
                c: math.load(c),
            },
            UnrolledLine::PerpendicularThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => {
                        Line::ParallelThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    UnrolledLine::ParallelThrough(l, _) => {
                        Line::PerpendicularThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    _ => Line::PerpendicularThrough {
                        point: math.load(p),
                        line: math.load(k)
                    }
                }
            }
            UnrolledLine::ParallelThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => {
                        Line::PerpendicularThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    UnrolledLine::ParallelThrough(l, _) => {
                        Line::ParallelThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    _ => Line::ParallelThrough {
                        point: math.load(p),
                        line: math.load(k)
                    }
                }
            },
            _ => unreachable!()
        };

        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

impl<M: Ord> Normalize for Line<M> {
    fn normalize(&mut self) {
        // Simplification.
        *self = match self {
            Self::ParallelThrough { mut point, mut line } => {
                point.normalize();
                line.normalize();

                match *line.kind {
                    Self::ParallelThrough { line, .. } => Self::ParallelThrough { point, line },
                    Self::PerpendicularThrough { line, .. } => Self::PerpendicularThrough { point, line },
                    _ => Self::ParallelThrough { point, line }
                }
            }
            Self::PerpendicularThrough { mut point, mut line } => {
                point.normalize();
                line.normalize();

                match *line.kind { 
                    Self::ParallelThrough { line, .. } => Self::PerpendicularThrough { point, line },
                    Self::PerpendicularThrough { line, .. } => Self::ParallelThrough { point, line },
                    _ => Self::PerpendicularThrough { point, line }
                }
            }
            Self::Var { .. } => return,
            // Reorder if necessary
            Self::PointPoint { mut p, mut q } => {
                p.normalize();
                q.normalize();

                if p > q {
                    mem::swap(&mut p, &mut q);
                }

                Self::PointPoint { p, q }
            }
            Self::AngleBisector { mut a, mut b, mut c } => {
                a.normalize();
                b.normalize();
                c.normalize();

                if a > c {
                    mem::swap(&mut a, &mut c);
                }

                Self::AngleBisector { a, b, c }
            }
        };
    }
}

/// Normalized if its parameters are normalized and potential additional conditions are met.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Circle<M> {
    /// Used to represent a variable in script or a variadic in a pattern.
    Var {
        id: usize
    },
    Construct {
        center: PointExpr<M>,
        radius: NumberExpr<M>
    }
}

pub type CircleExpr<M> = Expr<Circle<M>, M>;

impl<M> HandleEntity for Circle<M> {
    fn contains_entity(&self, entity: usize) -> bool {
        match self {
            Circle::Var { .. } => false,
            Circle::Construct { center, radius } => {
                center.contains_entity(entity) || radius.contains_entity(entity)
            }
        }
    }
}

impl<M: Ord> Normalize for Circle<M> {
    fn normalize(&mut self) {
        match self {
            Circle::Var { .. } => (),
            Circle::Construct { mut center, mut radius } => {
                center.normalize();
                radius.normalize();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Any<M> {
    Point(Point<M>),
    Number(Number<M>),
    Line(Line<M>)
}

pub type AnyExpr<M> = Expr<Any<M>, M>;

impl<M> From<Number<M>> for Any<M> {
    fn from(value: Number<M>) -> Self {
        Self::Number(value)
    }
}

impl<M> From<Line<M>> for Any<M> {
    fn from(value: Line<M>) -> Self {
        Self::Line(value)
    }
}

impl<M> From<Point<M>> for Any<M> {
    fn from(value: Point<M>) -> Self {
        Self::Point(value)
    }
}

#[derive(Debug, Clone)]
pub struct AlwaysEq<T>(T);

impl<T> PartialEq for AlwaysEq<T> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T> Eq for AlwaysEq<T> {}

impl<T> PartialOrd for AlwaysEq<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for AlwaysEq<T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl<T> Deref for AlwaysEq<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for AlwaysEq<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expr<T, M> {
    pub kind: Box<T>,
    pub meta: M
}

impl<T: HandleEntity, M> HandleEntity for Expr<T, M> {
    fn contains_entity(&self, entity: usize) -> bool {
        self.kind.contains_entity(entity)
    }
}

impl<T: Var> Var for Expr<T, ()> {
    fn var(id: usize) -> Self {
        Self {
            kind: Box::new(T::var(id)),
            meta: ()
        }
    }
}

impl<T: Normalize, M> Normalize for Expr<T, M> {
    fn normalize(&mut self) {
        self.kind.normalize();
    }
}

impl<T, M> HasMeta for Expr<T, M> {
    type Meta = M;
}

impl<M, Dst, T: MapMeta<Dst> + HasMeta<Meta = M>> MapMeta<Dst> for Expr<T, M> {
    type Output = Expr<T::Output, Dst>;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, mut f: F) -> Self::Output {
        Expr {
            kind: self.kind.map_meta(f),
            meta: f(self.meta)
        }
    }
}

impl<T> Expr<T, ()> {
    #[must_use]
    pub const fn new(kind: T) -> Self {
        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

impl<T, M> Expr<T, M> where Any<M>: From<T> {
    #[must_use]
    pub fn into_any(self) -> AnyExpr<M> {
        AnyExpr {
            kind: Box::new((*self.kind).into()),
            meta: self.meta
        }
    }
}

/// Represents a rule of the figure.
/// Rules are normalized iff:
/// * their operands are normalized
/// * their operands are sorted in ascending order
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rule<M> {
    PointEq(PointExpr<M>, PointExpr<M>),
    NumberEq(NumberExpr<M>, NumberExpr<M>),
    Lt(NumberExpr<M>, NumberExpr<M>),
    Gt(NumberExpr<M>, NumberExpr<M>),
    Alternative(Vec<Rule<M>>),
    Invert(Box<Rule<M>>),
    Bias
}

impl Rule<()> {
    /// # Returns
    /// A normalized rule.
    fn load(rule: &UnrolledRule, math: &mut Expand) -> Self {
        let mut mathed = match &rule.kind {
            UnrolledRuleKind::PointEq(a, b) => Self::Eq(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::ScalarEq(a, b) => Self::Eq(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::Gt(a, b) => Self::Gt(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::Lt(a, b) => Self::Lt(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::Alternative(rules) => Self::Alternative(
                rules.iter().map(|x| Self::load(x, math)).collect()
            ),
            UnrolledRuleKind::Bias(_) => Self::Bias
        };

        mathed.normalize();

        if rule.inverted {
            Self::Invert(Box::new(mathed))
        } else {
            mathed
        }
    }
}

impl Normalize for Rule<()> {
    fn normalize(&mut self) {
        match self {
            | Self::NumberEq(a, b)
            | Self::Gt(a, b)
            | Self::Lt(a, b) => {
                if a > b {
                    mem::swap(a, b);
                }
            }
            Self::PointEq(a, b) => {
                if a > b {
                    mem::swap(a, b);
                }
            }
            Self::Alternative(v) => v.sort(),
            Self::Bias => (),
            Self::Invert(v) => v.normalize()
        }
    }
}

#[derive(Debug)]
pub struct Adjusted {
    pub template: Vec<AdjustableTemplate>,
    pub items: Vec<AnyExpr<()>>,
    pub rules: Vec<Rule<()>>
}

#[derive(Debug)]
pub struct Intermediate {
    pub figure: Figure,
    /// Ready for generation
    pub adjusted: Adjusted
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub expr: AnyExpr<()>,
    pub uses: usize
}

#[derive(Debug)]
pub enum Entity {
    FreePoint,
    PointOnLine(LineExpr<()>),
    FreeReal,
    Bind
}

#[derive(Debug, Clone, Copy)]
pub struct EntityId(usize);

impl HandleEntity for EntityId {
    fn contains_entity(&self, entity: usize) -> bool {
        entity == 
    }
}

#[derive(Debug, Clone, Default)]
pub struct Expand {
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, AnyExpr<()>>,
    /// All found entities
    pub entities: Vec<Entity>,
    /// Dst variable
    pub dst_var: OnceCell<NumberExpr<()>>
}

impl Expand {
    pub fn load<T: Displayed, U>(&mut self, expr: &Unrolled<T>) -> Expr<U, ()>
        where Any<()>: From<U>, Expr<U, ()>: FromUnrolled<T> {
        let key = (expr.data.as_ref() as *const _) as usize;
        let loaded = self.expr_map.get_mut(&key).cloned();

        if let Some(loaded) = loaded {
            loaded.try_into().unwrap()
        } else {
            // If expression has not been mathed yet, math it and put it into the record.
            let mut loaded = Expr::load(expr, self);
            loaded.normalize();
            self.expr_map.insert( key, loaded.clone().into_any());
            loaded
        }
    }

    #[must_use]
    pub fn get_dst_var(&mut self) -> NumberExpr<()> {
        self.dst_var.get_or_init(|| Expr::new(Number::Entity { id: self.add_real() })).clone()
    }

    fn add_entity(&mut self, entity: Entity) -> usize {
        self.entities.push(entity);
        self.entities.len() - 1
    }

    pub fn add_point(&mut self) -> usize {
        self.add_entity(Entity::FreePoint)
    }

    pub fn add_real(&mut self) -> usize {
        self.add_entity(Entity::FreeReal)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Build {
    expand: Expand,
    id_map: HashMap<usize, usize>,
    loaded: Vec<AnyExpr<()>>,
    items: Vec<Item>
}

impl Build {
    pub fn load<T: Displayed + LoadsTo>(&mut self, expr: &Unrolled<T>) -> usize {
        let key = (expr.data.as_ref() as *const _) as usize;
        *self.id_map.entry(key).or_insert_with(|| {
            self.loaded.push(self.expand.load(expr));
            self.loaded.len() - 1
        })
    }

    pub fn add<I: Into<Item>>(&mut self, item: I) {
        self.items.push(item.into());
    }
}

/// Tries to transform the rules so that they are simpler to process for the generator.
///
/// # Returns
/// `true` if an optimization was performed. `false` otherwise.
fn optimize_rules(rules: &mut Vec<Option<Rule<()>>>, entities: &mut [Entity]) -> bool {
    let mut performed = false;
    
    for rule in rules {
        performed = performed
            | ZeroLineDst::process(rule, entities);
    }

    if performed {
        rules.retain(Option::is_some);
    }

    performed
}

pub fn load_script(input: &str) -> Result<(Adjusted, Figure), Vec<Error>> {
    // Unroll script
    // Expand rules & figure maximally, normalize them
    // ---
    // Optimize rules and entities
    // Reduce entities
    // --- repeat
    // Turn entities into adjustables
    // Split rules & figure
    // Fold rules & figure separately
    // Assign reserved registers to figure expressions
    // Return

    // Unroll script
    let (mut unrolled, nodes) = unroll::unroll(input)?;

    // Expand & normalize figure
    let mut build = Build::default();
    nodes.build(&mut build);

    // Move expand base
    let mut expand = build.expand;

    // Expand & normalize rules
    let mut rules = Vec::new();

    for rule in unrolled.take_rules() {
        rules.push(Some(Rule::load(&rule, &mut expand)));
    }

    // The optimize cycle:
    // 1. Optimize
    // 2. Normalize
    // 3. Repeat if any optimizations applied.

    loop {
        if !optimize_rules(&mut rules, &mut expand.entities) {
            break;
        }

        for rule in rules.iter_mut().flatten() {
            rule.normalize();
        }
    }


    Ok((
        todo!()
    ))
}