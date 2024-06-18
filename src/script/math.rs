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
use std::collections::{hash_map, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::iter::Peekable;
use std::{cmp, mem};
use std::ops::{Deref, DerefMut};
use derive_recursive::Recursive;
use num_traits::{FromPrimitive, One, Zero};

use crate::script::figure::Item;
use crate::script::math::optimizations::ZeroLineDst;
use crate::script::token::number::{CompExponent, ProcNum};
use crate::script::unroll::figure::Node;

use self::optimizations::{EqExpressions, EqPointDst, RightAngle};

use super::unroll::Flag;
use super::{figure::Figure, unroll::{self, Displayed, Expr as Unrolled, UnrolledRule, UnrolledRuleKind,
                                     Point as UnrolledPoint, Line as UnrolledLine, Circle as UnrolledCircle, ScalarData as UnrolledScalar}, Error, ComplexUnit, SimpleUnit};

mod optimizations;

#[derive(Debug)]
pub struct Optimizations {}

#[derive(Debug)]
pub struct Flags {
    pub optimizations: Optimizations,
    pub point_bounds: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Self {
            optimizations: Optimizations {},
            point_bounds: false,
        }
    }
}

trait HandleEntity {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool;

    fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self;
}

fn map_entity_override(entity_id: EntityId, mapped: EntityId, into: &ExprKind) -> ExprKind {
    if entity_id == mapped {
        into.clone()
    } else {
        ExprKind::Entity { id: entity_id }
    }
}

impl HandleEntity for ProcNum {
    fn contains_entity(&self, _entity: EntityId, _entities: &[EntityKind]) -> bool {
        false
    }

    fn map_entity(self, _entity: EntityId, _into: &ExprKind) -> Self {
        self
    }
}

impl<T: HandleEntity> HandleEntity for Box<T> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
        self.as_ref().contains_entity(entity, entities)
    }

    fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self {
        Self::new((*self).map_entity(entity, into))
    }
}

impl<T: HandleEntity> HandleEntity for Vec<T> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
        self.iter().any(|item| item.contains_entity(entity, entities))
    }

    fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self {
        self.into_iter().map(|x| x.map_entity(entity, into)).collect()
    }
}

struct ReconstructCtx {

}

trait Reconstruct {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self;
}

trait FindEntities {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId>;
}

impl FindEntities for Vec<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        self.iter().map(|x| previous[x.0].iter().copied()).flatten().collect()
    }
}

trait FromUnrolled<T: Displayed> {
    fn load(expr: &Unrolled<T>, math: &mut Expand) -> Self;
}

trait Normalize {
    fn normalize(&mut self, math: &Expand);
}

#[derive(Debug, Clone, Copy, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct VarIndex(pub usize);

impl Reindex for VarIndex {
    fn reindex(&mut self, map: &IndexMap) {
        self.0 = map.get(self.0);
    }
}

impl Deref for VarIndex {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for VarIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum ExprType {
    Number,
    #[default]
    Point,
    Line,
    Circle
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive, Hash)]
#[recursive(
    impl HandleEntity for Self {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
            aggregate = ||
        }

        fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self {
            aggregate = {},
            override_marker = override_map
        }
    }
)]
#[recursive(
    impl Reindex for Self {
        fn reindex(&mut self, map: &IndexMap) {
            aggregate = _
        }
    }
)]
pub enum ExprKind {
    #[recursive(override_map = map_entity_override)]
    Entity {
        id: EntityId
    },

    // POINT

    /// k and l must be ordered
    LineLineIntersection {
        k: VarIndex,
        l: VarIndex
    },
    /// Items must be sorted
    AveragePoint {
        items: Vec<VarIndex>
    },
    CircleCenter {
        circle: VarIndex
    },

    // NUMBER

    /// plus and minus must be sorted and must not contain other sums. An aggregated constant, if any, must be at the end.
    Sum {
        plus: Vec<VarIndex>,
        minus: Vec<VarIndex>
    },
    /// times and by must be sorted and must not contain other sums. An aggregated constant, if any, must be at the end.
    Product {
        times: Vec<VarIndex>,
        by: Vec<VarIndex>
    },
    Const {
        value: ProcNum
    },
    Power {
        value: VarIndex,
        exponent: CompExponent
    },
    /// p and q must be ordered
    PointPointDistance {
        p: VarIndex,
        q: VarIndex
    },
    PointLineDistance {
        point: VarIndex,
        line: VarIndex
    },
    /// p and r must be ordered
    ThreePointAngle {
        p: VarIndex,
        q: VarIndex,
        r: VarIndex
    },
    /// p and r must be ordered
    ThreePointAngleDir {
        p: VarIndex,
        q: VarIndex,
        r: VarIndex
    },
    /// k and l must be ordered
    TwoLineAngle {
        k: VarIndex,
        l: VarIndex
    },
    PointX {
        point: VarIndex
    },
    PointY {
        point: VarIndex
    },

    // Line

    /// Normalized iff `p` and `q` are in ascending order
    PointPoint {
        p: VarIndex,
        q: VarIndex
    },
    /// Normalized iff `a` and `c` are in ascending order (`b` must stay in the middle)
    AngleBisector {
        p: VarIndex,
        q: VarIndex,
        r: VarIndex
    },
    /// Always normalized
    ParallelThrough {
        point: VarIndex,
        line: VarIndex
    },
    /// Always normalized
    PerpendicularThrough {
        point: VarIndex,
        line: VarIndex
    },

    // Circle

    ConstructCircle {
        center: VarIndex,
        radius: VarIndex
    }
}

impl FindEntities for ExprKind {
    // FIXME: DOESN'T WORK WITH FORWARD REFERENCING
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::Entity { id } => {
                set.insert(*id);
            }
            Self::AveragePoint { items } => {
                set.extend(items.iter().map(|x| previous[x.0].iter().copied()).flatten());
            }
            Self::CircleCenter { circle: x }
            | Self::PointX { point: x }
            | Self::PointY { point: x }
            | Self::Power { value: x, .. }=> {
                set.extend(previous[x.0].iter().copied());
            }
            Self::Sum { plus: v1, minus: v2 }
            | Self::Product { times: v1, by: v2 } => {
                set.extend(v1.iter().copied());
                set.extend(v2.iter().copied());
            }
            Self::PointPointDistance { p: a, q: b }
            | Self::PointLineDistance { point: a, line: b }
            | Self::TwoLineAngle { k: a, l: b }
            | Self::LineLineIntersection { k: a, l: b }
            | Self::ParallelThrough { point: a, line: b }
            | Self::PerpendicularThrough { point: a, line: b }
            | Self::PointPoint { p: a, q: b }
            | Self::ConstructCircle { center: a, radius: b } => {
                set.extend(previous[a.0].iter().copied());
                set.extend(previous[b.0].iter().copied());
            }
            Self::ThreePointAngle { p, q, r }
            | Self::ThreePointAngleDir { p, q, r }
            | Self::AngleBisector { p, q, r } => {
                set.extend(previous[p.0].iter().copied());
                set.extend(previous[q.0].iter().copied());
                set.extend(previous[r.0].iter().copied());
            }
            Self::Const { .. } => {}
        }

        set
    }
}

impl Default for ExprKind {
    fn default() -> Self {
        Self::Entity { id: EntityId(0) }
    }
}

impl FromUnrolled<UnrolledPoint> for ExprKind {
    fn load(expr: &Unrolled<UnrolledPoint>, math: &mut Expand) -> Self {
        let mut kind = match expr.get_data() {
            UnrolledPoint::LineLineIntersection(a, b) => ExprKind::LineLineIntersection {
                k: math.load(a),
                l: math.load(b)
            },
            UnrolledPoint::Average(exprs) => ExprKind::AveragePoint {
                items: exprs.iter().map(|x| math.load(x)).collect()
            },
            UnrolledPoint::CircleCenter(circle) => {
                match circle.get_data() {
                    UnrolledCircle::Circle(center, _) => return Self::load(center, math),
                    _ => unreachable!()
                }
            },
            UnrolledPoint::Free => ExprKind::Entity { id: math.add_point() },
            _ => unreachable!()
        };

        kind.normalize(math);
        kind
    }
}

impl FromUnrolled<unroll::Scalar> for ExprKind {
    fn load(expr: &Unrolled<unroll::Scalar>, math: &mut Expand) -> Self {
        let mut kind = match expr.get_data() {
            UnrolledScalar::Add(a, b) => ExprKind::Sum {
                plus: vec![math.load(a), math.load(b)],
                minus: Vec::new()
            },
            UnrolledScalar::Subtract(a, b) => ExprKind::Sum {
                plus: vec![math.load(a)],
                minus: vec![math.load(b)]
            },
            UnrolledScalar::Multiply(a, b) => ExprKind::Product {
                times: vec![math.load(a), math.load(b)],
                by: Vec::new()
            },
            UnrolledScalar::Divide(a, b) => ExprKind::Product {
                times: vec![math.load(a)],
                by: vec![math.load(b)]
            },
            UnrolledScalar::Average(exprs) => {
                let times = ExprKind::Sum {
                    plus: exprs.iter().map(|x| math.load(x)).collect(),
                    minus: Vec::new(),
                };
                let by = ExprKind::Const { value: ProcNum::from_usize(exprs.len()).unwrap() };

                ExprKind::Product {
                    times: vec![math.store(times)],
                    by: vec![math.store(by)],
                }
            },
            UnrolledScalar::CircleRadius(circle) => {
                match circle.get_data() {
                    UnrolledCircle::Circle(_, radius) => return math.load(radius),
                    _ => unreachable!()
                }
            }
            UnrolledScalar::Free => ExprKind::Entity { id: math.add_real() },
            UnrolledScalar::Number(x) => return fix_dst(Expr::new(
                ExprKind::Const { value: x.clone() },
                ExprType::Number
            ), expr.data.unit, math),
            UnrolledScalar::DstLiteral(x) => ExprKind::Const { value: x.clone() },
            UnrolledScalar::SetUnit(x, unit) => return fix_dst(math.load(x), Some(*unit), math),
            UnrolledScalar::PointPointDistance(p, q) => ExprKind::PointPointDistance {
                p: math.load(p),
                q: math.load(q)
            },
            UnrolledScalar::PointLineDistance(point, line) => ExprKind::PointLineDistance {
                point: math.load(point),
                line: math.load(line)
            },
            UnrolledScalar::Negate(x) => ExprKind::Sum {
                plus: Vec::new(),
                minus: vec![math.load(x)]
            },
            UnrolledScalar::ThreePointAngle(p, q, r) => ExprKind::ThreePointAngle {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r)
            },
            UnrolledScalar::ThreePointAngleDir(p, q, r) => ExprKind::ThreePointAngleDir {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r)
            },
            UnrolledScalar::TwoLineAngle(k, l) => ExprKind::TwoLineAngle {
                k: math.load(k),
                l: math.load(l)
            },
            UnrolledScalar::Pow(base, exponent) => ExprKind::Power {
                value: math.load(base),
                exponent: exponent.clone()
            },
            UnrolledScalar::PointX(point) => ExprKind::PointX {
                point: math.load(point)
            },
            UnrolledScalar::PointY(point) => ExprKind::PointY {
                point: math.load(point)
            },
            _ => unreachable!()
        };

        kind.normalize(math);
        kind
    }
}

impl FromUnrolled<UnrolledLine> for ExprKind {
    fn load(expr: &Unrolled<UnrolledLine>, math: &mut Expand) -> Self {
        let mut kind = match expr.get_data() {
            UnrolledLine::LineFromPoints(a, b) => Self::PointPoint {
                p: math.load(a),
                q: math.load(b)
            },
            UnrolledLine::AngleBisector(a, b, c) => Self::AngleBisector {
                p: math.load(a),
                q: math.load(b),
                r: math.load(c),
            },
            UnrolledLine::PerpendicularThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => {
                        Self::ParallelThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    UnrolledLine::ParallelThrough(l, _) => {
                        Self::PerpendicularThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    _ => Self::PerpendicularThrough {
                        point: math.load(p),
                        line: math.load(k)
                    }
                }
            }
            UnrolledLine::ParallelThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => {
                        Self::PerpendicularThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    UnrolledLine::ParallelThrough(l, _) => {
                        Self::ParallelThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    _ => Self::ParallelThrough {
                        point: math.load(p),
                        line: math.load(k)
                    }
                }
            },
            _ => unreachable!()
        };

        kind.normalize(math);
        kind
    }
}

impl Normalize for ExprKind {
    fn normalize(&mut self, math: &Expand) {
        let cmp_and_swap = |a: &mut VarIndex, b: &mut VarIndex| {
            if math.at(*a).kind > math.at(*b) {
                mem::swap(a, b);
            }
        };
        let cmp = |a: &VarIndex, b: &VarIndex| math.at(*a).kind.cmp(&math.at(*b).kind);
        let mut new_self = None;

        match self {
            Self::CircleCenter { .. }
            | Self::PointLineDistance { .. }
            | Self::PointX { .. }
            | Self::PointY { .. }
            | Self::Power { .. }
            | Self::ConstructCircle { .. }
            | Self::Const { .. }
            | Self::Entity { .. } => (),
            Self::LineLineIntersection { k: a, l: b }
            | Self::PointPoint { p: a, q: b }
            | Self::TwoLineAngle { k: a, l: b }
            | Self::AngleBisector { p: a, r: b, .. }
            | Self::ThreePointAngle { p: a, r: b, .. }
            | Self::ThreePointAngleDir { p: a, r: b, .. }
            | Self::PointPointDistance {p: a, q: b} => {
                cmp_and_swap(a, b)
            }
            Self::AveragePoint { items } => {
                items.sort_by(&cmp);
            }
            Self::Sum { plus, minus } => {
                normalize_sum(plus, minus, math);
            }
            Self::Product { times, by } => {
                normalize_product(times, by, math);
            }
            Self::ParallelThrough { point, line } => {
                new_self = Some(match &math.at(*line).kind {
                    Self::ParallelThrough { line, .. } => Self::ParallelThrough { point: *point, line: *line },
                    Self::PerpendicularThrough { line, .. } => Self::PerpendicularThrough { point: *point, line: *line },
                    _ => Self::ParallelThrough { point: *point, line: *line }
                });
            }
            Self::PerpendicularThrough { mut point, mut line } => {
                new_self = Some(match &math.at(*line).kind {
                    Self::ParallelThrough { line, .. } => Self::PerpendicularThrough { point: *point, line: *line },
                    Self::PerpendicularThrough { line, .. } => Self::ParallelThrough { point: *point, line: *line },
                    _ => Self::PerpendicularThrough { point: *point, line: *line }
                });
            }
        }
    }
}

fn fix_dst(expr: ExprKind, unit: Option<ComplexUnit>, math: &mut Expand) -> ExprKind {
    match unit {
        None => expr,
        Some(unit) => {
            if unit.0[SimpleUnit::Distance as usize].is_zero() {
                expr
            } else {
                ExprKind::Product {
                    times: vec![math.store(expr), math.store(ExprKind::Power {
                        value: math.get_dst_var(),
                        exponent: unit.0[SimpleUnit::Distance as usize]
                    })],
                    by: Vec::new()
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Merge<T, I, J, F>
    where I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> cmp::Ordering {
    i: Peekable<I>,
    j: Peekable<J>,
    f: F
}

impl<T, I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> cmp::Ordering> Merge<T, I, J, F> {
    #[must_use]
    pub fn new<A: IntoIterator<IntoIter = I>, B: IntoIterator<IntoIter = J>>(a: A, b: B, f: F) -> Self {
        Self {
            i: a.into_iter().peekable(),
            j: b.into_iter().peekable(),
            f
        }
    }

    #[must_use]
    pub fn empty(f: F) -> Self {
        Self::new(None, None, f)
    }

    #[must_use]
    pub fn merge_with<It: IntoIterator<Item = T>>(self, other: It) -> Merge<T, Self, It::IntoIter, F> where F: Clone {
        let f_cloned = self.f.clone();
        Merge::new(self, other, f_cloned)
    }
}

impl<T, I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> cmp::Ordering> Iterator for Merge<T, I, J, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(i_item) = self.i.peek() {
            if let Some(j_item) = self.j.peek() {
                if self.f(i_item, j_item) == cmp::Ordering::Less {
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

fn normalize_sum(plus: &mut Vec<VarIndex>, minus: &mut Vec<VarIndex>, math: &mut Expand) {
    let plus_v = mem::take(plus);
    let minus_v = mem::take(minus);

    let mut constant = ProcNum::zero();

    let mut plus_final = Vec::new();
    let mut minus_final = Vec::new();

    let cmp = |a: &VarIndex, b: &VarIndex| {
        math.at(*a).kind.cmp(&math.at(*b).kind)
    };

    for item in plus_v {
        match &math.at(item).kind {
            ExprKind::Sum {
                plus, minus
            } => {
                plus_final = Merge::new(plus_final, plus.iter().copied(), &cmp).collect();
                minus_final = Merge::new(minus_final, minus.iter().copied(), &cmp).collect();
            }
            ExprKind::Const { value } => constant += value,
            _ => {
                plus_final = Merge::new(plus_final, Some(item), &cmp).collect();
            }
        }
    }

    for item in minus_v {
        match &math.at(item).kind {
            ExprKind::Sum {
                plus, minus
            } => {
                plus_final = Merge::new(plus_final, minus, &cmp).collect();
                minus_final = Merge::new(minus_final, plus, &cmp).collect();
            }
            ExprKind::Const { value } => constant -= value,
            _ => {
                minus_final = Merge::new(minus_final, Some(item), &cmp).collect();
            }
        }
    }

    if !constant.is_zero() {
        plus_final.push(math.store(ExprKind::Const { value: constant }));
    }

    *plus = plus_final;
    *minus = minus_final;
}

fn normalize_product(times: &mut Vec<VarIndex>, by: &mut Vec<VarIndex>, math: &mut Expand) {
    let times_v = mem::take(times);
    let by_v = mem::take(by);

    let mut constant = ProcNum::one();

    let mut times_final = Vec::new();
    let mut by_final = Vec::new();

    let cmp = |a: &VarIndex, b: &VarIndex| {
        math.at(*a).kind.cmp(&math.at(*b).kind)
    };

    for item in times_v {
        match &math.at(item).kind {
            ExprKind::Product {
                times, by
            } => {
                times_final = Merge::new(times_final, times, &cmp).collect();
                by_final = Merge::new(by_final, by, &cmp).collect();
            }
            ExprKind::Const { value } => constant *= value,
            _ => {
                times_final = Merge::new(times_final, Some(item), &cmp).collect();
            }
        }
    }

    for item in by_v {
        match &math.at(item).kind {
            ExprKind::Product {
                times, by
            } => {
                times_final = Merge::new(times_final, by, &cmp).collect();
                by_final = Merge::new(by_final, times, &cmp).collect();
            }
            ExprKind::Const { value } => constant /= value,
            _ => {
                by_final = Merge::new(by_final, Some(item), &cmp).collect();
            }
        }
    }

    if !constant.is_one() {
        times_final.push(math.store(ExprKind::Const { value: constant }));
    }

    *times = times_final;
    *by = by_final;
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Expr<M> {
    pub meta: M,
    pub kind: ExprKind,
    pub ty: ExprType
}

pub type MathMeta = u64;
pub type MathExpr = Expr<MathMeta>;

impl<M> FindEntities for Expr<M> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        self.kind.find_entities(previous)
    }
}

impl<M> Reindex for Expr<M> {
    fn reindex(&mut self, map: &IndexMap) {
        self.kind.reindex(map);
    }
}

impl<M> HandleEntity for Expr<M> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
        self.kind.contains_entity(entity, entities)
    }

    fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self {
        Self {
            kind: self.kind.map_entity(entity, into),
            ..self
        }
    }
}

impl<M> Normalize for Expr<M> {
    fn normalize(&mut self, math: &Expand) {
        self.kind.normalize(math);
    }
}

impl Expr<MathMeta> {
    #[must_use]
    pub const fn new(kind: ExprKind, ty: ExprType) -> Self {
        let mut hasher = DefaultHasher::new();
        kind.hash(&mut hasher);

        Self {
            kind,
            meta: hasher.finish(),
            ty
        }
    }
}

/// Represents a rule of the figure.
/// Rules are normalized iff:
/// * their operands are normalized
/// * their operands are sorted in ascending order
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive)]
#[recursive(
    impl HandleEntity for Self {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
            aggregate = ||,
            init = false
        }

        fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self {
            aggregate = {}
        }
    }
)]
pub enum RuleKind {
    PointEq(VarIndex, VarIndex),
    NumberEq(VarIndex, VarIndex),
    Lt(VarIndex, VarIndex),
    Gt(VarIndex, VarIndex),
    Alternative(Vec<RuleKind>),
    Invert(Box<RuleKind>),
    Bias
}

impl FindEntities for RuleKind {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::PointEq(a, b)
            | Self::NumberEq(a, b)
            | Self::Lt(a, b)
            | Self::Gt(a, b) => {
                set.extend(previous[a.0].iter().copied());
                set.extend(previous[b.0].iter().copied());
            },
            Self::Alternative(items) => {
                return items.iter().map(|x| x.find_entities(previous).into_iter()).flatten().collect();
            }
            Self::Invert(rule) => {
                return rule.find_entities(previous);
            }
            Self::Bias => unreachable!()
        }

        set
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rule {
    pub kind: RuleKind,
    pub weight: ProcNum,
    pub entities: Vec<EntityId>
}

impl HandleEntity for Rule {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
        self.kind.contains_entity(entity, entities)
    }

    fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self {
        Self {
            kind: self.kind.map_entity(entity, into),
            ..self
        }
    }
}

impl Reindex for RuleKind {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Self::PointEq(a, b)
            | Self::NumberEq(a, b)
            | Self::Lt(a, b)
            | Self::Gt(a, b) => {
                a.reindex(map);
                b.reindex(map);
            }
            Self::Alternative(items) => {
                items.reindex(map);
            }
            Self::Invert(rule) => rule.reindex(map),
            Self::Bias => {}
        }
    }
}

impl Reindex for Rule {
    fn reindex(&mut self, map: &IndexMap) {
        self.kind.reindex(map);
    }
}

impl RuleKind {
    /// # Returns
    /// A normalized rule.
    fn load(rule: &UnrolledRule, math: &mut Expand) -> Self {
        let mut mathed = match &rule.kind {
            UnrolledRuleKind::PointEq(a, b) => Self::PointEq(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::ScalarEq(a, b) => Self::ScalarEq(
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

        mathed.normalize(math);

        if rule.inverted {
            Self::Invert(Box::new(mathed))
        } else {
            mathed
        }
    }
}

impl Rule {
    /// # Returns
    /// A normalized rule.
    fn load(rule: &UnrolledRule, math: &mut Expand) -> Self {
        Self {
            kind: RuleKind::load(rule, math),
            weight: rule.weight.clone(),
            entities: Vec::new()
        }
    }
}

impl Normalize for RuleKind {
    fn normalize(&mut self, math: &Expand) {
        match self {
            | Self::PointEq(a, b)
            | Self::NumberEq(a, b)
            | Self::Gt(a, b)
            | Self::Lt(a, b) => {
                if math.at(*a).kind > math.at(*b).kind {
                    mem::swap(a, b);
                }
            }
            Self::Alternative(v) => v.sort(),
            Self::Invert(_) | Self::Bias => ()
        }
    }
}

impl Normalize for Rule {
    fn normalize(&mut self, math: &Expand) {
        self.kind.normalize(math);
    }
}

#[derive(Debug)]
pub struct Adjusted {
    pub variables: Vec<Expr<()>>,
    pub rules: Vec<Rule>,
    pub entities: Vec<Entity<()>>
}

#[derive(Debug)]
pub struct Intermediate {
    pub figure: Figure,
    /// Ready for generation
    pub adjusted: Adjusted,
    pub flags: Flags
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub expr: Expr<()>,
    pub uses: usize
}

#[derive(Debug, Clone)]
pub struct Entity<M> {
    pub kind: EntityKind,
    pub meta: M
}

#[derive(Debug, Clone, Recursive)]
#[recursive(
    impl HandleEntity for Self {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
            aggregate = ||,
            init = false
        }

        fn map_entity(self, entity: EntityId, into: &ExprKind) -> Self {
            aggregate = {}
        }
    }
)]
pub enum EntityKind {
    FreePoint,
    PointOnLine {
        line: VarIndex
    },
    PointOnCircle {
        circle: VarIndex
    },
    FreeReal,
    Bind(VarIndex)
}

impl Reindex for EntityKind {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            EntityKind::FreePoint
            | EntityKind::FreeReal => {}
            EntityKind::PointOnLine { line } => line.reindex(map),
            EntityKind::PointOnCircle { circle } => circle.reindex(map),
            EntityKind::Bind(_) => unreachable!("Should not appear")
        }
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct EntityId(pub usize);

impl HandleEntity for EntityId {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind]) -> bool {
        *self == entity || entities[self.0].contains_entity(entity, entities)
    }

    fn map_entity(self, _: EntityId, _: &ExprKind) -> Self {
        unreachable!("map_entity should never be called for EntityId")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Expand {
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, VarIndex>,
    /// All found entities
    pub entities: Vec<EntityKind>,
    /// Dst variable
    pub dst_var: OnceCell<EntityId>,
    /// Collected expressions
    pub expr_record: Vec<MathExpr>
}

impl Expand {
    pub fn load<T: Displayed>(&mut self, expr: &Unrolled<T>) -> VarIndex {
        let key = (expr.data.as_ref() as *const _) as usize;
        let loaded = self.expr_map.get_mut(&key).cloned();

        if let Some(loaded) = loaded {
            let expr = self.expr_record[loaded].deep_clone(self);
            self.expr_record.push(expr);
            VarIndex(self.expr_record.len() - 1)
        } else {
            // If expression has not been mathed yet, math it and put it into the record.
            let mut loaded = Expr::load(expr, self);
            loaded.normalize(self);
            self.expr_record.push(loaded);
            let index = VarIndex(self.expr_record.len() - 1);
            self.expr_map.insert(key, index);
            index
        }
    }

    #[must_use]
    pub fn get_dst_var(&mut self) -> VarIndex {
        let id = *self.dst_var.get_or_init(|| self.add_real());
        self.store(ExprKind::Entity { id })
    }

    fn add_entity(&mut self, entity: EntityKind) -> EntityId {
        self.entities.push(entity);
        EntityId(self.entities.len() - 1)
    }

    pub fn add_point(&mut self) -> EntityId {
        self.add_entity(EntityKind::FreePoint)
    }

    pub fn add_real(&mut self) -> EntityId {
        self.add_entity(EntityKind::FreeReal)
    }
}

/// Used explicitly for figure building.
#[derive(Debug, Clone, Default)]
pub struct Build {
    expand: Expand,
    id_map: HashMap<usize, usize>,
    loaded: Vec<VarIndex>,
    items: Vec<Item>
}

impl Build {
    /// If the expression has been loaded through this function before,
    /// the cached index will be returned. Used ONLY for figure.
    pub fn load<T: Displayed>(&mut self, expr: &Unrolled<T>) -> usize {
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
fn optimize_rules(rules: &mut Vec<Option<Rule>>, entities: &mut [EntityKind]) -> bool {
    let mut performed = false;
    
    for rule in rules {
        performed = performed
            | ZeroLineDst::process(rule, entities)
            | RightAngle::process(rule)
            | EqPointDst::process(rule, entities)
            | EqExpressions::process(rule)
            ;
    }

    if performed {
        rules.retain(Option::is_some);
    }

    performed
}

/// Constructs a map between two sets of numbers A -> B.
#[derive(Debug, Clone, Default)]
pub struct IndexMap {
    /// The main map
    a_to_b: HashMap<usize, usize>,
    /// An inverse map
    b_to_a: HashMap<usize, usize>
}

impl IndexMap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            a_to_b: HashMap::new(),
            b_to_a: HashMap::new()
        }
    }

    pub fn get(&self, a: usize) -> usize {
        self.a_to_b.get(&a).copied().unwrap_or(a)
    }

    /// Creates a mapping from a to b (works like function composition).
    pub fn map(&mut self, a: usize, b: usize) {
        // Before everything, if a == b, nothing happens.
        if a == b {
            return;
        }

        // If a is present in b->a, that means there exists a direct mapping a->b which we should override and remove the b->a map.
        // Otherwise, if a is not present in a->b, we should add it along with a b->a mapping.
        // If a is present in a->b but not present in b->a, a is not reachable, thus nothing should happen.
        let a_key = self.b_to_a.get(&a).copied();
        if let Some(a_key) = a_key {
            // There's a mapping a_key -> a
            // Remove the inverse mapping a -> a_key
            self.b_to_a.remove(&a);

            if a_key == b {
                // Also remove a -> b
                self.a_to_b.remove(&a_key);
            } else {
                // Make the mapping a_key -> b
                self.a_to_b.get_mut(&a_key).map(|current| *current = b);
                // Insert the new mapping b -> a_key
                self.b_to_a.insert(b, a_key);
            }
        } else if !self.a_to_b.contains_key(&a){
            // There is no a -> ?.
            self.a_to_b.insert(a, b);
            self.b_to_a.insert(b, a);
        }
    }

    /// Composes two index maps: any get call will be functionally equivalent to self.get(other.get(i)).
    pub fn compose(lhs: Self, rhs: &mut Self) {
        for (a, b) in lhs.a_to_b {
            rhs.map(a, b);
        }
    }
}

pub trait Reindex {
    fn reindex(&mut self, map: &IndexMap);
}

impl<T: Reindex> Reindex for Box<T> {
    fn reindex(&mut self, map: &IndexMap) {
        self.as_mut().reindex(map);
    }
}

impl<T: Reindex> Reindex for Vec<T> {
    fn reindex(&mut self, map: &IndexMap) {
        for item in self {
            item.reindex(map);
        }
    }
}

/// Folds the given matrix:
///     - We reindex the element
///     - We put it into a hashmap
///     - If the element repeats, we replace it and add an entry into the index map
///     - Otherwise, we rejoice
/// Returns the index map created by the folding.
fn fold(matrix: &mut Vec<Expr<MathMeta>>) -> IndexMap {
    let mut target = Vec::new();
    let mut final_map = IndexMap::new();
    let mut record = HashMap::new();
    target.resize_with(matrix.len(), Expr::default);

    loop {
        let mut map = IndexMap::new();
        let mut folded = false;
        for (i, expr) in matrix.iter_mut().enumerate() {
            let mut expr = mem::take(expr);
            expr.reindex(&map);
            match record.entry(expr) {
                hash_map::Entry::Vacant(entry) => {
                    target.push(entry.key().clone());
                    let new_i = target.len() - 1;
                    map.map(i, new_i);
                    entry.insert(new_i);
                },
                hash_map::Entry::Occupied(entry) => {
                    // We have to update the index map. No push into target happens.
                    map.map(i, *entry.get());
                    folded = true;
                }
            }
        }

        // We have to also build the final map.
        IndexMap::compose(map, &mut final_map);

        // Swap target with matrix before next loop.
        mem::swap(matrix, &mut target);

        // And truncate target to match matrix.
        target.truncate(matrix.len());

        if !folded {
            break final_map;
        }
    }
}

fn read_flags(flags: &HashMap<String, Flag>) -> Flags {
    Flags {
        optimizations: Optimizations {},
        point_bounds: flags["point_bounds"].as_bool().unwrap(),
    }
}

pub fn load_script(input: &str) -> Result<Intermediate, Vec<Error>> {
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

        // Create the entity reduction map:
        // If an entity is a bind, it should be turned into an expression
        // Remaining entities should be appropriately offset.

        let mut intos = Vec::new() ;
        let mut offset = 0;

        // We'll need mutable access to expand in the loop, so we take the entities out of it.
        let entities = mem::take(&mut expand.entities);

        for (i, entity) in entities.iter().enumerate() {
            intos.push(match entity {
                EntityKind::FreePoint
                | EntityKind::PointOnCircle(_)
                | EntityKind::PointOnLine(_) => expand.store(ExprKind::Entity { id: EntityId(i - offset) }),
                EntityKind::FreeReal => expand.store(ExprKind::Entity { id: EntityId(i - offset) }),
                EntityKind::Bind(expr) => {
                    offset += 1;
                    *expr
                }
            });
        }

        // Put the entities back in
        expand.entities = entities;
        expand.entities.retain(|x| !matches!(x, EntityKind::Bind(_)));

        // The entities are now corrected, but the rules, entities (sic) and variables don't know that.
        // The easiest way to fix it is to reconstruct the loaded variable vector recursively.
        // This way, we can also fix forward referencing and remove potentially unused expressions.
        // We'll also have to update items. Otherwise, they will have misguided indices.

        let mut ctx = ReconstructCtx::new(intos);
        expand.entities = expand.entities.reconstruct(&mut ctx);
        build.items = build.items.reconstruct(&mut ctx);
        rules = rules.reconstruct(&mut ctx);
        expand.expr_record = ctx.expr_record;

        // Normalize all rules now
        for rule in rules.iter_mut().flatten() {
            rule.normalize(&mut expand);
        }
    }

    // After all optimizations are done, normalize the loaded expressions
    for loaded in &mut build.loaded {
        loaded.normalize();
    }

    // We can also finalize rules:
    let mut rules: Vec<_> = rules.into_iter().flatten().collect();

    // THE FOLDING STEP
    // We perform similar expression elimination multiple times
    //      - We reindex the element
    //      - We put it into a hashmap
    //      - If the element repeats, we replace it and add an entry into the index map
    //      - Otherwise, we rejoice
    // When the process ends NO REORGANIZATION HAPPENS AS NONE IS NECESSARY
    // Rules and entities are updated using the obtained index map.

    let mut entities = expand.entities;
    let mut variables = expand.expr_record;
    let mut fig_variables = variables.clone();
    let adj_entities = entities.clone();

    let index_map = fold(&mut variables);
    entities.reindex(&index_map);
    rules.reindex(&index_map);

    // Find entities affected by specific rules
    let mut found_entities = Vec::new();
    for expr in &variables {
        let found = expr.find_entities(&found_entities);
        found_entities.push(found);
    }

    for rule in &mut rules {
        let entities = rule.kind.find_entities(&found_entities);
        rule.entities = entities.into_iter().collect();
    }

    let mut counter = 0..;

    // Give metas to entities.
    let entities = entities
        .into_iter()
        .map(|ent| Entity {
            kind: ent,
            meta: counter.next().unwrap()
        })
        .collect();

    // Fold figure variables
    let index_map = fold(&mut fig_variables);
    let mut items = build.items;
    items.reindex(&index_map);

    let fig_variables = fig_variables
        .into_iter()
        .map(|expr| Expr {
            kind: expr.kind,
            meta: counter.next().unwrap(),
            ty: expr.ty
        })
        .collect();

    Ok(Intermediate {
        adjusted: Adjusted {
            variables,
            rules,
            entities: adj_entities
        },
        figure: Figure {
            entities,
            variables: fig_variables,
            items
        },
        flags: read_flags(&unrolled.flags)
    })
}