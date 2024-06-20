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
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::iter::Peekable;
use std::mem;
use std::cmp::Ordering;
use std::ops::{Deref, DerefMut};
use derive_recursive::Recursive;
use num_traits::{FromPrimitive, One, Zero};
use serde::Serialize;

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

pub trait GetMathType {
    #[must_use]
    fn get_math_type() -> ExprType;
}

impl GetMathType for UnrolledPoint {
    fn get_math_type() -> ExprType {
        ExprType::Point
    }
}

impl GetMathType for UnrolledLine {
    fn get_math_type() -> ExprType {
        ExprType::Line
    }
}

impl GetMathType for UnrolledCircle {
    fn get_math_type() -> ExprType {
        ExprType::Circle
    }
}

impl GetMathType for unroll::Scalar {
    fn get_math_type() -> ExprType {
        ExprType::Number
    }
}

pub trait DeepClone {
    #[must_use]
    fn deep_clone(&self, math: &mut Math) -> Self;
}

impl DeepClone for ProcNum {
    fn deep_clone(&self, _math: &mut Math) -> Self {
        self.clone()
    }
}

impl DeepClone for CompExponent {
    fn deep_clone(&self, _math: &mut Math) -> Self {
        *self
    }
}

impl<T: DeepClone> DeepClone for Vec<T> {
    fn deep_clone(&self, math: &mut Math) -> Self {
        self.iter().map(|x| x.deep_clone(math)).collect()
    }
}

trait Compare {
    #[must_use]
    fn compare(&self, other: &Self, math: &Math) -> Ordering;
}

impl<T: Compare> Compare for Vec<T> {
    fn compare(&self, other: &Self, math: &Math) -> Ordering {
        self.iter()
            .zip(other)
            .map(|(a, b)| a.compare(b, math))
            .find(|x| x.is_ne())
            .unwrap_or_else(|| self.len().cmp(&other.len()))
    }
}

trait ContainsEntity {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool;
}

impl ContainsEntity for CompExponent {
    fn contains_entity(&self, _entity: EntityId, _math: &Math) -> bool {
        false
    }
}

impl ContainsEntity for ProcNum {
    fn contains_entity(&self, _entity: EntityId, _math: &Math) -> bool {
        false
    }
}

impl<T: ContainsEntity> ContainsEntity for Box<T> {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
        self.as_ref().contains_entity(entity, math)
    }
}

impl<T: ContainsEntity> ContainsEntity for Vec<T> {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
        self.iter().any(|item| item.contains_entity(entity, math))
    }
}

pub struct ReconstructCtx<'r> {
    entity_replacement: &'r [VarIndex],
    old_vars: &'r [Expr<()>],
    new_vars: Vec<Expr<()>>
}

impl<'r> ReconstructCtx<'r> {
    #[must_use]
    fn new(entity_replacement: &'r [VarIndex], old_vars: &'r [Expr<()>]) -> Self {
        Self {
            entity_replacement,
            old_vars,
            new_vars: Vec::new()
        }
    }
}

pub trait Reconstruct {
    #[must_use]
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self;
}

fn reconstruct_entity(entity_id: EntityId, ctx: &mut ReconstructCtx) -> ExprKind {
    ctx.old_vars[ctx.entity_replacement[entity_id.0].0].kind.clone().reconstruct(ctx)
}

impl Reconstruct for ProcNum {
    fn reconstruct(self, _ctx: &mut ReconstructCtx) -> Self {
        self
    }
}

impl Reconstruct for CompExponent {
    fn reconstruct(self, _ctx: &mut ReconstructCtx) -> Self {
        self
    }
}

impl<T: Reconstruct> Reconstruct for Option<T> {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        self.map(|v| v.reconstruct(ctx))
    }
}

impl<T: Reconstruct> Reconstruct for Box<T> {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        Self::new((*self).reconstruct(ctx))
    }
}

impl<T: Reconstruct> Reconstruct for Vec<T> {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        self.into_iter().map(|x| x.reconstruct(ctx)).collect()
    }
}

trait FindEntities {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId>;
}

impl FindEntities for Vec<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        self.iter().flat_map(|x| previous[x.0].iter().copied()).collect()
    }
}

pub trait FromUnrolled<T: Displayed> {
    fn load(expr: &Unrolled<T>, math: &mut Expand) -> Self;
}

trait Normalize {
    fn normalize(&mut self, math: &mut Math);
}

#[derive(Debug, Clone, Copy, Hash, Ord, PartialOrd, Eq, PartialEq, Serialize)]
pub struct VarIndex(pub usize);

impl Display for VarIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Reconstruct for VarIndex {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        let expr = ctx.old_vars[self.0].clone();
        let kind = expr.kind.reconstruct(ctx);
        ctx.new_vars.push(Expr::new(kind, expr.ty));
        VarIndex(ctx.new_vars.len() - 1)
    }
}

impl DeepClone for VarIndex {
    fn deep_clone(&self, math: &mut Math) -> Self {
        // The clone here is necessary to satisfy the borrow checker.
        // Looks ugly. but otherwise, we'd borrow `math` both mutably and immutably.
        let ty = math.at(*self).ty;
        let expr = math.at(*self).kind.clone().deep_clone(math);
        math.store(expr, ty)
    }
}

impl Compare for VarIndex {
    fn compare(&self, other: &Self, math: &Math) -> Ordering {
        math.at(*self).kind.compare(&math.at(*other).kind, math)
    }
}

impl Reindex for VarIndex {
    fn reindex(&mut self, map: &IndexMap) {
        self.0 = map.get(self.0);
    }
}

impl ContainsEntity for VarIndex {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
        math.at(*self).contains_entity(entity, math)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash, Serialize)]
pub enum ExprType {
    Number,
    #[default]
    Point,
    Line,
    Circle
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive, Hash, Serialize)]
#[recursive(
    impl ContainsEntity for Self {
        fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
            aggregate = ||
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
#[recursive(
    impl DeepClone for Self {
        fn deep_clone(&self, math: &mut Math) -> Self {
            aggregate = {}
        }
    }
)]
#[recursive(
    impl Reconstruct for Self {
        fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
            aggregate = {},
            override_marker = override_reconstruct
        }
    }
)]
pub enum ExprKind {
    #[recursive(override_reconstruct = reconstruct_entity)]
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

impl ExprKind {
    #[must_use]
    pub fn variant_id(&self) -> usize {
        match self {
            Self::Entity { .. } => 0,
            Self::LineLineIntersection { .. } => 1,
            Self::AveragePoint { .. } => 2,
            Self::CircleCenter { .. } => 3,
            Self::Sum { .. } => 4,
            Self::Product { .. } => 5,
            Self::Const { .. } => 6,
            Self::Power { .. } => 7,
            Self::PointPointDistance { .. } => 8,
            Self::PointLineDistance { .. } => 9,
            Self::ThreePointAngle { .. } => 10,
            Self::ThreePointAngleDir { .. } => 11,
            Self::TwoLineAngle { .. } => 12,
            Self::PointX { .. } => 13,
            Self::PointY { .. } => 14,
            Self::PointPoint { .. } => 15,
            Self::AngleBisector { .. } => 16,
            Self::ParallelThrough { .. } => 17,
            Self::PerpendicularThrough { .. } => 18,
            Self::ConstructCircle { .. } => 19,
        }
    }

    #[must_use]
    pub fn compare(&self, other: &Self, math: &Math) -> Ordering {
        self.variant_id()
            .cmp(&other.variant_id())
            .then_with(|| match (self, other) {
                (
                    Self::Entity { id: self_id }, Self::Entity { id: other_id }
                ) => self_id.cmp(other_id),
                (
                    Self::LineLineIntersection { k: self_a, l: self_b },
                    Self::LineLineIntersection { k: other_a, l: other_b },
                ) | (
                    Self::PointPointDistance { p: self_a, q: self_b },
                    Self::PointPointDistance { p: other_a, q: other_b },
                ) | (
                    Self::PointLineDistance { point: self_a, line: self_b },
                    Self::PointLineDistance { point: other_a, line: other_b },
                ) | (
                    Self::TwoLineAngle { k: self_a, l: self_b },
                    Self::TwoLineAngle { k: other_a, l: other_b },
                ) | (
                    Self::PointPoint { p: self_a, q: self_b },
                    Self::PointPoint { p: other_a, q: other_b },
                ) | (
                    Self::ParallelThrough { point: self_a, line: self_b },
                    Self::ParallelThrough { point: other_a, line: other_b },
                ) | (
                    Self::PerpendicularThrough { point: self_a, line: self_b },
                    Self::PerpendicularThrough { point: other_a, line: other_b },
                ) | (
                    Self::ConstructCircle { center: self_a, radius: self_b },
                    Self::ConstructCircle { center: other_a, radius: other_b },
                ) => self_a.compare(other_a, math).then_with(|| self_b.compare(other_b, math)),
                (
                    Self::AveragePoint { items: self_items },
                    Self::AveragePoint { items: other_items }
                ) => self_items.compare(other_items, math),
                (
                    Self::CircleCenter { circle: self_x },
                    Self::CircleCenter { circle: other_x }
                ) | (
                    Self::PointX { point: self_x },
                    Self::PointX { point: other_x }
                ) | (
                    Self::PointY { point: self_x },
                    Self::PointY { point: other_x }
                ) => self_x.compare(other_x, math),
                (
                    Self::Sum { plus: self_v, minus: self_u },
                    Self::Sum { plus: other_v, minus: other_u }
                ) | (
                    Self::Product { times: self_v, by: self_u },
                    Self::Product { times: other_v, by: other_u }
                ) => self_v.compare(other_v, math).then_with(|| self_u.compare(other_u, math)),
                (
                    Self::Const { value: self_v },
                    Self::Const { value: other_v }
                ) => self_v.cmp(other_v),
                (
                    Self::Power { value: self_v, exponent: self_exp },
                    Self::Power { value: other_v, exponent: other_exp },
                ) => self_v.compare(other_v, math).then_with(|| self_exp.cmp(other_exp)),
                (
                    Self::ThreePointAngle { p: self_p, q: self_q, r: self_r },
                    Self::ThreePointAngle { p: other_p, q: other_q, r: other_r },
                ) | (
                    Self::ThreePointAngleDir { p: self_p, q: self_q, r: self_r },
                    Self::ThreePointAngleDir { p: other_p, q: other_q, r: other_r },
                )| (
                    Self::AngleBisector { p: self_p, q: self_q, r: self_r },
                    Self::AngleBisector { p: other_p, q: other_q, r: other_r },
                ) => self_p.compare(other_p, math).then_with(|| self_q.compare(other_q, math)).then_with(|| self_r.compare(other_r, math)),
                (_, _) => Ordering::Equal
            })
    }
}

impl FindEntities for ExprKind {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::Entity { id } => {
                set.insert(*id);
            }
            Self::AveragePoint { items } => {
                set.extend(items.iter().flat_map(|x| previous[x.0].iter().copied()));
            }
            Self::CircleCenter { circle: x }
            | Self::PointX { point: x }
            | Self::PointY { point: x }
            | Self::Power { value: x, .. }=> {
                set.extend(previous[x.0].iter().copied());
            }
            Self::Sum { plus: v1, minus: v2 }
            | Self::Product { times: v1, by: v2 } => {
                set.extend(v1.iter().copied().flat_map(|x| previous[x.0].iter().copied()));
                set.extend(v2.iter().copied().flat_map(|x| previous[x.0].iter().copied()));
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
                    UnrolledCircle::Generic(_) => unreachable!()
                }
            },
            UnrolledPoint::Free => ExprKind::Entity { id: math.add_point() },
            UnrolledPoint::Generic(_) => unreachable!()
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
                    times: vec![math.store(times, ExprType::Number)],
                    by: vec![math.store(by, ExprType::Number)],
                }
            },
            UnrolledScalar::CircleRadius(circle) => {
                match circle.get_data() {
                    UnrolledCircle::Circle(_, radius) => Self::load(radius, math),
                    UnrolledCircle::Generic(_) => unreachable!()
                }
            }
            UnrolledScalar::Free => ExprKind::Entity { id: math.add_real() },
            UnrolledScalar::Number(x) => return fix_dst(
                ExprKind::Const { value: x.clone() }, expr.data.unit, math
            ),
            UnrolledScalar::DstLiteral(x) => ExprKind::Const { value: x.clone() },
            UnrolledScalar::SetUnit(x, unit) => return fix_dst(Self::load(x, math), Some(*unit), math),
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
                exponent: *exponent
            },
            UnrolledScalar::PointX(point) => ExprKind::PointX {
                point: math.load(point)
            },
            UnrolledScalar::PointY(point) => ExprKind::PointY {
                point: math.load(point)
            },
            UnrolledScalar::Generic(_) => unreachable!()
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
            UnrolledLine::Generic(_) => unreachable!()
        };

        kind.normalize(math);
        kind
    }
}

impl FromUnrolled<UnrolledCircle> for ExprKind {
    fn load(expr: &Unrolled<UnrolledCircle>, math: &mut Expand) -> Self {
        let mut kind = match expr.data.as_ref() {
            UnrolledCircle::Circle(center, radius) => Self::ConstructCircle {
                center: math.load(center),
                radius: math.load(radius)
            },
            UnrolledCircle::Generic(_) => unreachable!()
        };

        kind.normalize(math);
        kind
    }
}

impl Normalize for ExprKind {
    fn normalize(&mut self, math: &mut Math) {
        let cmp_and_swap = |a: &mut VarIndex, b: &mut VarIndex| {
            if math.compare(*a, *b) == Ordering::Less {
                mem::swap(a, b);
            }
        };
        let cmp = |a: &VarIndex, b: &VarIndex| math.compare(*a, *b);
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
                cmp_and_swap(a, b);
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
            Self::PerpendicularThrough { point, line } => {
                new_self = Some(match &math.at(*line).kind {
                    Self::ParallelThrough { line, .. } => Self::PerpendicularThrough { point: *point, line: *line },
                    Self::PerpendicularThrough { line, .. } => Self::ParallelThrough { point: *point, line: *line },
                    _ => Self::PerpendicularThrough { point: *point, line: *line }
                });
            }
        }

        if let Some(new_self) = new_self {
            *self = new_self;
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
                let dst_var = math.get_dst_var();
                ExprKind::Product {
                    times: vec![
                        math.store(expr, ExprType::Number),
                        math.store(ExprKind::Power {
                            value: dst_var,
                            exponent: unit.0[SimpleUnit::Distance as usize]
                        }, ExprType::Number)
                    ],
                    by: Vec::new()
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Merge<T, I, J, F>
    where I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> Ordering {
    i: Peekable<I>,
    j: Peekable<J>,
    f: F
}

impl<T, I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> Ordering> Merge<T, I, J, F> {
    #[must_use]
    pub fn new<A: IntoIterator<IntoIter = I>, B: IntoIterator<IntoIter = J>>(a: A, b: B, f: F) -> Self {
        Self {
            i: a.into_iter().peekable(),
            j: b.into_iter().peekable(),
            f
        }
    }

    #[must_use]
    pub fn merge_with<It: IntoIterator<Item = T>>(self, other: It) -> Merge<T, Self, It::IntoIter, F> where F: Clone {
        let f_cloned = self.f.clone();
        Merge::new(self, other, f_cloned)
    }
}

impl<T, F: FnMut(&T, &T) -> Ordering> Merge<T, std::option::IntoIter<T>, std::option::IntoIter<T>, F> {
    #[must_use]
    pub fn empty(f: F) -> Self {
        Self::new(None, None, f)
    }
}

impl<T, I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> Ordering> Iterator for Merge<T, I, J, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(i_item) = self.i.peek() {
            if let Some(j_item) = self.j.peek() {
                if (self.f)(i_item, j_item) == Ordering::Less {
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

fn normalize_sum(plus: &mut Vec<VarIndex>, minus: &mut Vec<VarIndex>, math: &mut Math) {
    let plus_v = mem::take(plus);
    let minus_v = mem::take(minus);

    let mut constant = ProcNum::zero();

    let mut plus_final = Vec::new();
    let mut minus_final = Vec::new();

    let cmp = |a: &VarIndex, b: &VarIndex| {
        math.compare(*a, *b)
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
                plus_final = Merge::new(plus_final, minus.iter().copied(), &cmp).collect();
                minus_final = Merge::new(minus_final, plus.iter().copied(), &cmp).collect();
            }
            ExprKind::Const { value } => constant -= value,
            _ => {
                minus_final = Merge::new(minus_final, Some(item), &cmp).collect();
            }
        }
    }

    if !constant.is_zero() {
        plus_final.push(math.store(ExprKind::Const { value: constant }, ExprType::Number));
    }

    *plus = plus_final;
    *minus = minus_final;
}

fn normalize_product(times: &mut Vec<VarIndex>, by: &mut Vec<VarIndex>, math: &mut Math) {
    let times_v = mem::take(times);
    let by_v = mem::take(by);

    let mut constant = ProcNum::one();

    let mut times_final = Vec::new();
    let mut by_final = Vec::new();

    let cmp = |a: &VarIndex, b: &VarIndex| {
        math.compare(*a, *b)
    };

    for item in times_v {
        match &math.at(item).kind {
            ExprKind::Product {
                times, by
            } => {
                times_final = Merge::new(times_final, times.iter().copied(), &cmp).collect();
                by_final = Merge::new(by_final, by.iter().copied(), &cmp).collect();
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
                times_final = Merge::new(times_final, by.iter().copied(), &cmp).collect();
                by_final = Merge::new(by_final, times.iter().copied(), &cmp).collect();
            }
            ExprKind::Const { value } => constant /= value,
            _ => {
                by_final = Merge::new(by_final, Some(item), &cmp).collect();
            }
        }
    }

    if !constant.is_one() {
        times_final.push(math.store(ExprKind::Const { value: constant }, ExprType::Number));
    }

    *times = times_final;
    *by = by_final;
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash, Serialize)]
pub struct Expr<M> {
    pub meta: M,
    pub kind: ExprKind,
    pub ty: ExprType
}

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

impl<M> ContainsEntity for Expr<M> {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
        self.kind.contains_entity(entity, math)
    }
}

impl<M> Normalize for Expr<M> {
    fn normalize(&mut self, math: &mut Math) {
        self.kind.normalize(math);
    }
}

impl Expr<()> {
    #[must_use]
    pub fn new(kind: ExprKind, ty: ExprType) -> Self {
        Self {
            kind,
            meta: (),
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
    impl ContainsEntity for Self {
        fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
            aggregate = ||,
            init = false
        }
    }
)]
#[recursive(
    impl Reconstruct for Self {
        fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
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
                return items.iter().flat_map(|x| x.find_entities(previous).into_iter()).collect();
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

impl ContainsEntity for Rule {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
        self.kind.contains_entity(entity, math)
    }
}

impl Reconstruct for Rule {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        Self {
            kind: self.kind.reconstruct(ctx),
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
            UnrolledRuleKind::ScalarEq(a, b) => Self::NumberEq(
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
    fn normalize(&mut self, math: &mut Math) {
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
    fn normalize(&mut self, math: &mut Math) {
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

#[derive(Debug, Clone, Serialize)]
pub struct Entity<M> {
    pub kind: EntityKind,
    pub meta: M
}

#[derive(Debug, Clone, Recursive, Serialize)]
#[recursive(
    impl ContainsEntity for Self {
        fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
            aggregate = ||,
            init = false
        }
    }
)]
#[recursive(
    impl Reconstruct for Self {
        fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
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

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize)]
pub struct EntityId(pub usize);

impl Reindex for EntityId {
    fn reindex(&mut self, _map: &IndexMap) {}
}

impl DeepClone for EntityId {
    fn deep_clone(&self, _math: &mut Math) -> Self {
        // The clone is not THAT deep
        *self
    }
}

impl ContainsEntity for EntityId {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
        *self == entity || math.entities[self.0].contains_entity(entity, math)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Expand {
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, VarIndex>,
    /// Processing context
    pub math: Math
}

impl Deref for Expand {
    type Target = Math;

    fn deref(&self) -> &Self::Target {
        &self.math
    }
}

impl DerefMut for Expand {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.math
    }
}

#[derive(Debug, Clone, Default)]
pub struct Math {
    /// All found entities
    pub entities: Vec<EntityKind>,
    /// Dst variable
    pub dst_var: OnceCell<EntityId>,
    /// Collected expressions
    pub expr_record: Vec<Expr<()>>
}

impl Expand {
    pub fn load<T: Displayed + GetMathType>(&mut self, expr: &Unrolled<T>) -> VarIndex
    where ExprKind: FromUnrolled<T> {
        let key = (expr.data.as_ref() as *const _) as usize;
        let loaded = self.expr_map.get_mut(&key).copied();

        if let Some(loaded) = loaded {
            // Shallow clone here is weirs but necessary.
            let expr = self.at(loaded).kind.clone().deep_clone(self);
            let ty = self.at(loaded).ty;
            self.store(expr, ty)
        } else {
            // If expression has not been mathed yet, math it and put it into the record.
            let loaded = ExprKind::load(expr, self);
            let index = self.store(loaded, T::get_math_type());
            self.expr_map.insert(key, index);
            index
        }
    }
}

impl Math {
    #[must_use]
    pub fn store(&mut self, expr: ExprKind, ty: ExprType) -> VarIndex {
        self.expr_record.push(Expr::new(expr, ty));
        VarIndex(self.expr_record.len() - 1)
    }

    #[must_use]
    pub fn compare(&self, a: VarIndex, b: VarIndex) -> Ordering {
        self.at(a).kind.compare(&self.at(b).kind, self)
    }

    /// # Panics
    /// Will never.
    #[must_use]
    pub fn get_dst_var(&mut self) -> VarIndex {
        let id = self.dst_var.get();
        let is_some = id.is_some();

        let id = *if is_some {
            id.unwrap()
        } else {
            let real = self.add_real();
            self.dst_var.get_or_init(|| real)
        };

        self.store(ExprKind::Entity { id }, ExprType::Number)
    }

    #[must_use]
    pub fn at(&self, index: VarIndex) -> &Expr<()> {
        &self.expr_record[index.0]
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
    items: Vec<Item>
}

impl Build {
    pub fn load<T: Displayed + GetMathType>(&mut self, expr: &Unrolled<T>) -> VarIndex
    where ExprKind: FromUnrolled<T> {
        self.expand.load(expr)
    }

    pub fn add<I: Into<Item>>(&mut self, item: I) {
        self.items.push(item.into());
    }
}

/// Tries to transform the rules so that they are simpler to process for the generator.
///
/// # Returns
/// `true` if an optimization was performed. `false` otherwise.
fn optimize_rules(rules: &mut Vec<Option<Rule>>, math: &mut Math) -> bool {
    let mut performed = false;
    
    for rule in rules.iter_mut() {
        performed = performed
            | ZeroLineDst::process(rule, math)
            | RightAngle::process(rule, math)
            | EqPointDst::process(rule, math)
            | EqExpressions::process(rule, math)
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

    #[must_use]
    pub fn get(&self, a: usize) -> usize {
        self.a_to_b.get(&a).copied().unwrap_or(a)
    }

    /// Creates a mapping from a to b (works like function composition).
    pub fn map(&mut self, a: usize, b: usize) {
        // Before everything, if a == b, nothing happens.
        if a == b {
            return;
        }

        // If `a` is present in `b->a`, that means there exists a direct mapping a->b which we should override and remove the `b->a` map.
        // Otherwise, if `a` is not present in `a->b`, we should add it along with a `b->a` mapping.
        // If `a` is present in `a->b` but not present in `b->a`, `a` is not reachable, thus nothing should happen.
        let a_key = self.b_to_a.get(&a).copied();
        if let Some(a_key) = a_key {
            // There's a mapping `a_key -> a`
            // Remove the inverse mapping `a -> a_key`
            self.b_to_a.remove(&a);

            if a_key == b {
                // Also remove a -> b
                self.a_to_b.remove(&a_key);
            } else {
                // Make the mapping a_key -> b
                if let Some(current) = self.a_to_b.get_mut(&a_key) {
                    *current = b;
                }
                // Insert the new mapping b -> a_key
                self.b_to_a.insert(b, a_key);
            }
        } else if let hash_map::Entry::Vacant(e) = self.a_to_b.entry(a) {
            // There is no a -> ?.
            e.insert(b);
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

impl Reindex for CompExponent {
    fn reindex(&mut self, _map: &IndexMap) {}
}

impl Reindex for ProcNum {
    fn reindex(&mut self, _map: &IndexMap) {}
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
fn fold(matrix: &mut Vec<Expr<()>>) -> IndexMap {
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

/// Optimize, Normalize, Repeat
fn optimize_cycle(rules: &mut Vec<Option<Rule>>, math: &mut Math, items: &mut Vec<Item>) {
    let mut entity_map = Vec::new() ;
    loop {
        if !optimize_rules(rules, math) {
            break;
        }

        // Create the entity reduction map:
        // If an entity is a bind, it should be turned into an expression
        // Remaining entities should be appropriately offset.

        let mut offset = 0;
        entity_map.clear();

        // We'll need mutable access to expand in the loop, so we take the entities out of it.
        let mut entities = mem::take(&mut math.entities);

        for (i, entity) in entities.iter().enumerate() {
            entity_map.push(match entity {
                EntityKind::FreePoint
                | EntityKind::PointOnCircle { .. }
                | EntityKind::PointOnLine { .. } => math.store(ExprKind::Entity { id: EntityId(i - offset) }, ExprType::Point),
                EntityKind::FreeReal => math.store(ExprKind::Entity { id: EntityId(i - offset) }, ExprType::Number),
                EntityKind::Bind(expr) => {
                    offset += 1;
                    *expr
                }
            });
        }

        entities.retain(|x| !matches!(x, EntityKind::Bind(_)));

        // The entities are now corrected, but the rules, entities (sic) and variables don't know that.
        // The easiest way to fix it is to reconstruct the loaded variable vector recursively.
        // This way, we can also fix forward referencing and remove potentially unused expressions.
        // We'll also have to update items. Otherwise, they will have misguided indices.

        let old_vars = mem::take(&mut math.expr_record);
        let mut ctx = ReconstructCtx::new(&entity_map, &old_vars);
        math.entities = entities.reconstruct(&mut ctx);
        let old_items = mem::take(items);
        *items = old_items.reconstruct(&mut ctx);
        let old_rules = mem::take(rules);
        *rules = old_rules.reconstruct(&mut ctx);
        math.expr_record = ctx.new_vars;

        // After reconstruction, all forward referencing is gone.

        // Normalize expressions. Unfortunately due to borrow checking this requires quite the mess.
        // First, we clone the entire expression vector (shallow, this time) and normalize it.
        let v = math.expr_record.clone();
        for (i, mut var) in v.into_iter().enumerate() {
            var.normalize(math);
            // Then, set each corresponding var.
            math.expr_record[i] = var;
        }

        // Normalize all rules now
        for rule in rules.iter_mut().flatten() {
            rule.normalize(math);
        }
    }
}

/// # Errors
/// Returns an error if the script is not a valid one.
/// Any errors should result from tokenizing, parsing and unrolling, not mathing.
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
    Box::new(nodes).build(&mut build);

    // Move expand base
    let mut expand = build.expand;

    // Expand & normalize rules
    let mut rules = Vec::new();

    for rule in unrolled.take_rules() {
        rules.push(Some(Rule::load(&rule, &mut expand)));
    }

    // Get the math out of the `Expand`.
    let mut math = expand.math;

    optimize_cycle(&mut rules, &mut math, &mut build.items);

    // Now everything that could be normalized is normalized.
    // Unfortunately, normalization can introduce forward referencing, which is not what we want.
    // This means we have to fix it. And the easiest way to fix it is to reconstruct it once more.

    let old_entities = mem::take(&mut math.entities);
    let entity_map: Vec<_> = old_entities
        .iter()
        .enumerate()
        .map(|(i, ent)| match ent {
            EntityKind::FreePoint
            | EntityKind::PointOnLine { .. }
            | EntityKind::PointOnCircle { .. } => math.store(ExprKind::Entity { id: EntityId(i) }, ExprType::Point),
            EntityKind::FreeReal => math.store(ExprKind::Entity { id: EntityId(i) }, ExprType::Number),
            EntityKind::Bind(_) => unreachable!()
        })
        .collect();

    let old_vars = mem::take(&mut math.expr_record);
    let mut ctx = ReconstructCtx::new(&entity_map, &old_vars);
    math.entities = old_entities.reconstruct(&mut ctx);
    build.items = build.items.reconstruct(&mut ctx);
    rules = rules.reconstruct(&mut ctx);
    math.expr_record = ctx.new_vars;

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

    let mut entities = math.entities;
    let mut variables = math.expr_record;
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
            meta: counter.next().unwrap_or_default()
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
            meta: counter.next().unwrap_or_default(),
            ty: expr.ty
        })
        .collect();

    Ok(Intermediate {
        adjusted: Adjusted {
            variables: variables.into_iter().map(|expr| Expr { kind: expr.kind, ty: expr.ty, meta: () }).collect(),
            rules,
            entities: adj_entities.into_iter().map(|kind| Entity { kind, meta: () }).collect()
        },
        figure: Figure {
            entities,
            variables: fig_variables,
            items
        },
        flags: read_flags(&unrolled.flags)
    })
}