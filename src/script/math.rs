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
use std::iter::Peekable;
use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, DerefMut};
use derive_recursive::Recursive;
use num_traits::{FromPrimitive, One, Zero};

use crate::script::figure::Item;
use crate::script::math::optimizations::ZeroLineDst;
use crate::script::token::number::{CompExponent, ProcNum};
use crate::script::unroll::figure::Node;

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

trait HandleEntity<I: Indirection> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool;

    fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self;
}

fn map_entity_over<T, I: Indirection>(entity_id: EntityId, mapped: EntityId, into: &Any<I>) -> T
    where Any<I>: TryInto<T> {
    if entity_id == mapped {
        into.clone().try_into().unwrap()
    }
}

impl<I: Indirection> HandleEntity<I> for ProcNum {
    fn contains_entity(&self, _entity: EntityId, _entities: &[EntityKind<I>]) -> bool {
        false
    }

    fn map_entity(self, _entity: EntityId, _into: &Any<I>) -> Self {
        self
    }
}

impl<I: Indirection, T: HandleEntity<I>> HandleEntity<I> for Box<T> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
        self.as_ref().contains_entity(entity, entities)
    }

    fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
        Self::new((*self).map_entity(entity, into))
    }
}

impl<I: Indirection, T: HandleEntity<I>> HandleEntity<I> for Vec<T> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
        self.iter().any(|item| item.contains_entity(entity, entities))
    }

    fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
        self.into_iter().map(|x| x.map_entity(entity, into)).collect()
    }
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

trait FindEntities {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId>;
}

impl FindEntities for Vec<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        self.iter().map(|x| previous[x.0].iter().copied()).flatten().collect()
    }
}

trait Flatten {
    type Output;

    /// Maps an `ExprTypes` into a `VarIndex`
    fn flatten(self, context: &mut FlattenContext) -> Self::Output;
}

impl<T: Flatten> Flatten for Vec<T> {
    type Output = Vec<T::Output>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        self.into_iter().map(|x| x.flatten(context)).collect()
    }
}

impl<T: Flatten> Flatten for Box<T> {
    type Output = Box<T::Output>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        Box::new((*self).flatten(context))
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

pub trait Indirection {
    type Point;
    type Line;
    type Number;
    type Circle;
}

#[derive(Debug, Clone, Copy)]
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

impl Indirection for VarIndex {
    type Point = Self;
    type Line = Self;
    type Number = Self;
    type Circle = Self;
}

#[derive(Debug, Clone, Copy, Default, Ord, PartialOrd, Eq, PartialEq)]
pub struct ExprTypes<M>(PhantomData<M>);

impl<M> ExprTypes<M> {
    #[must_use]
    pub fn new() -> Self {
        Self(PhantomData::default())
    }
}

impl<M> Indirection for ExprTypes<M> {
    type Point = PointExpr<M>;
    type Line = LineExpr<M>;
    type Number = NumberExpr<M>;
    type Circle = CircleExpr<M>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive, Hash)]
#[recursive(
    impl<I: Indirection> HandleEntity<I> for Self<I>
    where
        I::Point: HandleEntity<I>,
        I::Line: HandleEntity<I>,
        I::Circle: HandleEntity<I>,
        I::Number: HandleEntity<I> {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
            aggregate = ||
        }

        fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
            aggregate = {},
            override_marker = override_map
        }
    }
)]
pub enum Point<I: Indirection> {
    /// k and l must be ordered
    LineLineIntersection {
        k: I::Line,
        l: I::Line
    },
    /// items must be sorted
    Average {
        items: Vec<I::Point>
    },
    CircleCenter {
        circle: I::Circle
    },
    #[recursive(override_map = map_entity_over::<Self, I>)]
    Entity {
        id: EntityId
    }
}

pub type PointExpr<M> = Expr<Point<ExprTypes<M>>, M>;

impl FindEntities for Point<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::LineLineIntersection { k, l } => {
                set.extend(previous[k.0].iter().copied());
                set.extend(previous[l.0].iter().copied());
            }
            Self::Average { items } => {
                set.extend(items.iter().map(|x| previous[x.0].iter().copied()).flatten());
            }
            Self::CircleCenter { circle } => {
                set.extend(previous[circle.0].iter().copied());
            }
            Self::Entity { id } => {
                set.insert(*id);
            }
        }

        set
    }
}

impl Reindex for Point<VarIndex> {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Self::LineLineIntersection { k, l } => {
                k.reindex(map);
                l.reindex(map);
            }
            Self::Average { items } => {
                items.reindex(map);
            }
            Self::CircleCenter { circle } => {
                circle.reindex(map)
            }
            Self::Entity { .. } => {}
        }
    }
}

impl<I: Indirection> Default for Point<I> {
    fn default() -> Self {
        Self::Entity { id: EntityId(0) }
    }
}

impl Flatten for Point<ExprTypes<()>> {
    type Output = Point<VarIndex>;
    
    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        match self {
            Self::LineLineIntersection { k, l } => {
                Point::LineLineIntersection {
                    k: k.flatten(context),
                    l: l.flatten(context)
                }
            }
            Self::Average { items } => Point::Average {
                items: items.flatten(context)
            },
            Self::CircleCenter { circle } => Point::CircleCenter {
                circle: circle.flatten(context)
            },
            Self::Entity { id } => Point::Entity { id }
        }
    }
}

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

impl<M: Ord> Normalize for Point<ExprTypes<M>> {
    fn normalize(&mut self) {
        match self {
            Point::Entity { .. } => (),
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive, Hash)]
#[recursive(
    impl<I: Indirection> HandleEntity<I> for Self<I>
    where
        I::Point: HandleEntity<I>,
        I::Line: HandleEntity<I>,
        I::Circle: HandleEntity<I>,
        I::Number: HandleEntity<I> {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
            aggregate = ||
        }

        fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
            aggregate = {},
            override_marker = override_map
        }
    }
)]
pub enum Number<I: Indirection> {
    #[recursive(override_map = map_entity_over::<Self, I>)]
    Entity {
        id: EntityId
    },
    /// plus and minus must be sorted and must not contain other sums. An aggregated constant, if any, must be at the end.
    Sum {
        plus: Vec<I::Number>,
        minus: Vec<I::Number>
    },
    /// times and by must be sorted and must not contain other sums. An aggregated constant, if any, must be at the end.
    Product {
        times: Vec<I::Number>,
        by: Vec<I::Number>
    },
    Const {
        value: ProcNum
    },
    Power {
        value: I::Number,
        exponent: CompExponent
    },
    /// p and q must be ordered
    PointPointDistance {
        p: I::Point,
        q: I::Point
    },
    PointLineDistance {
        p: I::Point,
        k: I::Line
    },
    /// p and r must be ordered
    ThreePointAngle {
        p: I::Point,
        q: I::Point,
        r: I::Point
    },
    /// p and r must be ordered
    ThreePointAngleDir {
        p: I::Point,
        q: I::Point,
        r: I::Point
    },
    /// k and l must be ordered
    TwoLineAngle {
        k: I::Line,
        l: I::Line
    },
    PointX {
        point: I::Point
    },
    PointY {
        point: I::Point
    }
}

pub type NumberExpr<M> = Expr<Number<ExprTypes<M>>, M>;

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

impl FindEntities for Number<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::Entity { id } => {
                set.insert(*id);
            }
            Self::Sum { plus: v1, minus: v2 }
            | Self::Product { times: v1, by: v2 } => {
                set.extend(v1.iter().copied());
                set.extend(v2.iter().copied());
            }
            Self::PointPointDistance { p: a, q: b }
            | Self::PointLineDistance { p: a, k: b }
            | Self::TwoLineAngle { k: a, l: b } => {
                set.extend(previous[a.0].iter().copied());
                set.extend(previous[b.0].iter().copied());
            }
            Self::ThreePointAngle { p, q, r }
            | Self::ThreePointAngleDir { p, q, r } => {
                set.extend(previous[p.0].iter().copied());
                set.extend(previous[q.0].iter().copied());
                set.extend(previous[r.0].iter().copied());
            }
            Self::PointX { point: x }
            | Self::PointY { point: x }
            | Self::Power { value: x, .. } => {
                set.extend(previous[x.0].iter().copied());
            }
            Self::Const { .. } => {}
        }

        set
    }
}

impl Reindex for Number<VarIndex> {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Self::Entity { .. }
            | Self::Const { .. } => {}
            Self::Sum { plus: v1, minus: v2 }
            | Self::Product { times: v1, by: v2 } => {
                v1.reindex(map);
                v2.reindex(map);
            }
            Self::PointPointDistance { p: a, q: b }
            | Self::PointLineDistance { p: a, k: b }
            | Self::TwoLineAngle { k: a, l: b } => {
                a.reindex(map);
                b.reindex(map);
            }
            Self::ThreePointAngle { p, q, r }
            | Self::ThreePointAngleDir { p, q, r } => {
                p.reindex(map);
                q.reindex(map);
                r.reindex(map);
            }
            Self::PointX { point: x }
            | Self::PointY { point: x }
            | Self::Power { value: x, .. } => {
                x.reindex(map);
            }
        }
    }
}

impl Flatten for Number<ExprTypes<()>> {
    type Output = Number<VarIndex>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        match self {
            Number::Entity { id } => Number::Entity { id },
            Number::Sum { plus, minus } => Number::Sum {
                plus: plus.flatten(context),
                minus: minus.flatten(context)
            },
            Number::Product { times, by } => Number::Product {
                times: times.flatten(context),
                by: by.flatten(context)
            },
            Number::Const { value } => Number::Const { value },
            Number::Power { value, exponent } => Number::Power {
                value: value.flatten(context),
                exponent
            },
            Number::PointPointDistance { p, q } => Number::PointPointDistance {
                p: p.flatten(context),
                q: q.flatten(context)
            },
            Number::PointLineDistance { p, k } => Number::PointLineDistance {
                p: p.flatten(context),
                k: k.flatten(context)
            },
            Number::ThreePointAngle { p, q, r } => Number::ThreePointAngle {
                p: p.flatten(context),
                q: q.flatten(context),
                r: r.flatten(context)
            },
            Number::ThreePointAngleDir { p, q, r } => Number::ThreePointAngleDir {
                p: p.flatten(context),
                q: q.flatten(context),
                r: r.flatten(context)
            },
            Number::TwoLineAngle { k, l } => Number::TwoLineAngle {
                k: k.flatten(context),
                l: l.flatten(context)
            },
            Number::PointX { point } => Number::PointX { point: point.flatten(context) },
            Number::PointY { point } => Number::PointY { point: point.flatten(context) }
        }
    }
}

impl FromUnrolled<unroll::Scalar> for NumberExpr<()> {
    fn load(expr: &Unrolled<unroll::Scalar>, math: &mut Expand) -> Self {
        let kind = match expr.get_data() {
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

impl<M: Ord> Normalize for Number<ExprTypes<M>> {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive, Hash)]
#[recursive(
    impl<I: Indirection> HandleEntity<I> for Self<I>
        where
        I::Point: HandleEntity<I>,
        I::Line: HandleEntity<I>,
        I::Circle: HandleEntity<I>,
        I::Number: HandleEntity<I> {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
            aggregate = ||
        }

        fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
            aggregate = {},
            override_marker = override_map
        }
    }
)]
pub enum Line<I: Indirection> {
    /// Normalized iff `p` and `q` are in ascending order
    PointPoint {
        p: I::Point,
        q: I::Point
    },
    /// Normalized iff `a` and `c` are in ascending order (`b` must stay in the middle)
    AngleBisector {
        a: I::Point,
        b: I::Point,
        c: I::Point
    },
    /// Always normalized
    ParallelThrough {
        point: I::Point,
        line: I::Line
    },
    /// Always normalized
    PerpendicularThrough {
        point: I::Point,
        line: I::Line
    }
}

pub type LineExpr<M> = Expr<Line<ExprTypes<M>>, M>;

impl FindEntities for Line<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::Entity { id } => {
                set.insert(*id);
            }
            Self::Sum { plus: v1, minus: v2 }
            | Self::Product { times: v1, by: v2 } => {
                set.extend(v1.iter().copied());
                set.extend(v2.iter().copied());
            }
            Self::ParallelThrough { point: a, line: b }
            | Self::PerpendicularThrough { point: a, line: b }
            | Self::PointPoint { p: a, q: b } => {
                set.extend(previous[a.0].iter().copied());
                set.extend(previous[b.0].iter().copied());
            }
            Self::AngleBisector { a, b, c } => {
                set.extend(previous[a.0].iter().copied());
                set.extend(previous[b.0].iter().copied());
                set.extend(previous[c.0].iter().copied());
            }
        }

        set
    }
}

impl Reindex for Line<VarIndex> {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Self::PointPoint { p: a, q: b }
            | Self::ParallelThrough { point: a, line: b }
            | Self::PerpendicularThrough { point: a, line: b } => {
                a.reindex(map);
                b.reindex(map);
            }
            Self::AngleBisector { a, b, c } => {
                a.reindex(map);
                b.reindex(map);
                c.reindex(map);
            }
        }
    }
}

impl Flatten for Line<ExprTypes<()>> {
    type Output = Line<VarIndex>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        match self {
            Line::PointPoint { p, q } => Line::PointPoint {
                p: p.flatten(context),
                q: q.flatten(context)
            },
            Line::AngleBisector { a, b, c } => Line::AngleBisector {
                a: a.flatten(context),
                b: b.flatten(context),
                c: c.flatten(context)
            },
            Line::ParallelThrough { point, line } => Line::ParallelThrough {
                point: point.flatten(context),
                line: line.flatten(context)
            },
            Line::PerpendicularThrough { point, line } => Line::PerpendicularThrough {
                point: point.flatten(context),
                line: line.flatten(context)
            }
        }
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

impl<M: Ord> Normalize for Line<ExprTypes<M>> {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive, Hash)]
#[recursive(
    impl<I: Indirection> HandleEntity<I> for Self<I>
    where
        I::Point: HandleEntity<I>,
        I::Line: HandleEntity<I>,
        I::Circle: HandleEntity<I>,
        I::Number: HandleEntity<I> {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
            aggregate = ||
        }

        fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
            aggregate = {},
            override_marker = override_map
        }
    }
)]
pub enum Circle<I: Indirection> {
    Construct {
        center: I::Point,
        radius: I::Number
    }
}

pub type CircleExpr<M> = Expr<Circle<ExprTypes<M>>, M>;

impl FindEntities for Circle<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::Circle { center: a, radius: b } => {
                set.extend(previous[a.0].iter().copied());
                set.extend(previous[b.0].iter().copied());
            }
        }

        set
    }
}

impl Reindex for Circle<VarIndex> {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Self::Construct { center, radius } => {
                center.reindex(map);
                radius.reindex(map);
            }
        }
    }
}

impl Flatten for Circle<ExprTypes<()>> {
    type Output = Circle<VarIndex>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        match self {
            Circle::Construct { center, radius } => Circle::Construct {
                center: center.flatten(context),
                radius: radius.flatten(context)
            }
        }
    }
}

impl<M: Ord> Normalize for Circle<ExprTypes<M>> {
    fn normalize(&mut self) {
        match self {
            Circle::Construct { mut center, mut radius } => {
                center.normalize();
                radius.normalize();
            }
        }
    }
}

#[derive(Debug, Clone, Recursive, Hash)]
#[recursive(
    impl<I: Indirection> HandleEntity<I> for Self<I> {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
            aggregate = ||
        }

        fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
            aggregate = {}
        }
    }
)]
pub enum Any<I: Indirection> {
    Point(Point<I>),
    Number(Number<I>),
    Line(Line<I>),
    Circle(Circle<I>)
}

impl Reindex for Any<VarIndex> {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Any::Point(v) => v.reindex(map),
            Any::Number(v) => v.reindex(map),
            Any::Line(v) => v.reindex(map),
            Any::Circle(v) => v.reindex(map),
        }
    }
}

impl FindEntities for Any<VarIndex> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        match self {
            Any::Point(v) => v.find_entities(previous),
            Any::Number(v) => v.find_entities(previous),
            Any::Line(v) => v.find_entities(previous),
            Any::Circle(v) => v.find_entities(previous),
        }
    }
}

impl<I: Indirection> Default for Any<I> {
    fn default() -> Self {
        Self::Point(Point::default())
    }
}

pub type AnyExpr<M> = Expr<Any<ExprTypes<M>>, M>;

impl<M: Indirection> From<Number<M>> for Any<M> {
    fn from(value: Number<M>) -> Self {
        Self::Number(value)
    }
}

impl<M: Indirection> From<Line<M>> for Any<M> {
    fn from(value: Line<M>) -> Self {
        Self::Line(value)
    }
}

impl<M: Indirection> From<Point<M>> for Any<M> {
    fn from(value: Point<M>) -> Self {
        Self::Point(value)
    }
}

impl<M: Indirection> From<Circle<M>> for Any<M> {
    fn from(value: Circle<M>) -> Self {
        Self::Circle(value)
    }
}

#[derive(Debug)]
pub struct WrongVariant;

impl<M: Indirection> TryFrom<Any<M>> for Point<M> {
    type Error = WrongVariant;

    fn try_from(value: Any<M>) -> Result<Self, Self::Error> {
        if let Any::Point(point) = value {
            Ok(point)
        } else {
            Err(WrongVariant)
        }
    }
}

impl<M: Indirection> TryFrom<Any<M>> for Number<M> {
    type Error = WrongVariant;

    fn try_from(value: Any<M>) -> Result<Self, Self::Error> {
        if let Any::Number(number) = value {
            Ok(number)
        } else {
            Err(WrongVariant)
        }
    }
}

impl<M: Indirection> TryFrom<Any<M>> for Line<M> {
    type Error = WrongVariant;

    fn try_from(value: Any<M>) -> Result<Self, Self::Error> {
        if let Any::Line(line) = value {
            Ok(line)
        } else {
            Err(WrongVariant)
        }
    }
}

impl<M: Indirection> TryFrom<Any<M>> for Circle<M> {
    type Error = WrongVariant;

    fn try_from(value: Any<M>) -> Result<Self, Self::Error> {
        if let Any::Circle(circle) = value {
            Ok(circle)
        } else {
            Err(WrongVariant)
        }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct Expr<T, M> {
    pub kind: Box<T>,
    pub meta: M
}

impl<T: FindEntities, M> FindEntities for Expr<T, M> {
    fn find_entities(&self, previous: &[HashSet<EntityId>]) -> HashSet<EntityId> {
        self.kind.find_entities(previous)
    }
}

impl<T: Reindex, M> Reindex for Expr<T, M> {
    fn reindex(&mut self, map: &IndexMap) {
        self.kind.reindex(map);
    }
}

macro_rules! impl_flatten {
    ($ex:ident) => {
        impl Flatten for $ex<()> {
            type Output = VarIndex;

            fn flatten(self, context: &mut FlattenContext) -> Self::Output {

                let loaded = (*self.kind).flatten(context).into();
                context.loaded.push(Expr {
                    kind: Box::new(loaded),
                    meta: ()
                });

                VarIndex(context.loaded.len() - 1)
            }
        }
    }
}

impl_flatten!{PointExpr}
impl_flatten!{LineExpr}
impl_flatten!{CircleExpr}
impl_flatten!{NumberExpr}
impl_flatten!{AnyExpr}

impl<T: HandleEntity<M>, M: Indirection> HandleEntity<M> for Expr<T, M> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<M>]) -> bool {
        self.kind.contains_entity(entity, entities)
    }

    fn map_entity(self, entity: EntityId, into: &Any<M>) -> Self {
        Self {
            kind: self.kind.map_entity(entity, into),
            meta: self.meta
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

impl<T, M> Expr<T, M> {
    #[must_use]
    pub fn into_any<I: Indirection>(self) -> Expr<Any<I>, M> where Any<I>: From<T> {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Recursive)]
#[recursive(
    impl<I: Indirection> HandleEntity<I> for Self<I> {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
            aggregate = ||,
            init = false
        }

        fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
            aggregate = {}
        }
    }
)]
pub enum RuleKind<I: Indirection> {
    PointEq(I::Point, I::Point),
    NumberEq(I::Number, I::Number),
    Lt(I::Number, I::Number),
    Gt(I::Number, I::Number),
    Alternative(Vec<RuleKind<I>>),
    Invert(Box<RuleKind<I>>),
    Bias
}

impl FindEntities for RuleKind<VarIndex> {
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
pub struct Rule<I: Indirection> {
    pub kind: RuleKind<I>,
    pub weight: ProcNum,
    pub entities: Vec<EntityId>
}

impl<I: Indirection> HandleEntity<I> for Rule<I> {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
        self.kind.contains_entity(entity, entities)
    }

    fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
        Self {
            kind: self.kind.map_entity(entity, into),
            ..self
        }
    }
}

pub type SimpleRule = Rule<ExprTypes<()>>;

impl Reindex for RuleKind<VarIndex> {
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

impl Reindex for Rule<VarIndex> {
    fn reindex(&mut self, map: &IndexMap) {
        self.kind.reindex(map);
    }
}

impl Flatten for RuleKind<ExprTypes<()>> {
    type Output = RuleKind<VarIndex>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        match self {
            Self::PointEq(p, q) => Rule::PointEq(
                p.flatten(context),
                q.flatten(context)
            ),
            Self::NumberEq(a, b) => Rule::NumberEq(
                a.flatten(context), b.flatten(context)
            ),
            Self::Lt(a, b) => Rule::Lt(
                a.flatten(context), b.flatten(context)
            ),
            Self::Gt(a, b) => Rule::Gt(
                a.flatten(context), b.flatten(context)
            ),
            Self::Alternative(items) => Rule::Alternative(
                items.flatten(context)
            ),
            Self::Invert(rule) => Rule::Invert(rule.flatten(context)),
            Self::Bias => unreachable!()
        }
    }
}

impl Flatten for Rule<ExprTypes<()>> {
    type Output = Rule<VarIndex>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        Rule {
            kind: self.kind.flatten(context),
            weight: self.weight,
            entities: self.entities
        }
    }
}

impl RuleKind<ExprTypes<()>> {
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

impl Rule<ExprTypes<()>> {
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

impl Normalize for RuleKind<ExprTypes<()>> {
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

impl Normalize for Rule<ExprTypes<()>> {
    fn normalize(&mut self) {
        self.kind.normalize();
    }
}

#[derive(Debug)]
pub struct Adjusted {
    pub variables: Vec<Expr<Any<VarIndex>, ()>>,
    pub rules: Vec<Rule<VarIndex>>
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
    pub expr: AnyExpr<()>,
    pub uses: usize
}

#[derive(Debug, Clone)]
pub struct Entity<I: Indirection, M> {
    pub kind: EntityKind<I>,
    pub meta: M
}

impl<I: Indirection, M> HasMeta for Entity<I, M> {
    type Meta = M;
}

impl<I: Indirection, M, Dst> MapMeta<Dst> for Entity<I, M> {
    type Output = Entity<I, Dst>;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, mut f: F) -> Self::Output {
        Entity {
            kind: self.kind,
            meta: f(self.meta)
        }
    }
}

#[derive(Debug, Recursive)]
#[recursive(
    impl<I: Indirection> HandleEntity<I> for Self<I> {
        fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<I>]) -> bool {
            aggregate = ||,
            init = false
        }

        fn map_entity(self, entity: EntityId, into: &Any<I>) -> Self {
            aggregate = {}
        }
    }
)]
pub enum EntityKind<I: Indirection> {
    FreePoint,
    PointOnLine(I::Line),
    FreeReal,
    Bind(Any<I>)
}

impl Clone for EntityKind<VarIndex> {
    fn clone(&self) -> Self {
        match self {
            EntityKind::FreePoint => Self::FreePoint,
            EntityKind::PointOnLine(ln) => Self::PointOnLine(ln.clone()),
            EntityKind::FreeReal => Self::FreeReal,
            EntityKind::Bind(bind) => Self::Bind(bind.clone()),
        }
    }
}

impl Reindex for EntityKind<VarIndex> {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            EntityKind::FreePoint
            | EntityKind::FreeReal => {}
            EntityKind::PointOnLine(line) => line.reindex(map),
            EntityKind::Bind(_) => unreachable!("Should not appear")
        }
    }
}

impl Flatten for EntityKind<ExprTypes<()>> {
    type Output = EntityKind<VarIndex>;

    fn flatten(self, context: &mut FlattenContext) -> Self::Output {
        match self {
            Self::FreePoint => EntityKind::FreePoint,
            Self::PointOnLine(line) => EntityKind::PointOnLine(line.flatten(context)),
            Self::FreeReal => EntityKind::FreeReal,
            Self::Bind(_) => unreachable!("bound entities should never appear in the flattening step")
        }
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct EntityId(pub usize);

impl HandleEntity<ExprTypes<()>> for EntityId {
    fn contains_entity(&self, entity: EntityId, entities: &[EntityKind<ExprTypes<()>>]) -> bool {
        *self == entity || entities[self.0].contains_entity(entity, entities)
    }

    fn map_entity(self, _: EntityId, _: &Any<ExprTypes<()>>) -> Self {
        unreachable!("map_entity should never be called for EntityId")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Expand {
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, AnyExpr<()>>,
    /// All found entities
    pub entities: Vec<EntityKind<ExprTypes<()>>>,
    /// Dst variable
    pub dst_var: OnceCell<NumberExpr<()>>
}

impl Expand {
    pub fn load<T: Displayed, U>(&mut self, expr: &Unrolled<T>) -> Expr<U, ()>
        where Any<ExprTypes<()>>: From<U>, Expr<U, ()>: FromUnrolled<T> {
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

    fn add_entity(&mut self, entity: EntityKind<ExprTypes<()>>) -> EntityId {
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

#[derive(Debug, Clone, Default)]
struct FlattenContext {
    loaded: Vec<Expr<Any<VarIndex>, ()>>
}

/// Tries to transform the rules so that they are simpler to process for the generator.
///
/// # Returns
/// `true` if an optimization was performed. `false` otherwise.
fn optimize_rules(rules: &mut Vec<Option<SimpleRule>>, entities: &mut [EntityKind<ExprTypes<()>>]) -> bool {
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
fn fold(matrix: &mut Vec<Expr<Any<VarIndex>, ()>>) -> IndexMap {
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

        for (i, entity) in expand.entities.iter().enumerate() {
            intos.push(match entity {
                EntityKind::FreePoint
                | EntityKind::PointOnLine(_) => Any::Point(Point::Entity { id: EntityId(i - offset) }),
                EntityKind::FreeReal => Any::Number(Number::Entity { id: EntityId(i - offset) }),
                EntityKind::Bind(expr) => {
                    offset += 1;
                    expr.clone()
                }
            });
        }

        expand.entities.retain(|x| !matches!(x, EntityKind::Bind(_)));

        // Bind entities are now gone, we should update entities, loaded node expressions and rules with the map.

        for (ent, into) in intos.into_iter().enumerate() {
            expand.entities = expand.entities.map_entity(EntityId(ent), &into);
            build.loaded = HandleEntity::<ExprTypes<()>>::map_entity(build.loaded, EntityId(ent), &into);
            rules = rules.map_entity(EntityId(ent), &into);
        }

        // Normalize all rules now
        for rule in rules.iter_mut().flatten() {
            rule.normalize();
        }
    }

    // After all optimizations are done, normalize the loaded expressions
    for loaded in &mut build.loaded {
        loaded.normalize();
    }

    // We can also finalize rules:
    let rules: Vec<_> = rules.into_iter().flatten().collect();

    // THE FOLDING STEP
    // First, all rules, entities and expressions have to be flattened.
    // Then, we perform similar expression elimination multiple times
    //      - We reindex the element
    //      - We put it into a hashmap
    //      - If the element repeats, we replace it and add an entry into the index map
    //      - Otherwise, we rejoice
    // When the process ends NO REORGANIZATION HAPPENS AS NONE IS NECESSARY
    // Rules and entities are updated using the obtained index map.
    let mut context = FlattenContext::default();

    let mut entities = expand.entities.flatten(&mut context);
    let mut rules = rules.flatten(&mut context);
    let mut variables = context.loaded;
    let mut fig_variables = variables.clone();

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
            meta: counter.next().unwrap()
        })
        .collect();

    Ok(Intermediate {
        adjusted: Adjusted {
            variables,
            rules
        },
        figure: Figure {
            entities,
            variables: fig_variables,
            items
        },
        flags: read_flags(&unrolled.flags)
    })
}