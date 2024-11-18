//! Math stage is responsible for most if not all of the optimizations the compiler does.
//! It's at this point where rules are analyzed, expressions normalized, patterns that
//! can be optimized optimized. It's the final and most important stage of compilation.

use crate::figure::Item;
use crate::math::optimizations::ZeroLineDst;
use crate::token::number::{CompExponent, ProcNum};
use crate::unroll::figure::Node;
use crate::unroll::flags::Flag;
use derive_recursive::Recursive;
use geo_aid_figure::{EntityIndex as EntityId, VarIndex};
use num_traits::{FromPrimitive, One, Zero};
use serde::Serialize;
use std::any::Any;
use std::cell::OnceCell;
use std::cmp::Ordering;
use std::collections::{hash_map, HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::iter::Peekable;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use self::optimizations::{EqExpressions, EqPointDst, RightAngle};

use super::unroll::GetData;
use super::{
    figure::Figure,
    unroll::{
        self, Circle as UnrolledCircle, Displayed, Expr as Unrolled, Line as UnrolledLine,
        NumberData as UnrolledNumber, Point as UnrolledPoint, UnrolledRule, UnrolledRuleKind,
    },
    ComplexUnit, Error, SimpleUnit,
};

mod optimizations;

/// The `optimizations` flag group. Currently empty.
/// Has nothing to do with the [`optimizations`] module.
#[derive(Debug, Clone)]
pub struct Optimizations {}

/// Compiler flags.
#[derive(Debug, Clone)]
pub struct Flags {
    /// The `optimizations` flag group.
    pub optimizations: Optimizations,
    /// Whether to include point inequalitiy rules.
    pub point_inequalities: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Self {
            optimizations: Optimizations {},
            point_inequalities: false,
        }
    }
}

/// Helper trait for getting the type of the expression.
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

impl GetMathType for unroll::Number {
    fn get_math_type() -> ExprType {
        ExprType::Number
    }
}

/// Deep clone a mathematical expression. This is different from `Clone` in that
/// it works with the math stage's flattened memory model and deep clones all of
/// an expression's dependencies (subexpressions).
pub trait DeepClone {
    /// Perform a deep clone.
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

/// Compare two things using the math context.
trait Compare {
    /// Compare two things.
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

/// Check if an expression contains an entity.
trait ContainsEntity {
    /// Checks if an expression contains the specified entity.
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

/// Defines how the reconstruction process should affect a given entity.
#[derive(Debug, Clone, PartialEq, Eq)]
enum EntityBehavior {
    /// Map this id to another id.
    MapEntity(EntityId),
    /// Map this id to another expression (deep clones the expression)
    MapVar(VarIndex),
}

/// A context for the recosntruction process.
pub struct ReconstructCtx<'r> {
    /// Beheaviors of each entity
    entity_replacement: &'r [EntityBehavior],
    /// Old expressions
    old_vars: &'r [Expr<()>],
    /// New, reconstructed expressions.
    new_vars: Vec<Expr<()>>,
    /// Old entities.
    old_entities: &'r [EntityKind],
    /// New, reconstructed entities.
    new_entities: Vec<Option<EntityKind>>,
}

impl<'r> ReconstructCtx<'r> {
    /// Create a new context
    #[must_use]
    fn new(
        entity_replacement: &'r [EntityBehavior],
        old_vars: &'r [Expr<()>],
        old_entities: &'r [EntityKind],
    ) -> Self {
        let mut ctx = Self {
            entity_replacement,
            old_vars,
            new_vars: Vec::new(),
            old_entities,
            new_entities: vec![None; old_entities.len()],
        };

        for i in 0..old_entities.len() {
            ctx.reconstruct_entity(EntityId(i));
        }

        ctx
    }

    /// Reconstruct an entity.
    fn reconstruct_entity(&mut self, id: EntityId) {
        if self.new_entities[id.0].is_none() {
            self.new_entities[id.0] = Some(self.old_entities[id.0].clone().reconstruct(self));
        }
    }
}

/// The main actor in reconstruction process.
///
/// The point of a reconstruction is to remove all potential forward references
/// for the sake of later processing. This means that if a reconstructed expression
/// requires some other expressions to be computed before it can be computed itself,
/// those expressions will have a smaller index in the expression vector.
pub trait Reconstruct {
    /// Reconstruct the value.
    #[must_use]
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self;
}

/// Helper for reconstructing entities.
fn reconstruct_entity(entity_id: EntityId, ctx: &mut ReconstructCtx) -> ExprKind {
    match &ctx.entity_replacement[entity_id.0] {
        EntityBehavior::MapEntity(id) => {
            ctx.reconstruct_entity(*id);
            ExprKind::Entity { id: *id }
        }
        EntityBehavior::MapVar(index) => ctx.old_vars[index.0].kind.clone().reconstruct(ctx),
    }
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

/// Helper for finding entities inside rules and expressions.
trait FindEntities {
    /// Find all entities in this expression based on the entities
    /// found in all previous expressions.
    fn find_entities(
        &self,
        previous: &[HashSet<EntityId>],
        entities: &[EntityKind],
    ) -> HashSet<EntityId>;
}

impl FindEntities for EntityId {
    fn find_entities(
        &self,
        previous: &[HashSet<EntityId>],
        entities: &[EntityKind],
    ) -> HashSet<EntityId> {
        entities[self.0].find_entities(previous, entities)
    }
}

impl FindEntities for Vec<VarIndex> {
    fn find_entities(
        &self,
        previous: &[HashSet<EntityId>],
        _entities: &[EntityKind],
    ) -> HashSet<EntityId> {
        self.iter()
            .flat_map(|x| previous[x.0].iter().copied())
            .collect()
    }
}

/// Helper trait for converting unrolled expressions into math expressions.
pub trait FromUnrolled<T: Displayed> {
    /// Load the unroll expression
    fn load(expr: &Unrolled<T>, math: &mut Expand) -> Self;
}

/// Helper trait for normalizing expressions.
trait Normalize {
    /// Normalization is a crucial step for further processing.
    /// Usually normalized expressions and rules have their parameters
    /// ordered. There can also be extra requirements on certain kinds
    /// but they are documented in their respective places.
    fn normalize(&mut self, math: &mut Math);
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
        let ty = math.at(self).ty;
        let expr = math.at(self).kind.clone().deep_clone(math);
        math.store(expr, ty)
    }
}

impl Compare for VarIndex {
    fn compare(&self, other: &Self, math: &Math) -> Ordering {
        math.at(self).kind.compare(&math.at(other).kind, math)
    }
}

impl Reindex for VarIndex {
    fn reindex(&mut self, map: &IndexMap) {
        self.0 = map.get(self.0);
    }
}

impl ContainsEntity for VarIndex {
    fn contains_entity(&self, entity: EntityId, math: &Math) -> bool {
        math.at(self).contains_entity(entity, math)
    }
}

/// The primitive type of an expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash, Serialize)]
pub enum ExprType {
    Number,
    #[default]
    Point,
    Line,
    Circle,
}

/// A mathematical expression with a flattened memory model.
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
    /// An entity referece.
    #[recursive(override_reconstruct = reconstruct_entity)]
    Entity { id: EntityId },

    // POINT
    /// An intersection of two lines: `k` and `l`
    LineLineIntersection { k: VarIndex, l: VarIndex },
    /// The arithmetic mean of given point expressions
    AveragePoint { items: Vec<VarIndex> },
    /// Center of a circle.
    CircleCenter { circle: VarIndex },
    /// Convert a complex number to a point (no-op)
    ComplexToPoint { number: VarIndex },

    // NUMBER
    /// Sum of numbers.
    ///
    /// A normalized sum must be sorted and must not contain other sums or negations.
    /// An aggregated constant, if any, must be at the end.
    Sum {
        /// Items to add.
        plus: Vec<VarIndex>,
        /// Items to subtract.
        minus: Vec<VarIndex>,
    },
    /// Product of numbers.
    ///
    /// A normalized product must be sorted and must not contain other products.
    /// An aggregated constant, if any, must be at the end.
    Product {
        /// Items to multiply by.
        times: Vec<VarIndex>,
        /// Items to divide by.
        by: Vec<VarIndex>,
    },
    /// A constant.
    Const { value: ProcNum },
    /// Raising a value to a power.
    Exponentiation {
        value: VarIndex,
        exponent: CompExponent,
    },
    /// A distance between two points.
    PointPointDistance { p: VarIndex, q: VarIndex },
    /// A distance of a point from a line.
    PointLineDistance { point: VarIndex, line: VarIndex },
    /// An angle defined by three points (arm, origin, arm)
    ThreePointAngle {
        /// Angle's one arm.
        p: VarIndex,
        /// Angle's origin
        q: VarIndex,
        /// Angle's other arm
        r: VarIndex,
    },
    /// A directed angle defined by three points (arm, origin, arm)
    ThreePointAngleDir {
        /// Angle's first arm.
        p: VarIndex,
        /// Angle's origin.
        q: VarIndex,
        /// Angle's second arm.
        r: VarIndex,
    },
    /// The angle described by two lines.
    TwoLineAngle { k: VarIndex, l: VarIndex },
    /// The real part of a point.
    PointX { point: VarIndex },
    /// The imaginary part of a point.
    PointY { point: VarIndex },
    /// Convert a point to a complex number (no-op)
    PointToComplex { point: VarIndex },
    /// Real part of a number
    Real { number: VarIndex },
    /// Imaginary part of a number
    Imaginary { number: VarIndex },
    /// Natural logarithm
    Log { number: VarIndex },
    /// Exponential function (e^this)
    Exp { number: VarIndex },
    /// Sine of an angle
    Sin { angle: VarIndex },
    /// Cosine of an angle
    Cos { angle: VarIndex },
    /// Arctan2 function
    Atan2 { y: VarIndex, x: VarIndex },
    /// Line's direction vector
    DirectionVector { line: VarIndex },

    // Line
    /// A line through two points.
    PointPoint { p: VarIndex, q: VarIndex },
    /// The angle bisector line.
    AngleBisector {
        p: VarIndex,
        q: VarIndex,
        r: VarIndex,
    },
    /// A line parallel to another `line` going through a `point`
    ParallelThrough { point: VarIndex, line: VarIndex },
    /// A line perpendicular to another `line` going through a `point`
    PerpendicularThrough { point: VarIndex, line: VarIndex },
    /// A line made from a point and a direction vector
    PointVector { point: VarIndex, vector: VarIndex },

    // Circle
    /// A circle constructed from its center and radius.
    ConstructCircle { center: VarIndex, radius: VarIndex },
}

impl ExprKind {
    /// Get the id of a variant. Used for comparison
    #[must_use]
    pub fn variant_id(&self) -> usize {
        match self {
            Self::Entity { .. } => 0,
            Self::Const { .. } => 1,
            Self::LineLineIntersection { .. } => 2,
            Self::AveragePoint { .. } => 3,
            Self::CircleCenter { .. } => 4,
            Self::Sum { .. } => 5,
            Self::Product { .. } => 6,
            Self::Exponentiation { .. } => 7,
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
            Self::PointToComplex { .. } => 20,
            Self::ComplexToPoint { .. } => 21,
            Self::Real { .. } => 22,
            Self::Imaginary { .. } => 23,
            Self::Sin { .. } => 24,
            Self::Cos { .. } => 25,
            Self::Atan2 { .. } => 26,
            Self::Log { .. } => 27,
            Self::Exp { .. } => 28,
            Self::DirectionVector { .. } => 29,
            Self::PointVector { .. } => 30,
        }
    }

    /// Compare two expressions.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn compare(&self, other: &Self, math: &Math) -> Ordering {
        self.variant_id()
            .cmp(&other.variant_id())
            .then_with(|| match (self, other) {
                (Self::Entity { id: self_id }, Self::Entity { id: other_id }) => {
                    self_id.cmp(other_id)
                }
                (
                    Self::LineLineIntersection {
                        k: self_a,
                        l: self_b,
                    },
                    Self::LineLineIntersection {
                        k: other_a,
                        l: other_b,
                    },
                )
                | (
                    Self::PointPointDistance {
                        p: self_a,
                        q: self_b,
                    },
                    Self::PointPointDistance {
                        p: other_a,
                        q: other_b,
                    },
                )
                | (
                    Self::PointLineDistance {
                        point: self_a,
                        line: self_b,
                    },
                    Self::PointLineDistance {
                        point: other_a,
                        line: other_b,
                    },
                )
                | (
                    Self::TwoLineAngle {
                        k: self_a,
                        l: self_b,
                    },
                    Self::TwoLineAngle {
                        k: other_a,
                        l: other_b,
                    },
                )
                | (
                    Self::PointPoint {
                        p: self_a,
                        q: self_b,
                    },
                    Self::PointPoint {
                        p: other_a,
                        q: other_b,
                    },
                )
                | (
                    Self::ParallelThrough {
                        point: self_a,
                        line: self_b,
                    },
                    Self::ParallelThrough {
                        point: other_a,
                        line: other_b,
                    },
                )
                | (
                    Self::PerpendicularThrough {
                        point: self_a,
                        line: self_b,
                    },
                    Self::PerpendicularThrough {
                        point: other_a,
                        line: other_b,
                    },
                )
                | (
                    Self::ConstructCircle {
                        center: self_a,
                        radius: self_b,
                    },
                    Self::ConstructCircle {
                        center: other_a,
                        radius: other_b,
                    },
                ) => self_a
                    .compare(other_a, math)
                    .then_with(|| self_b.compare(other_b, math)),
                (
                    Self::AveragePoint { items: self_items },
                    Self::AveragePoint { items: other_items },
                ) => self_items.compare(other_items, math),
                (Self::CircleCenter { circle: self_x }, Self::CircleCenter { circle: other_x })
                | (Self::PointX { point: self_x }, Self::PointX { point: other_x })
                | (Self::PointY { point: self_x }, Self::PointY { point: other_x }) => {
                    self_x.compare(other_x, math)
                }
                (
                    Self::Sum {
                        plus: self_v,
                        minus: self_u,
                    },
                    Self::Sum {
                        plus: other_v,
                        minus: other_u,
                    },
                )
                | (
                    Self::Product {
                        times: self_v,
                        by: self_u,
                    },
                    Self::Product {
                        times: other_v,
                        by: other_u,
                    },
                ) => self_v
                    .compare(other_v, math)
                    .then_with(|| self_u.compare(other_u, math)),
                (Self::Const { value: self_v }, Self::Const { value: other_v }) => {
                    self_v.cmp(other_v)
                }
                (
                    Self::Exponentiation {
                        value: self_v,
                        exponent: self_exp,
                    },
                    Self::Exponentiation {
                        value: other_v,
                        exponent: other_exp,
                    },
                ) => self_v
                    .compare(other_v, math)
                    .then_with(|| self_exp.cmp(other_exp)),
                (
                    Self::ThreePointAngle {
                        p: self_p,
                        q: self_q,
                        r: self_r,
                    },
                    Self::ThreePointAngle {
                        p: other_p,
                        q: other_q,
                        r: other_r,
                    },
                )
                | (
                    Self::ThreePointAngleDir {
                        p: self_p,
                        q: self_q,
                        r: self_r,
                    },
                    Self::ThreePointAngleDir {
                        p: other_p,
                        q: other_q,
                        r: other_r,
                    },
                )
                | (
                    Self::AngleBisector {
                        p: self_p,
                        q: self_q,
                        r: self_r,
                    },
                    Self::AngleBisector {
                        p: other_p,
                        q: other_q,
                        r: other_r,
                    },
                ) => self_p
                    .compare(other_p, math)
                    .then_with(|| self_q.compare(other_q, math))
                    .then_with(|| self_r.compare(other_r, math)),
                (_, _) => Ordering::Equal,
            })
    }

    /// Get the expression's type.
    #[must_use]
    pub fn get_type<M>(&self, expressions: &[Expr<M>], entities: &[Entity<M>]) -> ExprType {
        match self {
            Self::Entity { id } => entities[id.0].get_type(expressions, entities),
            Self::LineLineIntersection { .. }
            | Self::AveragePoint { .. }
            | Self::CircleCenter { .. }
            | Self::ComplexToPoint { .. } => ExprType::Point,
            Self::Sum { .. }
            | Self::Product { .. }
            | Self::Const { .. }
            | Self::Exponentiation { .. }
            | Self::PointPointDistance { .. }
            | Self::PointLineDistance { .. }
            | Self::ThreePointAngle { .. }
            | Self::ThreePointAngleDir { .. }
            | Self::TwoLineAngle { .. }
            | Self::Sin { .. }
            | Self::Cos { .. }
            | Self::Atan2 { .. }
            | Self::PointX { .. }
            | Self::PointY { .. }
            | Self::Real { .. }
            | Self::Imaginary { .. }
            | Self::Log { .. }
            | Self::Exp { .. }
            | Self::DirectionVector { .. }
            | Self::PointToComplex { .. } => ExprType::Number,
            Self::PointPoint { .. }
            | Self::AngleBisector { .. }
            | Self::ParallelThrough { .. }
            | Self::PerpendicularThrough { .. }
            | Self::PointVector { .. } => ExprType::Line,
            Self::ConstructCircle { .. } => ExprType::Circle,
        }
    }
}

impl From<ExprKind> for geo_aid_figure::ExpressionKind {
    fn from(value: ExprKind) -> Self {
        match value {
            ExprKind::Entity { id } => Self::Entity { id },
            ExprKind::LineLineIntersection { k, l } => Self::LineLineIntersection { k, l },
            ExprKind::AveragePoint { items } => Self::AveragePoint { items },
            ExprKind::CircleCenter { circle } => Self::CircleCenter { circle },
            ExprKind::ComplexToPoint { number } => Self::ComplexToPoint { number },
            ExprKind::Sum { plus, minus } => Self::Sum { plus, minus },
            ExprKind::Product { times, by } => Self::Product { times, by },
            ExprKind::Const { value } => Self::Const {
                value: value.to_complex().into(),
            },
            ExprKind::Exponentiation { value, exponent } => Self::Power {
                value,
                exponent: exponent.into(),
            },
            ExprKind::PointPointDistance { p, q } => Self::PointPointDistance { p, q },
            ExprKind::PointLineDistance { point, line } => Self::PointLineDistance { point, line },
            ExprKind::ThreePointAngle { p, q, r } => Self::ThreePointAngle { a: p, b: q, c: r },
            ExprKind::ThreePointAngleDir { p, q, r } => {
                Self::ThreePointAngleDir { a: p, b: q, c: r }
            }
            ExprKind::TwoLineAngle { k, l } => Self::TwoLineAngle { k, l },
            ExprKind::Sin { angle } => Self::Sin { angle },
            ExprKind::Cos { angle } => Self::Cos { angle },
            ExprKind::Atan2 { y, x } => Self::Atan2 { y, x },
            ExprKind::DirectionVector { line } => Self::DirectionVector { line },
            ExprKind::PointX { point } => Self::PointX { point },
            ExprKind::PointY { point } => Self::PointY { point },
            ExprKind::PointToComplex { point } => Self::PointToComplex { point },
            ExprKind::Real { number } => Self::Real { number },
            ExprKind::Imaginary { number } => Self::Imaginary { number },
            ExprKind::Log { number } => Self::Log { number },
            ExprKind::Exp { number } => Self::Exp { number },
            ExprKind::PointPoint { p, q } => Self::PointPointLine { p, q },
            ExprKind::PointVector { point, vector } => Self::PointVectorLine { point, vector },
            ExprKind::AngleBisector { p, q, r } => Self::AngleBisector { p, q, r },
            ExprKind::ParallelThrough { point, line } => Self::ParallelThrough { point, line },
            ExprKind::PerpendicularThrough { point, line } => {
                Self::PerpendicularThrough { point, line }
            }
            ExprKind::ConstructCircle { center, radius } => {
                Self::ConstructCircle { center, radius }
            }
        }
    }
}

impl FindEntities for ExprKind {
    fn find_entities(
        &self,
        previous: &[HashSet<EntityId>],
        entities: &[EntityKind],
    ) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::Entity { id } => {
                set.insert(*id);
                set.extend(id.find_entities(previous, entities));
            }
            Self::AveragePoint { items } => {
                set.extend(items.iter().flat_map(|x| previous[x.0].iter().copied()));
            }
            Self::CircleCenter { circle: x }
            | Self::PointX { point: x }
            | Self::PointY { point: x }
            | Self::Sin { angle: x }
            | Self::Cos { angle: x }
            | Self::Exponentiation { value: x, .. }
            | Self::PointToComplex { point: x }
            | Self::ComplexToPoint { number: x }
            | Self::Log { number: x }
            | Self::Exp { number: x }
            | Self::DirectionVector { line: x }
            | Self::Real { number: x }
            | Self::Imaginary { number: x } => {
                set.extend(previous[x.0].iter().copied());
            }
            Self::Sum {
                plus: v1,
                minus: v2,
            }
            | Self::Product { times: v1, by: v2 } => {
                set.extend(v1.iter().flat_map(|x| previous[x.0].iter().copied()));
                set.extend(v2.iter().flat_map(|x| previous[x.0].iter().copied()));
            }
            Self::PointPointDistance { p: a, q: b }
            | Self::PointLineDistance { point: a, line: b }
            | Self::TwoLineAngle { k: a, l: b }
            | Self::LineLineIntersection { k: a, l: b }
            | Self::ParallelThrough { point: a, line: b }
            | Self::PerpendicularThrough { point: a, line: b }
            | Self::PointPoint { p: a, q: b }
            | Self::Atan2 { y: a, x: b }
            | Self::PointVector {
                point: a,
                vector: b,
            }
            | Self::ConstructCircle {
                center: a,
                radius: b,
            } => {
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
                l: math.load(b),
            },
            UnrolledPoint::Average(exprs) => ExprKind::AveragePoint {
                items: exprs.iter().map(|x| math.load(x)).collect(),
            },
            UnrolledPoint::CircleCenter(circle) => match circle.get_data() {
                UnrolledCircle::Circle(center, _) => return math.load_no_store(center),
                UnrolledCircle::Generic(_) => unreachable!(),
            },
            UnrolledPoint::Free => ExprKind::Entity {
                id: math.add_point(),
            },
            UnrolledPoint::FromComplex(number) => ExprKind::ComplexToPoint {
                number: math.load(number),
            },
            UnrolledPoint::Generic(g) => {
                unreachable!("Expression shouldn't have reached math stage: {g}")
            }
        };

        kind.normalize(math);
        kind
    }
}

impl FromUnrolled<unroll::Number> for ExprKind {
    fn load(expr: &Unrolled<unroll::Number>, math: &mut Expand) -> Self {
        let mut kind = match &expr.get_data().data {
            UnrolledNumber::Add(a, b) => ExprKind::Sum {
                plus: vec![math.load(a), math.load(b)],
                minus: Vec::new(),
            },
            UnrolledNumber::Subtract(a, b) => ExprKind::Sum {
                plus: vec![math.load(a)],
                minus: vec![math.load(b)],
            },
            UnrolledNumber::Multiply(a, b) => ExprKind::Product {
                times: vec![math.load(a), math.load(b)],
                by: Vec::new(),
            },
            UnrolledNumber::Divide(a, b) => ExprKind::Product {
                times: vec![math.load(a)],
                by: vec![math.load(b)],
            },
            UnrolledNumber::Average(exprs) => {
                let times = ExprKind::Sum {
                    plus: exprs.iter().map(|x| math.load(x)).collect(),
                    minus: Vec::new(),
                };
                let by = ExprKind::Const {
                    value: ProcNum::from_usize(exprs.len()).unwrap(),
                };

                ExprKind::Product {
                    times: vec![math.store(times, ExprType::Number)],
                    by: vec![math.store(by, ExprType::Number)],
                }
            }
            UnrolledNumber::CircleRadius(circle) => match circle.get_data() {
                UnrolledCircle::Circle(_, radius) => math.load_no_store(radius),
                UnrolledCircle::Generic(_) => unreachable!(),
            },
            UnrolledNumber::Free => ExprKind::Entity {
                id: math.add_real(),
            },
            UnrolledNumber::Number(x) => {
                return fix_dst(ExprKind::Const { value: x.clone() }, expr.data.unit, math);
            }
            UnrolledNumber::DstLiteral(x) => ExprKind::Const { value: x.clone() },
            UnrolledNumber::SetUnit(x, unit) => {
                return fix_dst(math.load_no_store(x), Some(*unit), math)
            }
            UnrolledNumber::PointPointDistance(p, q) => ExprKind::PointPointDistance {
                p: math.load(p),
                q: math.load(q),
            },
            UnrolledNumber::PointLineDistance(point, line) => ExprKind::PointLineDistance {
                point: math.load(point),
                line: math.load(line),
            },
            UnrolledNumber::Negate(x) => ExprKind::Sum {
                plus: Vec::new(),
                minus: vec![math.load(x)],
            },
            UnrolledNumber::ThreePointAngle(p, q, r) => ExprKind::ThreePointAngle {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r),
            },
            UnrolledNumber::ThreePointAngleDir(p, q, r) => ExprKind::ThreePointAngleDir {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r),
            },
            UnrolledNumber::TwoLineAngle(k, l) => ExprKind::TwoLineAngle {
                k: math.load(k),
                l: math.load(l),
            },
            UnrolledNumber::Pow(base, exponent) => ExprKind::Exponentiation {
                value: math.load(base),
                exponent: *exponent,
            },
            UnrolledNumber::PointX(point) => ExprKind::PointX {
                point: math.load(point),
            },
            UnrolledNumber::PointY(point) => ExprKind::PointY {
                point: math.load(point),
            },
            UnrolledNumber::Sin(angle) => ExprKind::Sin {
                angle: math.load(angle),
            },
            UnrolledNumber::Cos(angle) => ExprKind::Cos {
                angle: math.load(angle),
            },
            UnrolledNumber::Atan2(y, x) => ExprKind::Atan2 {
                y: math.load(y),
                x: math.load(x),
            },
            UnrolledNumber::Direction(line) => ExprKind::DirectionVector {
                line: math.load(line),
            },
            UnrolledNumber::FromPoint(point) => ExprKind::PointToComplex {
                point: math.load(point),
            },
            UnrolledNumber::Real(number) => ExprKind::Real {
                number: math.load(number),
            },
            UnrolledNumber::Imaginary(number) => ExprKind::Imaginary {
                number: math.load(number),
            },
            UnrolledNumber::Log(number) => ExprKind::Log {
                number: math.load(number),
            },
            UnrolledNumber::Exp(number) => ExprKind::Exp {
                number: math.load(number),
            },
            UnrolledNumber::Generic(_) => unreachable!(),
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
                q: math.load(b),
            },
            UnrolledLine::AngleBisector(a, b, c) => Self::AngleBisector {
                p: math.load(a),
                q: math.load(b),
                r: math.load(c),
            },
            UnrolledLine::PerpendicularThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => Self::ParallelThrough {
                        point: math.load(p),
                        line: math.load(l),
                    },
                    UnrolledLine::ParallelThrough(l, _) => Self::PerpendicularThrough {
                        point: math.load(p),
                        line: math.load(l),
                    },
                    _ => Self::PerpendicularThrough {
                        point: math.load(p),
                        line: math.load(k),
                    },
                }
            }
            UnrolledLine::ParallelThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => Self::PerpendicularThrough {
                        point: math.load(p),
                        line: math.load(l),
                    },
                    UnrolledLine::ParallelThrough(l, _) => Self::ParallelThrough {
                        point: math.load(p),
                        line: math.load(l),
                    },
                    _ => Self::ParallelThrough {
                        point: math.load(p),
                        line: math.load(k),
                    },
                }
            }
            UnrolledLine::PointVector(point, vector) => Self::PointVector {
                point: math.load(point),
                vector: math.load(vector),
            },
            UnrolledLine::Generic(_) => unreachable!(),
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
                radius: math.load(radius),
            },
            UnrolledCircle::Generic(_) => unreachable!(),
        };

        kind.normalize(math);
        kind
    }
}

impl Normalize for ExprKind {
    fn normalize(&mut self, math: &mut Math) {
        let cmp_and_swap = |a: &mut VarIndex, b: &mut VarIndex| {
            if math.compare(a, b) == Ordering::Greater {
                mem::swap(a, b);
            }
        };
        let cmp = |a: &VarIndex, b: &VarIndex| math.compare(a, b);
        let mut new_self = None;

        match self {
            Self::CircleCenter { .. }
            | Self::PointLineDistance { .. }
            | Self::PointX { .. }
            | Self::PointY { .. }
            | Self::Sin { .. }
            | Self::Cos { .. }
            | Self::DirectionVector { .. }
            | Self::Atan2 { .. }
            | Self::Exponentiation { .. }
            | Self::ConstructCircle { .. }
            | Self::Const { .. }
            | Self::ThreePointAngleDir { .. } // DO NOT NORMALIZE DIRECTED ANGLES
            | Self::Entity { .. }
            | Self::ComplexToPoint { .. }
            | Self::PointToComplex { .. }
            | Self::Log { .. }
            | Self::Exp { .. }
            | Self::PointVector { .. }
            | Self::Real { .. }
            | Self::Imaginary { .. } => (),
            Self::LineLineIntersection { k: a, l: b }
            | Self::PointPoint { p: a, q: b }
            | Self::TwoLineAngle { k: a, l: b }
            | Self::AngleBisector { p: a, r: b, .. }
            | Self::ThreePointAngle { p: a, r: b, .. }
            | Self::PointPointDistance { p: a, q: b } => {
                cmp_and_swap(a, b);
            }
            Self::AveragePoint { items } => {
                items.sort_by(&cmp);
            }
            Self::Sum { plus, minus } => {
                normalize_sum(plus, minus, math);
                if plus.len() == 1 && minus.is_empty() {
                    new_self = Some(math.at(&plus[0]).kind.clone());
                }
            }
            Self::Product { times, by } => {
                normalize_product(times, by, math);
                if times.len() == 1 && by.is_empty() {
                    new_self = Some(math.at(&times[0]).kind.clone());
                }
            }
            Self::ParallelThrough { point, line } => {
                // This is technically a move, although ugly, so we clone.
                let point = point.clone();
                new_self = Some(match &math.at(line).kind {
                    Self::ParallelThrough { line, .. } => Self::ParallelThrough { point, line: line.clone() },
                    Self::PerpendicularThrough { line, .. } => Self::PerpendicularThrough { point, line: line.clone() },
                    _ => Self::ParallelThrough { point, line: line.clone() }
                });
            }
            Self::PerpendicularThrough { point, line } => {
                // This is technically a move, although ugly, so we clone.
                let point = point.clone();
                new_self = Some(match &math.at(line).kind {
                    Self::ParallelThrough { line, .. } => Self::PerpendicularThrough { point, line: line.clone() },
                    Self::PerpendicularThrough { line, .. } => Self::ParallelThrough { point, line: line.clone() },
                    _ => Self::PerpendicularThrough { point, line: line.clone() }
                });
            }
        }

        if let Some(new_self) = new_self {
            *self = new_self;
        }
    }
}

/// Distance units must have special treatment as they need to be represented by a special
/// entity. This function makes sure this entity is inserted and raised to the right power
/// corresponding to the unit.
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
                        math.store(
                            ExprKind::Exponentiation {
                                value: dst_var,
                                exponent: unit.0[SimpleUnit::Distance as usize],
                            },
                            ExprType::Number,
                        ),
                    ],
                    by: Vec::new(),
                }
            }
        }
    }
}

/// A utility iterator for merging two sorted iterators.
#[derive(Debug, Clone)]
pub struct Merge<T, I, J, F>
where
    I: Iterator<Item = T>,
    J: Iterator<Item = T>,
    F: FnMut(&T, &T) -> Ordering,
{
    i: Peekable<I>,
    j: Peekable<J>,
    f: F,
}

impl<T, I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> Ordering>
    Merge<T, I, J, F>
{
    /// Create a new merger.
    #[must_use]
    pub fn new<A: IntoIterator<IntoIter = I>, B: IntoIterator<IntoIter = J>>(
        a: A,
        b: B,
        f: F,
    ) -> Self {
        Self {
            i: a.into_iter().peekable(),
            j: b.into_iter().peekable(),
            f,
        }
    }

    /// Merge this iterator with another iterator.
    #[must_use]
    pub fn merge_with<It: IntoIterator<Item = T>>(
        self,
        other: It,
    ) -> Merge<T, Self, It::IntoIter, F>
    where
        F: Clone,
    {
        let f_cloned = self.f.clone();
        Merge::new(self, other, f_cloned)
    }
}

impl<T, F: FnMut(&T, &T) -> Ordering>
    Merge<T, std::option::IntoIter<T>, std::option::IntoIter<T>, F>
{
    /// Create an empty merge iterator.
    #[must_use]
    pub fn empty(f: F) -> Self {
        Self::new(None, None, f)
    }
}

impl<T, I: Iterator<Item = T>, J: Iterator<Item = T>, F: FnMut(&T, &T) -> Ordering> Iterator
    for Merge<T, I, J, F>
{
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

/// Normalize a sum. Specific normalization rules are given in [`ExprKind::Sum`] documentation.
fn normalize_sum(plus: &mut Vec<VarIndex>, minus: &mut Vec<VarIndex>, math: &mut Math) {
    let plus_v = mem::take(plus);
    let minus_v = mem::take(minus);

    let mut constant = ProcNum::zero();

    let mut plus_final = Vec::new();
    let mut minus_final = Vec::new();

    let cmp = |a: &VarIndex, b: &VarIndex| math.compare(a, b);

    for item in plus_v {
        match &math.at(&item).kind {
            ExprKind::Sum { plus, minus } => {
                // Yet again, we're technically moving, so we can clone
                plus_final = Merge::new(plus_final, plus.iter().cloned(), &cmp).collect();
                minus_final = Merge::new(minus_final, minus.iter().cloned(), &cmp).collect();
            }
            ExprKind::Const { value } => constant += value,
            _ => {
                plus_final = Merge::new(plus_final, Some(item), &cmp).collect();
            }
        }
    }

    for item in minus_v {
        match &math.at(&item).kind {
            ExprKind::Sum { plus, minus } => {
                // Yet again, we're technically moving, so we can clone
                plus_final = Merge::new(plus_final, minus.iter().cloned(), &cmp).collect();
                minus_final = Merge::new(minus_final, plus.iter().cloned(), &cmp).collect();
            }
            ExprKind::Const { value } => constant -= value,
            _ => {
                minus_final = Merge::new(minus_final, Some(item), &cmp).collect();
            }
        }
    }

    if !constant.is_zero() || (plus_final.is_empty() && minus_final.is_empty()) {
        plus_final.push(math.store(ExprKind::Const { value: constant }, ExprType::Number));
    }

    *plus = plus_final;
    *minus = minus_final;
}

/// Normalize a sum. Specific normalization rules are given in [`ExprKindi::Product`] documentation.
fn normalize_product(times: &mut Vec<VarIndex>, by: &mut Vec<VarIndex>, math: &mut Math) {
    let times_v = mem::take(times);
    let by_v = mem::take(by);

    let mut constant = ProcNum::one();

    let mut times_final = Vec::new();
    let mut by_final = Vec::new();

    let cmp = |a: &VarIndex, b: &VarIndex| math.compare(a, b);

    for item in times_v {
        match &math.at(&item).kind {
            ExprKind::Product { times, by } => {
                // Yet again, we're technically moving, so we can clone
                times_final = Merge::new(times_final, times.iter().cloned(), &cmp).collect();
                by_final = Merge::new(by_final, by.iter().cloned(), &cmp).collect();
            }
            ExprKind::Const { value } => constant *= value,
            _ => {
                times_final = Merge::new(times_final, Some(item), &cmp).collect();
            }
        }
    }

    for item in by_v {
        match &math.at(&item).kind {
            ExprKind::Product { times, by } => {
                // Yet again, we're technically moving, so we can clone
                times_final = Merge::new(times_final, by.iter().cloned(), &cmp).collect();
                by_final = Merge::new(by_final, times.iter().cloned(), &cmp).collect();
            }
            ExprKind::Const { value } => constant /= value,
            _ => {
                by_final = Merge::new(by_final, Some(item), &cmp).collect();
            }
        }
    }

    if !constant.is_one() || (times_final.is_empty() && by_final.is_empty()) {
        times_final.push(math.store(ExprKind::Const { value: constant }, ExprType::Number));
    }

    *times = times_final;
    *by = by_final;
}

/// An expression with some metadata and a type.
#[derive(Debug, Clone, PartialEq, Eq, Default, Hash, Serialize)]
pub struct Expr<M> {
    pub meta: M,
    pub kind: ExprKind,
    pub ty: ExprType,
}

impl<M> Expr<M> {
    /// Get the expression's type.
    #[must_use]
    pub fn get_type(&self, expressions: &[Expr<M>], entities: &[Entity<M>]) -> ExprType {
        self.kind.get_type(expressions, entities)
    }
}

impl<M> FindEntities for Expr<M> {
    fn find_entities(
        &self,
        previous: &[HashSet<EntityId>],
        entities: &[EntityKind],
    ) -> HashSet<EntityId> {
        self.kind.find_entities(previous, entities)
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
    /// Create a new expression with a kind and a type.
    #[must_use]
    pub fn new(kind: ExprKind, ty: ExprType) -> Self {
        Self { kind, meta: (), ty }
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
    /// Equality of two points (distance of 0)
    PointEq(VarIndex, VarIndex),
    /// Equality of two numbers
    NumberEq(VarIndex, VarIndex),
    /// a > b
    Gt(VarIndex, VarIndex),
    /// At least one of the rules must be satisfied
    Alternative(Vec<RuleKind>),
    /// The inverse of a rule
    Invert(Box<RuleKind>),
    /// A special bias rule for making the entity more stable in certain engines.
    Bias,
}

impl Display for RuleKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuleKind::PointEq(a, b) | RuleKind::NumberEq(a, b) => write!(f, "{a} = {b}"),
            RuleKind::Gt(a, b) => write!(f, "{a} > {b}"),
            RuleKind::Alternative(v) => {
                for kind in v {
                    write!(f, "| {kind}")?;
                }

                Ok(())
            }
            RuleKind::Invert(v) => write!(f, "not {v}"),
            RuleKind::Bias => write!(f, "bias"),
        }
    }
}

impl FindEntities for RuleKind {
    #[allow(clippy::only_used_in_recursion)]
    fn find_entities(
        &self,
        previous: &[HashSet<EntityId>],
        entities: &[EntityKind],
    ) -> HashSet<EntityId> {
        let mut set = HashSet::new();

        match self {
            Self::PointEq(a, b) | Self::NumberEq(a, b) | Self::Gt(a, b) => {
                set.extend(previous[a.0].iter().copied());
                set.extend(previous[b.0].iter().copied());
            }
            Self::Alternative(items) => {
                return items
                    .iter()
                    .flat_map(|x| x.find_entities(previous, entities).into_iter())
                    .collect();
            }
            Self::Invert(rule) => {
                return rule.find_entities(previous, entities);
            }
            Self::Bias => unreachable!(),
        }

        set
    }
}

/// A rule along if its aggregated information. Note that `entities`
/// is not filled until the final steps of compilation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rule {
    /// The kind of this rule
    pub kind: RuleKind,
    /// The rule's weight
    pub weight: ProcNum,
    /// Entities this rule affects.
    pub entities: Vec<EntityId>,
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
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
            Self::PointEq(a, b) | Self::NumberEq(a, b) | Self::Gt(a, b) => {
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
    /// Load a rule from an [`UnrolledRule`].
    ///
    /// # Returns
    /// A normalized rule.
    fn load(rule: &UnrolledRule, math: &mut Expand) -> Self {
        let mut mathed = match &rule.kind {
            UnrolledRuleKind::PointEq(a, b) => Self::PointEq(math.load(a), math.load(b)),
            UnrolledRuleKind::NumberEq(a, b) => Self::NumberEq(math.load(a), math.load(b)),
            UnrolledRuleKind::Gt(a, b) => Self::Gt(math.load(a), math.load(b)),
            UnrolledRuleKind::Alternative(rules) => {
                Self::Alternative(rules.iter().map(|x| Self::load(x, math)).collect())
            }
            UnrolledRuleKind::Bias(_) => Self::Bias,
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
    /// Load a rule from an [`UnrolledRule`].
    ///
    /// # Returns
    /// A normalized rule.
    fn load(rule: &UnrolledRule, math: &mut Expand) -> Self {
        Self {
            kind: RuleKind::load(rule, math),
            weight: rule.weight.clone(),
            entities: Vec::new(),
        }
    }
}

impl Normalize for RuleKind {
    fn normalize(&mut self, math: &mut Math) {
        match self {
            Self::PointEq(a, b) | Self::NumberEq(a, b) => {
                if math.compare(a, b) == Ordering::Greater {
                    mem::swap(a, b);
                }
            }
            Self::Alternative(v) => v.sort(),
            Self::Invert(_) | Self::Bias | Self::Gt(_, _) => (),
        }
    }
}

impl Normalize for Rule {
    fn normalize(&mut self, math: &mut Math) {
        self.kind.normalize(math);
    }
}

/// The adjusted (optimized) part of IR.
#[derive(Debug)]
pub struct Adjusted {
    /// Expressions needed for rules and entities.
    pub variables: Vec<Expr<()>>,
    /// Rules binding the figure
    pub rules: Vec<Rule>,
    /// Entities of the figure.
    pub entities: Vec<EntityKind>,
}

/// The full Math IR, the ultimate result of the entire compiler
#[derive(Debug)]
pub struct Intermediate {
    /// A figure IR.
    pub figure: Figure,
    /// The adjusted, later optimized part.
    pub adjusted: Adjusted,
    /// Compiler flags.
    pub flags: Flags,
}

/// An entity along with some metadata.
#[derive(Debug, Clone, Serialize)]
pub struct Entity<M> {
    pub kind: EntityKind,
    pub meta: M,
}

impl<M> Entity<M> {
    /// Get the entity's type.
    #[must_use]
    pub fn get_type(&self, expressions: &[Expr<M>], entities: &[Entity<M>]) -> ExprType {
        match &self.kind {
            EntityKind::FreePoint
            | EntityKind::PointOnLine { .. }
            | EntityKind::PointOnCircle { .. } => ExprType::Point,
            EntityKind::FreeReal | EntityKind::DistanceUnit => ExprType::Number,
            EntityKind::Bind(expr) => expressions[expr.0].get_type(expressions, entities),
        }
    }
}

/// The kind of an entity.
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
    /// A free point with two degrees of freedom.
    FreePoint,
    /// A point bound to a specific line, only one relative degree of freedom.
    PointOnLine { line: VarIndex },
    /// A point bound to a specific circle, only one relative degree of freedom.
    PointOnCircle { circle: VarIndex },
    /// A free real with one degree of freedom
    FreeReal,
    /// A special distance unit entity, effectively a free real.
    DistanceUnit,
    /// A bind. Never shows up past the compilation stage. It serves as a temporary
    /// value in-between compilation steps.
    Bind(VarIndex),
}

impl FindEntities for EntityKind {
    fn find_entities(
        &self,
        previous: &[HashSet<EntityId>],
        _entities: &[EntityKind],
    ) -> HashSet<EntityId> {
        match self {
            Self::PointOnLine { line: var } | Self::PointOnCircle { circle: var } => {
                previous[var.0].clone()
            }
            Self::FreePoint | Self::FreeReal | Self::DistanceUnit => HashSet::new(),
            Self::Bind(_) => unreachable!(),
        }
    }
}

impl From<EntityKind> for geo_aid_figure::EntityKind {
    fn from(value: EntityKind) -> Self {
        match value {
            EntityKind::FreePoint => Self::FreePoint,
            EntityKind::PointOnLine { line } => Self::PointOnLine { line },
            EntityKind::PointOnCircle { circle } => Self::PointOnCircle { circle },
            EntityKind::FreeReal => Self::FreeReal,
            EntityKind::DistanceUnit => Self::DistanceUnit,
            EntityKind::Bind(_) => unreachable!(),
        }
    }
}

impl Reindex for EntityKind {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Self::FreePoint | Self::DistanceUnit | Self::FreeReal => {}
            Self::PointOnLine { line } => line.reindex(map),
            Self::PointOnCircle { circle } => circle.reindex(map),
            Self::Bind(_) => unreachable!("Should not appear"),
        }
    }
}

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

/// A context struct for loading unrolled data.
#[derive(Debug, Clone, Default)]
pub struct Expand {
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, Expr<()>>,
    /// Processing context
    pub math: Math,
    /// Used to keep pointers with the data we need alive, so that no memory issues occur.
    /// Normally, an address add to `expr_map` as a key had the risk of expiring and a collision
    /// occurring. This way, this should be prevented. It will also increase memory usage, but shhh.
    /// It's an ugly solution, but it works. I'm most likely going to come back to this one with some
    /// new ideas for solving the issue.
    pub rc_keepalive: Vec<Rc<dyn Any>>,
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

/// A math stage context used across the whole compilation process.
#[derive(Debug, Clone, Default)]
pub struct Math {
    /// All figure's entities
    pub entities: Vec<EntityKind>,
    /// Distance unit, if exists.
    pub dst_var: OnceCell<EntityId>,
    /// Collected expressions in flattened layout.
    pub expr_record: Vec<Expr<()>>,
}

impl Expand {
    /// Load an unrolled expression. Also stores it in the variable record.
    pub fn load<T: Displayed + GetMathType + Debug + GetData + 'static>(
        &mut self,
        unrolled: &Unrolled<T>,
    ) -> VarIndex
    where
        ExprKind: FromUnrolled<T>,
    {
        let expr = self.load_no_store(unrolled);
        self.store(expr, T::get_math_type())
    }

    /// Load an unrolled expression without storing it.
    pub fn load_no_store<T: Displayed + GetMathType + GetData + Debug + 'static>(
        &mut self,
        unrolled: &Unrolled<T>,
    ) -> ExprKind
    where
        ExprKind: FromUnrolled<T>,
    {
        // Keep the smart pointer inside `unrolled` alive.
        self.rc_keepalive
            .push(Rc::clone(&unrolled.data) as Rc<dyn Any>);

        let key = std::ptr::from_ref(unrolled.get_data()) as usize;
        let loaded = self.expr_map.get(&key).cloned();

        if let Some(loaded) = loaded {
            loaded.kind.deep_clone(self)
        } else {
            // If expression has not been mathed yet, math it and put it into the record.
            let loaded = ExprKind::load(unrolled, self);
            self.expr_map
                .insert(key, Expr::new(loaded.clone(), T::get_math_type()));
            loaded
        }
    }
}

impl Math {
    /// Store an expression in the expression record.
    #[must_use]
    pub fn store(&mut self, expr: ExprKind, ty: ExprType) -> VarIndex {
        self.expr_record.push(Expr::new(expr, ty));
        VarIndex(self.expr_record.len() - 1)
    }

    /// Compare two expressions referenced by indices.
    #[must_use]
    pub fn compare(&self, a: &VarIndex, b: &VarIndex) -> Ordering {
        self.at(a).kind.compare(&self.at(b).kind, self)
    }

    /// Get the distance unit and generate it if it doesn't exist.
    ///
    /// # Panics
    /// Will never.
    #[must_use]
    pub fn get_dst_var(&mut self) -> VarIndex {
        let id = self.dst_var.get();
        let is_some = id.is_some();

        let id = *if is_some {
            id.unwrap()
        } else {
            let real = self.add_entity(EntityKind::DistanceUnit);
            self.dst_var.get_or_init(|| real)
        };

        self.store(ExprKind::Entity { id }, ExprType::Number)
    }

    /// Get the expression at given index.
    #[must_use]
    pub fn at(&self, index: &VarIndex) -> &Expr<()> {
        &self.expr_record[index.0]
    }

    /// Add an entity to the record.
    fn add_entity(&mut self, entity: EntityKind) -> EntityId {
        self.entities.push(entity);
        EntityId(self.entities.len() - 1)
    }

    /// Add a free point entity.
    pub fn add_point(&mut self) -> EntityId {
        self.add_entity(EntityKind::FreePoint)
    }

    /// Add a free real entity.
    pub fn add_real(&mut self) -> EntityId {
        self.add_entity(EntityKind::FreeReal)
    }
}

/// Used explicitly for figure IR building.
#[derive(Debug, Clone, Default)]
pub struct Build {
    /// A loading context for unrolled data.
    expand: Expand,
    /// Aggregated items to be drawn on the figure.
    items: Vec<Item>,
}

impl Build {
    /// Load an unrolled expression.
    pub fn load<T: Displayed + GetMathType + Debug + GetData + 'static>(
        &mut self,
        expr: &Unrolled<T>,
    ) -> VarIndex
    where
        ExprKind: FromUnrolled<T>,
    {
        self.expand.load(expr)
    }

    pub fn add<I: Into<Item>>(&mut self, item: I) {
        let item = item.into();
        self.items.push(item);
    }
}

/// Tries to transform the rules so that they are simpler to process for the generator.
///
/// # Returns
/// `true` if an optimization was performed. `false` otherwise.
fn optimize_rules(rules: &mut Vec<Option<Rule>>, math: &mut Math) -> bool {
    let mut performed = false;

    for rule in rules.iter_mut() {
        let rule_performed = ZeroLineDst::process(rule, math)
            | RightAngle::process(rule, math)
            | EqPointDst::process(rule, math)
            | EqExpressions::process(rule, math);

        performed |= rule_performed;
    }

    if performed {
        rules.retain(Option::is_some);
    }

    performed
}

/// Constructs a map between two sets of numbers A -> B.
#[derive(Debug, Clone, Default)]
pub struct IndexMap {
    /// Consecutive mappings. Not incredibly efficient, but simple and infallible.
    mappings: Vec<(usize, usize)>,
}

impl IndexMap {
    /// Create a new, identity map.
    #[must_use]
    pub fn new() -> Self {
        Self {
            mappings: Vec::new(),
        }
    }

    /// Get the value `a` maps to.
    #[must_use]
    pub fn get(&self, mut a: usize) -> usize {
        for m in &self.mappings {
            if a == m.0 {
                a = m.1;
            }
        }

        a
    }

    /// Creates a mapping from a to b (works like function composition).
    pub fn map(&mut self, a: usize, b: usize) {
        if a != b {
            self.mappings.push((a, b));
        }
    }

    /// Composes two index maps: any get call will be functionally equivalent to self.get(other.get(i)).
    pub fn compose(lhs: Self, rhs: &mut Self) {
        rhs.mappings.extend(lhs.mappings);
    }
}

/// Helper trait for reindexing process.
///
/// Reindexing is responsible for eliminating expression duplicates.
/// It must be performed on a structure with no forward references.
pub trait Reindex {
    /// Reindex the expression/rule according to the given map.
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

    loop {
        // println!("Folding...");
        let mut map = IndexMap::new();
        let mut folded = false;
        for (i, expr) in matrix.iter_mut().enumerate() {
            let mut expr = mem::take(expr);
            // print!("Found {expr:?}. Remapping to ");
            expr.reindex(&map);
            // println!("{expr:?}");
            match record.entry(expr) {
                hash_map::Entry::Vacant(entry) => {
                    target.push(entry.key().clone());
                    let new_i = target.len() - 1;
                    // println!("Not recorded, mapping {i} -> {new_i}");
                    map.map(i, new_i);
                    entry.insert(new_i);
                }
                hash_map::Entry::Occupied(entry) => {
                    // We have to update the index map. No push into target happens.
                    let j = *entry.get();
                    map.map(i, j);
                    // println!("Already recorded at {j}. Mapping {i} -> {j}");
                    folded = true;
                }
            }
        }
        // println!();
        // println!("We've build a map: {map:#?}");

        // We have to also build the final map.
        IndexMap::compose(map, &mut final_map);
        // println!("After composition, it became {final_map:#?}");

        // Swap target with matrix before next loop.
        mem::swap(matrix, &mut target);
        // And clear aux variables.
        target.clear();
        record.clear();

        if !folded {
            break final_map;
        }
    }
}

fn read_flags(flags: &HashMap<&'static str, Flag>) -> Flags {
    Flags {
        optimizations: Optimizations {},
        point_inequalities: flags["point_inequalities"].as_bool().unwrap(),
    }
}

/// Optimize, Normalize, Repeat
fn optimize_cycle(rules: &mut Vec<Option<Rule>>, math: &mut Math, items: &mut Vec<Item>) {
    let mut entity_map = Vec::new();
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
                | EntityKind::FreeReal
                | EntityKind::DistanceUnit
                | EntityKind::PointOnCircle { .. }
                | EntityKind::PointOnLine { .. } => EntityBehavior::MapEntity(EntityId(i - offset)),
                EntityKind::Bind(expr) => {
                    offset += 1;
                    EntityBehavior::MapVar(expr.clone()) // Technically moving
                }
            });
        }

        entities.retain(|x| !matches!(x, EntityKind::Bind(_)));

        // The entities are now corrected, but the rules, entities (sic) and variables don't know that.
        // The easiest way to fix it is to reconstruct the loaded variable vector recursively.
        // This way, we can also fix forward referencing and remove potentially unused expressions.
        // We'll also have to update items. Otherwise, they will have misguided indices.

        let old_vars = mem::take(&mut math.expr_record);
        let mut ctx = ReconstructCtx::new(&entity_map, &old_vars, &entities);
        let old_items = mem::take(items);
        *items = old_items.reconstruct(&mut ctx);
        let old_rules = mem::take(rules);
        *rules = old_rules.reconstruct(&mut ctx);
        math.expr_record = ctx.new_vars;
        math.entities = ctx.new_entities.into_iter().map(Option::unwrap).collect();

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

/// Loads a `GeoScript` script and compiles it into Math IR. Encapsulates the entire compiler's
/// work.
///
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

    // for rule in unrolled.rules.borrow().iter() {
    //     println!("{rule}");
    // }

    // Expand & normalize figure
    let mut build = Build::default();
    Box::new(nodes).build(&mut build);

    // Move expand base
    let mut expand = build.expand;

    // for (i, v) in expand.expr_record.iter().enumerate() {
    //     println!("[{i}] = {:?}", v.kind);
    // }
    //
    // println!("{:#?}", build.items);

    // Expand & normalize rules
    let mut rules = Vec::new();

    for rule in unrolled.take_rules() {
        rules.push(Some(Rule::load(&rule, &mut expand)));
    }

    // for (i, ent) in expand.entities.iter().enumerate() {
    //     println!("[{i}] = {ent:?}");
    // }

    // Get the math out of the `Expand`.
    let mut math = expand.math;

    optimize_cycle(&mut rules, &mut math, &mut build.items);

    // Now everything that could be normalized is normalized.
    // Unfortunately, normalization can introduce forward referencing, which is not what we want.
    // This means we have to fix it. And the easiest way to fix it is to reconstruct it once more.

    let old_entities = mem::take(&mut math.entities);
    let entity_map: Vec<_> = (0..old_entities.len())
        .map(|i| EntityBehavior::MapEntity(EntityId(i)))
        .collect();

    let old_vars = mem::take(&mut math.expr_record);
    let mut ctx = ReconstructCtx::new(&entity_map, &old_vars, &old_entities);
    build.items = build.items.reconstruct(&mut ctx);
    rules = rules.reconstruct(&mut ctx);
    math.expr_record = ctx.new_vars;
    let new_entities: Vec<_> = ctx.new_entities.into_iter().map(Option::unwrap).collect();

    // for (i, ent) in new_entities.iter().enumerate() {
    //     println!("[{i}] = {ent:?}");
    // }
    //
    // for (i, var) in math.expr_record.iter().enumerate() {
    //     println!("[{i}] = {:?}", var.kind);
    // }

    // We can also finalize rules:
    let mut rules: Vec<_> = rules.into_iter().flatten().collect();

    let flags = read_flags(&unrolled.flags);

    // And add point inequalities
    if flags.point_inequalities {
        for i in new_entities
            .iter()
            .enumerate()
            .filter(|ent| {
                matches!(
                    ent.1,
                    EntityKind::PointOnLine { .. }
                        | EntityKind::FreePoint
                        | EntityKind::PointOnCircle { .. }
                )
            })
            .map(|x| x.0)
        {
            for j in new_entities
                .iter()
                .enumerate()
                .skip(i + 1)
                .filter(|ent| {
                    matches!(
                        ent.1,
                        EntityKind::PointOnLine { .. }
                            | EntityKind::FreePoint
                            | EntityKind::PointOnCircle { .. }
                    )
                })
                .map(|x| x.0)
            {
                let ent1 = math.store(ExprKind::Entity { id: EntityId(i) }, ExprType::Point);
                let ent2 = math.store(ExprKind::Entity { id: EntityId(j) }, ExprType::Point);
                rules.push(Rule {
                    weight: ProcNum::one(),
                    entities: Vec::new(),
                    kind: RuleKind::Invert(Box::new(RuleKind::PointEq(ent1, ent2))),
                });
            }
        }
    }

    math.entities = new_entities;

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
    let mut fig_entities = entities.clone();

    let index_map = fold(&mut variables);
    entities.reindex(&index_map);
    rules.reindex(&index_map);

    // Find entities affected by specific rules
    let mut found_entities = Vec::new();
    for expr in &variables {
        let found = expr.find_entities(&found_entities, &entities);
        found_entities.push(found);
    }

    for rule in &mut rules {
        let entities = rule.kind.find_entities(&found_entities, &entities);
        rule.entities = entities.into_iter().collect();
    }

    // Fold figure variables
    // println!("PRE-FOLD");
    //
    // for (i, v) in fig_variables.iter().enumerate() {
    //     println!("[{i}] = {:?}", v.kind);
    // }

    // println!("{:#?}", build.items);

    let index_map = fold(&mut fig_variables);
    // println!("POST-FOLD");

    // println!("{index_map:#?}");

    // for (i, v) in fig_variables.iter().enumerate() {
    //     println!("[{i}] = {:?}", v.kind);
    // }
    let mut items = build.items;
    items.reindex(&index_map);
    fig_entities.reindex(&index_map);

    // for (i, v) in fig_variables.iter().enumerate() {
    //     println!("[{i}] = {:?}", v.kind);
    // }
    //
    // for rule in &rules {
    //     println!("\n{:?}", rule.kind);
    // }

    Ok(Intermediate {
        adjusted: Adjusted {
            variables,
            rules,
            entities,
        },
        figure: Figure {
            entities: fig_entities,
            variables: fig_variables,
            items,
        },
        flags,
    })
}
