/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

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

use geo_aid_derive::Definition;
use paste::paste;
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use std::{collections::HashMap, fmt::Debug};

use crate::generator::fast_float::FastFloat;
use crate::script::builtins::macros::number;
use crate::script::unroll::{AnyExpr, CloneWithNode, Simplify};
use crate::script::{unit, ComplexUnit, Error};
use crate::span;

use super::figure::FromExpr;
use super::{
    Circle as UnrolledCircle, CollectionNode, Displayed, Expr, FlagSet, HierarchyNode,
    Line as UnrolledLine, Node, Point as UnrolledPoint, Properties, Scalar as UnrolledScalar,
    ScalarData, UnrolledRule, UnrolledRuleKind,
};

/// For everything that can have an order (how modifiable the entity is).
pub trait Definition {
    /// Get the complexity order (how much adjustment is done to this entity).
    fn order(&self, context: &CompileContext) -> usize;

    /// Check if the object contains an entity
    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool;
}

/// A scalar is either a bind or a free real value.
#[derive(Debug, CloneWithNode)]
pub enum Scalar {
    /// A free, adjusted real.
    Free,
    /// A bind
    Bind(Expr<UnrolledScalar>),
}

impl Definition for Scalar {
    fn order(&self, context: &CompileContext) -> usize {
        match self {
            Self::Free => 1,
            Self::Bind(expr) => expr.order(context),
        }
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        match self {
            Self::Free => false,
            Self::Bind(expr) => expr.contains_entity(entity, context),
        }
    }
}

/// A point is either a bind or a free complex value.
#[derive(Debug, CloneWithNode, Definition)]
pub enum Point {
    /// A free, adjusted complex.
    #[def(order(2))]
    Free,
    /// A single-value clip.
    OnCircle(Expr<UnrolledCircle>),
    /// A single-value clip.
    OnLine(Expr<UnrolledLine>),
    /// A bind
    Bind(Expr<UnrolledPoint>),
}

/// A line is always a bind.
#[derive(Debug, CloneWithNode, Definition)]
pub enum Line {
    /// A bind
    Bind(Expr<UnrolledLine>),
}

/// A circle is always a bind.
#[derive(Debug, CloneWithNode, Definition)]
pub enum Circle {
    /// A bind
    Bind(Expr<UnrolledCircle>),
    /// A never compiled temporary value
    #[def(order(usize::MAX))]
    Temporary,
}

/// An entity is a single primitive on the figure plane.
#[derive(Debug, CloneWithNode)]
pub enum Entity {
    /// A scalar
    Scalar(Scalar),
    Point(Point),
    Line(Line),
    Circle(Circle),
}

impl Entity {
    #[must_use]
    pub fn free_point() -> Self {
        Self::Point(Point::Free)
    }

    #[must_use]
    pub fn free_scalar() -> Self {
        Self::Scalar(Scalar::Free)
    }

    #[must_use]
    pub fn free_circle() -> Self {
        Self::Circle(Circle::Temporary)
    }

    #[must_use]
    pub fn as_point(&self) -> Option<&Point> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_point_mut(&mut self) -> Option<&mut Point> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Definition for Entity {
    fn order(&self, context: &CompileContext) -> usize {
        match self {
            Self::Scalar(v) => v.order(context),
            Self::Point(v) => v.order(context),
            Self::Line(v) => v.order(context),
            Self::Circle(v) => v.order(context),
        }
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        match self {
            Self::Scalar(v) => v.contains_entity(entity, context),
            Self::Point(v) => v.contains_entity(entity, context),
            Self::Line(v) => v.contains_entity(entity, context),
            Self::Circle(v) => v.contains_entity(entity, context),
        }
    }
}

/// The context of compilation process.
#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct CompileContext {
    /// Variables
    pub variables: HashMap<String, AnyExpr>, // We have to store variables in this form to prevent type errors.
    /// Flags
    pub flags: FlagSet,
    /// Entities (primitives).
    pub entities: Vec<Entity>,
    /// Unrolled rules
    pub rules: RefCell<Vec<UnrolledRule>>,
    /// Errors collected.
    errors: RefCell<Vec<Error>>,
}

impl Default for CompileContext {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileContext {
    #[must_use]
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            flags: FlagSet::default(),
            entities: vec![],
            rules: RefCell::new(Vec::new()),
            errors: RefCell::new(Vec::new()),
        }
    }

    pub fn push_error(&self, err: Error) {
        self.errors.borrow_mut().push(err);
    }

    pub fn ok<T>(&self, res: Result<T, Error>) -> Option<T> {
        match res {
            Ok(v) => Some(v),
            Err(err) => {
                self.push_error(err);
                None
            }
        }
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        mem::take(&mut self.errors.borrow_mut())
    }

    pub fn extend_errors<I: IntoIterator<Item = Error>>(&self, iter: I) {
        self.errors.borrow_mut().extend(iter);
    }

    pub fn valid(&self) -> bool {
        self.errors.borrow().is_empty()
    }

    pub fn push_rule(&self, rule: UnrolledRule) {
        self.rules.borrow_mut().push(rule);
    }

    pub fn take_rules(&mut self) -> Vec<UnrolledRule> {
        mem::take(&mut self.rules.borrow_mut())
    }

    /// Gets the entity of the given index.
    #[must_use]
    pub fn get_entity(&self, i: usize) -> &Entity {
        &self.entities[i]
    }

    /// Gets the entity of the given index.
    #[must_use]
    pub fn get_entity_mut(&mut self, i: usize) -> &mut Entity {
        &mut self.entities[i]
    }

    #[must_use]
    pub fn add_scalar(&mut self) -> usize {
        self.entities.push(Entity::free_scalar());

        self.entities.len() - 1
    }

    #[must_use]
    pub fn add_point(&mut self) -> usize {
        self.entities.push(Entity::free_point());

        self.entities.len() - 1
    }

    /// # Panics
    /// Panics if given an invalid entity.
    #[must_use]
    pub fn get_point_by_index(&self, index: usize) -> Expr<UnrolledPoint> {
        let entity = self.entities.get(index).unwrap();
        if let Entity::Point(_) = entity {
            return Expr::new_spanless(UnrolledPoint::Entity(index));
        }

        panic!("Requested entity is not a point.");
    }

    #[must_use]
    pub fn get_point_entity_mut(&mut self, expr: &Expr<UnrolledPoint>) -> Option<&mut Point> {
        match expr.simplify(self).data.as_ref() {
            UnrolledPoint::Entity(i) => self.get_entity_mut(*i).as_point_mut(),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_point_entity(&self, expr: &Expr<UnrolledPoint>) -> Option<&Point> {
        match expr.simplify(self).data.as_ref() {
            UnrolledPoint::Entity(i) => self.get_entity(*i).as_point(),
            _ => None,
        }
    }

    pub fn add_circle(&mut self) -> usize {
        self.entities.push(Entity::free_circle());

        self.entities.len() - 1
    }

    /// # Panics
    /// Panics if given an invalid expression.
    #[must_use]
    pub fn get_circle_by_index(&self, index: usize) -> Expr<UnrolledCircle> {
        let entity = self.entities.get(index).unwrap();
        if let Entity::Circle(_) = entity {
            return Expr::new_spanless(UnrolledCircle::Entity(index));
        }

        panic!("Requested entity is not a circle.");
    }

    /// # Panics
    /// Panics if given an invalid expression.
    #[must_use]
    pub fn get_line_by_index(&self, index: usize) -> Expr<UnrolledLine> {
        let entity = self.entities.get(index).unwrap();
        if let Entity::Line(_) = entity {
            return Expr::new_spanless(UnrolledLine::Entity(index));
        }

        panic!("Requested entity is not a line.");
    }
}

/// Everything related to circles.
impl CompileContext {
    pub fn point_on_circle(&mut self, lhs: &Expr<UnrolledPoint>, rhs: &Expr<UnrolledCircle>) {
        if let Some(point) = self.get_point_entity_mut(lhs) {
            match point {
                Point::Free => {
                    *point = Point::OnCircle(rhs.clone_without_node());
                    return;
                }
                Point::OnCircle(_) | Point::OnLine(_) | Point::Bind(_) => (),
            }
        }

        self.scalar_eq(
            self.distance_pp(
                lhs.clone_without_node(),
                self.circle_center(rhs.clone_without_node()),
            ),
            self.circle_radius(rhs.clone_without_node()),
            false,
        );
    }

    pub fn point_on_line(&mut self, lhs: &Expr<UnrolledPoint>, rhs: &Expr<UnrolledLine>) {
        if let Some(point) = self.get_point_entity_mut(lhs) {
            match point {
                Point::Free => {
                    *point = Point::OnLine(rhs.clone_without_node());
                    return;
                }
                Point::OnLine(k) => {
                    *point = Point::Bind(Expr {
                        span: span!(0, 0, 0, 0),
                        weight: FastFloat::One,
                        node: None,
                        data: Rc::new(UnrolledPoint::LineLineIntersection(
                            k.clone_without_node(),
                            rhs.clone_without_node(),
                        )),
                    });
                    return;
                }
                Point::OnCircle(_) | Point::Bind(_) => (),
            }
        }

        self.scalar_eq(
            self.distance_pl(lhs.clone_without_node(), rhs.clone_without_node()),
            number!(=0.0),
            false,
        );
    }
}

#[macro_export]
macro_rules! take_nodes {
    ($($x:ident),* $(,)?) => {
        {
            let mut nodes = Vec::new();
            take_nodes!{nodes << $($x),*}
            nodes
        }
    };
    ($nodes:ident << $v:ident, $($x:ident),*) => {
        $nodes.extend($v.take_node().map(|node| Box::new(node) as Box<dyn $crate::script::unroll::Node>));
        take_nodes!{$nodes << $($x),*}
    };
    ($nodes:ident << $v:ident) => {
        $nodes.extend($v.take_node().map(|node| Box::new(node) as Box<dyn $crate::script::unroll::Node>));
    }
}

macro_rules! generic_expr {
    {$f:ident($v0:ident : $t0:ident, $($v:ident : $t:ident),* $(,)?) -> Scalar[inferred] :: $k:ident} => {
        paste! {
            pub fn [<$f _display>](&self, mut $v0: Expr<super::$t0>, $(mut $v: Expr<super::$t>),*, display: Properties) -> Expr<super::Scalar> {
                let nodes = take_nodes!($v0, $($v),*);
                self.expr_with(super::Scalar {
                    unit: $v0.data.unit,
                    data: super::ScalarData::$k($v0, $($v),*)
                }, display, nodes)
            }

            pub fn $f(&self, $v0: Expr<super::$t0>, $($v: Expr<super::$t>),*) -> Expr<super::Scalar> {
                self.[<$f _display>]($v0, $($v),*, Properties::default())
            }
        }
    };
    {$f:ident($($v:ident : $t:ident),* $(,)?) -> Scalar[$unit:expr] :: $k:ident} => {
        paste! {
            pub fn [<$f _display>](&self, $(mut $v: Expr<super::$t>),*, display: Properties) -> Expr<super::Scalar> {
                let nodes = take_nodes!($($v),*);
                self.expr_with(super::Scalar {
                    unit: Some($unit),
                    data: super::ScalarData::$k($($v),*)
                }, display, nodes)
            }

            pub fn $f(&self, $($v: Expr<super::$t>),*) -> Expr<super::Scalar> {
                self.[<$f _display>]($($v),*, Properties::default())
            }
        }
    };
    {$f:ident($($v:ident : $t:ident),* $(,)?) -> $r:ident :: $k:ident} => {
        paste! {
            pub fn [<$f _display>](&self, $(mut $v: Expr<super::$t>),*, display: Properties) -> Expr<super::$r> {
                let nodes = take_nodes!($($v),*);
                self.expr_with(super::$r::$k($($v),*), display, nodes)
            }

            pub fn $f(&self, $($v: Expr<super::$t>),*) -> Expr<super::$r> {
                self.[<$f _display>]($($v),*, Properties::default())
            }
        }
    }
}

// Expression constructors
impl CompileContext {
    pub fn expr_with<T: Displayed>(
        &self,
        content: T,
        display: Properties,
        nodes: Vec<Box<dyn Node>>,
    ) -> Expr<T>
    where
        T::Node: FromExpr<T>,
    {
        let mut expr = Expr {
            weight: FastFloat::One,
            span: span!(0, 0, 0, 0),
            data: Rc::new(content),
            node: None,
        };

        let mut node = HierarchyNode::new(T::Node::from_expr(&expr, display, self));

        node.extend_boxed(nodes);

        expr.node = Some(node);
        expr
    }

    pub fn average_p(&self, mut points: Vec<Expr<UnrolledPoint>>) -> Expr<UnrolledPoint> {
        let nodes = points
            .iter_mut()
            .flat_map(|v| v.take_node().map(|v| Box::new(v) as Box<dyn Node>))
            .collect();

        self.expr_with(
            UnrolledPoint::Average(points.into()),
            Properties::from(None),
            nodes,
        )
    }

    pub fn average_s(&self, mut values: Vec<Expr<UnrolledScalar>>) -> Expr<UnrolledScalar> {
        let nodes = values
            .iter_mut()
            .flat_map(|v| v.take_node().map(|v| Box::new(v) as Box<dyn Node>))
            .collect();

        self.expr_with(
            UnrolledScalar {
                unit: values[0].data.unit,
                data: ScalarData::Average(values.into()),
            },
            Properties::from(None),
            nodes,
        )
    }

    pub fn entity_p(&self, index: usize) -> Expr<UnrolledPoint> {
        self.expr_with(
            UnrolledPoint::Entity(index),
            Properties::from(None),
            Vec::new(),
        )
    }

    pub fn entity_s(&self, index: usize, unit: ComplexUnit) -> Expr<UnrolledScalar> {
        self.expr_with(
            UnrolledScalar {
                unit: Some(unit),
                data: ScalarData::Entity(index),
            },
            Properties::from(None),
            Vec::new(),
        )
    }

    pub fn set_unit(&self, mut v: Expr<UnrolledScalar>, unit: ComplexUnit) -> Expr<UnrolledScalar> {
        let node = v.take_node();
        self.expr_with(
            UnrolledScalar {
                unit: Some(unit),
                data: ScalarData::SetUnit(v, unit),
            },
            Properties::from(None),
            node.into_iter()
                .map(|x| Box::new(x) as Box<dyn Node>)
                .collect(),
        )
    }

    generic_expr! {intersection(k: Line, l: Line) -> Point::LineLineIntersection}
    generic_expr! {distance_pp(p: Point, q: Point) -> Scalar[unit::DISTANCE]::PointPointDistance}
    generic_expr! {distance_pl(p: Point, k: Line) -> Scalar[unit::DISTANCE]::PointLineDistance}
    generic_expr! {circle_center(c: Circle) -> Point::CircleCenter}
    generic_expr! {circle_radius(c: Circle) -> Scalar[unit::DISTANCE]::CircleRadius}
    generic_expr! {line(a: Point, b: Point) -> Line::LineFromPoints}
    generic_expr! {angle_ppp(a: Point, b: Point, c: Point) -> Scalar[unit::ANGLE]::ThreePointAngle}
    generic_expr! {angle_dir(a: Point, b: Point, c: Point) -> Scalar[unit::ANGLE]::ThreePointAngleDir}
    generic_expr! {angle_ll(k: Line, l: Line) -> Scalar[unit::ANGLE]::TwoLineAngle}
    generic_expr! {bisector_ppp(a: Point, b: Point, c: Point) -> Line::AngleBisector}
    generic_expr! {perpendicular_through(line: Line, point: Point) -> Line::PerpendicularThrough}
    generic_expr! {parallel_through(line: Line, point: Point) -> Line::ParallelThrough}
    generic_expr! {circle(center: Point, radius: Scalar) -> Circle::Circle}
    generic_expr! {add(a: Scalar, b: Scalar) -> Scalar[inferred]::Add}
    generic_expr! {sub(a: Scalar, b: Scalar) -> Scalar[inferred]::Subtract}
    generic_expr! {mult(a: Scalar, b: Scalar) -> Scalar[inferred]::Multiply}
    generic_expr! {div(a: Scalar, b: Scalar) -> Scalar[inferred]::Divide}
}

macro_rules! generic_rule {
    ($f:ident($lhs:ident, $rhs:ident) -> $r:ident) => {
        pub fn $f(
            &mut self,
            mut lhs: Expr<super::$lhs>,
            mut rhs: Expr<super::$rhs>,
            inverted: bool,
        ) {
            let (lhs_node, rhs_node) = (lhs.take_node(), rhs.take_node());
            self.rule_with(UnrolledRuleKind::$r(lhs, rhs), lhs_node, rhs_node, inverted);
        }
    };
}

// Rule constructors
impl CompileContext {
    fn rule_with<N: Node + 'static, M: Node + 'static>(
        &mut self,
        kind: UnrolledRuleKind,
        lhs: Option<N>,
        rhs: Option<M>,
        inverted: bool,
    ) {
        let mut node = CollectionNode::new();

        node.extend(lhs);
        node.extend(rhs);

        self.push_rule(UnrolledRule {
            kind,
            inverted,
            node: Some(Box::new(node)),
        })
    }

    pub fn display<N: Node + 'static>(&mut self, node: N) {
        self.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::Display,
            inverted: false,
            node: Some(Box::new(node)),
        })
    }

    generic_rule! {scalar_eq(Scalar, Scalar) -> ScalarEq}
    generic_rule! {point_eq(Point, Point) -> PointEq}
    generic_rule! {gt(Scalar, Scalar) -> Gt}
    generic_rule! {lt(Scalar, Scalar) -> Lt}
}
