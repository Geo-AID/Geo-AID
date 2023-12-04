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
use std::cell::RefCell;
use std::rc::Rc;
use std::{collections::HashMap, fmt::Debug};

use crate::generator::fast_float::FastFloat;
use crate::script::{Error, unit};
use crate::script::builtins::macros::{intersection, number};
use crate::script::unroll::{AnyExpr, Simplify, CloneWithNode};
use crate::script::builtins::macros::{circle_center, circle_radius, distance, rule};
use crate::span;

use super::{
    Circle as UnrolledCircle, Expr, FlagSet, FlagSetConstructor, Line as UnrolledLine,
    Point as UnrolledPoint, Scalar as UnrolledScalar, UnrolledRule, Displayed, HierarchyNode, Node, Properties, CollectionNode, UnrolledRuleKind, ScalarData
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
    pub rules: Vec<UnrolledRule>,
    /// Errors collected.
    errors: RefCell<Vec<Error>>
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
            flags: FlagSetConstructor::new()
                .add_set(
                    &"optimizations",
                    FlagSetConstructor::new().add_bool_def(&"identical_expressions", true),
                )
                .add_bool_def(&"point_bounds", false)
                .finish(),
            entities: vec![],
            rules: Vec::new(),
            errors: RefCell::new(Vec::new())
        }
    }

    pub fn push_error(&self, err: Error) {
        self.errors.borrow_mut().push(err);
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
            self.distance_pp(lhs.clone_without_node(), circle_center!(rhs.clone_without_node())),
            circle_radius!(rhs.clone_without_node())
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
                    *point = Point::Bind(self.intersection(k.clone_without_node(), rhs.clone_without_node()));
                    return;
                }
                Point::OnCircle(_) | Point::Bind(_) => (),
            }
        }

        self.scalar_eq(
            self.distance_pl(lhs.clone_without_node(), rhs.clone_without_node()),
            number!(=0.0)
        );
    }
}

macro_rules! take_nodes {
    ($($x:ident),* $(,)?) => {
        {
            let mut nodes = Vec::new();
            take_nodes!{nodes << $($x),*}
            nodes
        }
    };
    ($nodes:ident << $v:ident, $($x:ident),*) => {
        $nodes.extend($v.take_node().map(|node| Box::new(node) as Box<dyn Node>));
        take_nodes!{$nodes << $($x),*}
    };
    ($nodes:ident << $v:ident) => {
        $nodes.extend($v.take_node().map(|node| Box::new(node) as Box<dyn Node>));
    }
}

macro_rules! generic_expr {
    {$f:ident($($v:ident : $t:ident),* $(,)?) -> Scalar[$unit:expr] :: $k:ident} => {
        pub fn $f(&mut self, $(mut $v: Expr<super::$t>),*) -> Expr<super::Scalar> {
            let nodes = take_nodes!($($v),*);
            self.expr_with(super::Scalar {
                unit: Some($unit),
                data: super::ScalarData::$k($($v),*)
            }, nodes)
        }
    };
    {$f:ident($($v:ident : $t:ident),* $(,)?) -> $r:ident :: $k:ident} => {
        pub fn $f(&mut self, $(mut $v: Expr<super::$t>),*) -> Expr<super::$r> {
            let nodes = take_nodes!($($v),*);
            self.expr_with(super::$r::$k($($v),*), nodes)
        }
    }
}

// Expression constructors
impl CompileContext {
    fn expr_with<T: Displayed>(&mut self, content: T, nodes: Vec<Box<dyn Node>>) -> Expr<T> {
        let mut node = HierarchyNode::new(
            T::Node::from_props(self, Properties::from(None))
        );

        node.extend_boxed(nodes);

        Expr {
            weight: FastFloat::One,
            span: span!(0, 0, 0, 0),
            data: Rc::new(content),
            node: Some(node)
        }
    }

    pub fn dst_literal(&mut self, v: f64) -> Expr<UnrolledScalar> {
        self.expr_with(UnrolledScalar {
            unit: Some(unit::DISTANCE),
            data: ScalarData::DstLiteral(v)
        }, Vec::new())
    }

    generic_expr!{intersection(k: Line, l: Line) -> Point::LineLineIntersection}
    generic_expr!{distance_pp(p: Point, q: Point) -> Scalar[unit::DISTANCE]::PointPointDistance}
    generic_expr!{distance_pl(p: Point, k: Line) -> Scalar[unit::DISTANCE]::PointLineDistance}
}

macro_rules! generic_rule {
    ($f:ident($a:ident, $b:ident) -> $r:ident) => {
        pub fn $f(&mut self, mut a: Expr<super::$a>, mut b: Expr<super::$b>) {
            let (lhs, rhs) = (a.take_node(), b.take_node());
            self.rule_with(UnrolledRuleKind::$r(a, b), lhs, rhs, false);
        }
    }
}

// Rule constructors
impl CompileContext {
    fn rule_with<N: Node, M: Node>(&mut self, kind: UnrolledRuleKind, lhs: Option<N>, rhs: Option<M>, inverted: bool) {
        let mut node = CollectionNode::new();

        node.extend(lhs);
        node.extend(rhs);

        self.rules.push(UnrolledRule {
            kind,
            inverted,
            node: Some(Box::new(node))
        })
    }

    generic_rule!{scalar_eq(Scalar, Scalar) -> ScalarEq}
}
