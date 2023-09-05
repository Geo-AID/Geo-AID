use std::{collections::HashMap, rc::Rc, cell::RefCell, fmt::Debug};

use crate::script::{compile::PreFigure, ty};

use super::{Variable, UnrolledExpression, FlagSet, UnrolledRule, FlagSetConstructor, UnrolledExpressionData};

/// A definition has an order - a non-negative integer - depicting how 'modifiable' it is.
/// It can also use other definitions/entities.
pub trait Definition {
    /// Get the complexity order (how much adjustment is done to this entity).
    fn order(&self, context: &CompileContext) -> usize;

    /// Check if the definition contains an entity
    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool;
}

/// A scalar is either a bind or a free real value.
#[derive(Debug)]
pub enum Scalar {
    /// A free, adjusted real.
    Free,
    /// A bind
    Bind(UnrolledExpression)
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
#[derive(Debug)]
pub enum Point {
    /// A free, adjusted complex.
    Free,
    /// A single-value clip.
    OnCircle(UnrolledExpression),
    /// A bind
    Bind(UnrolledExpression)
}

impl Definition for Point {
    fn order(&self, context: &CompileContext) -> usize {
        match self {
            Self::Free => 1,
            Self::OnCircle(_) => 2,
            Self::Bind(expr) => expr.order(context),
        }
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        match self {
            Self::Free => false,
            Self::OnCircle(expr) => expr.contains_entity(entity, context),
            Self::Bind(expr) => expr.contains_entity(entity, context),
        }
    }
}

/// A line is always a bind.
#[derive(Debug)]
pub enum Line {
    /// A bind
    Bind(UnrolledExpression)
}

impl Definition for Line {
    fn order(&self, context: &CompileContext) -> usize {
        match self {
            Self::Bind(expr) => expr.order(context),
        }
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        match self {
            Self::Bind(expr) => expr.contains_entity(entity, context),
        }
    }
}

/// A circle is always a bind.
#[derive(Debug)]
pub enum Circle {
    /// A bind
    Bind(UnrolledExpression),
    /// A never compiled temporary value
    Temporary
}

impl Definition for Circle {
    fn order(&self, context: &CompileContext) -> usize {
        match self {
            Self::Temporary => usize::MAX,
            Self::Bind(expr) => expr.order(context),
        }
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        match self {
            Self::Temporary => false,
            Self::Bind(expr) => expr.contains_entity(entity, context),
        }
    }
}

/// An entity is a single primitive on the figure plane.
#[derive(Debug)]
pub enum Entity {
    /// A scalar
    Scalar(Scalar),
    Point(Point),
    Line(Line),
    Circle(Circle)
}

impl Entity {
    pub fn free_point() -> Self {
        Self::Point(Point::Free)
    }

    pub fn free_scalar() -> Self {
        Self::Scalar(Scalar::Free)
    }

    pub fn free_circle() -> Self {
        Self::Circle(Circle::Temporary)
    }

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

#[derive(Debug)]
pub struct CircleHandle(UnrolledExpression);

impl Definition for CircleHandle {
    fn order(&self, context: &CompileContext) -> usize {
        self.0.order(context)
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        self.0.contains_entity(entity, context)
    }
}

#[derive(Debug)]
pub struct PointHandle(UnrolledExpression);

impl Definition for PointHandle {
    fn order(&self, context: &CompileContext) -> usize {
        self.0.order(context)
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        self.0.contains_entity(entity, context)
    }
}

/// The context of compilation process.
#[derive(Debug)]
pub struct CompileContext {
    /// Variables
    pub variables: HashMap<String, Rc<RefCell<Variable>>>,
    /// Flags
    pub flags: FlagSet,
    /// Entities (primitives).
    pub entities: Vec<Entity>,
    /// Unrolled rules
    pub rules: Vec<UnrolledRule>,
    /// Pre-compiled figure
    pub figure: PreFigure
}

impl CompileContext {
    pub fn new() ->  Self {
        Self {
            variables: HashMap::new(),
            flags: FlagSetConstructor::new()
                .add_set(
                    &"optimizations",
                    FlagSetConstructor::new().add_bool_def(&"identical_expressions", true),
                )
                .add_ident_def(&"distance_literals", &"none")
                .add_bool_def(&"point_bounds", false)
                .finish(),
            entities: vec![],
            rules: Vec::new(),
            figure: PreFigure::default()
        }
    }

    /// Gets the entity of the given index.
    pub fn get_entity(&self, i: usize) -> &Entity {
        &self.entities[i]
    }

    /// Gets the entity of the given index.
    pub fn get_entity_mut(&self, i: usize) -> &mut Entity {
        &mut self.entities[i]
    }

    pub fn add_scalar(&mut self) -> usize {
        self.entities.push(Entity::free_scalar());

        self.entities.len() - 1
    }

    pub fn add_point(&mut self) -> usize {
        self.entities.push(Entity::free_point());

        self.entities.len() - 1
    }

    pub fn get_point_by_index(&mut self, index: usize) -> PointHandle {
        let entity = self.entities.get(index).unwrap();
        if let Entity::Point(_) = entity {
            return PointHandle(UnrolledExpression::new_spanless(UnrolledExpressionData::Entity(index), ty::POINT));
        } else {
            panic!("Requested entity is not a point.");
        }
    }

    pub fn get_point_by_expr(&mut self, expr: &UnrolledExpression) -> PointHandle {
        match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => self.get_point_by_expr(&var.borrow().definition),
            UnrolledExpressionData::Entity(index) => self.get_point_by_index(*index),
            UnrolledExpressionData::Boxed(expr) => self.get_point_by_expr(expr),
            UnrolledExpressionData::IndexCollection(_, _)
            | UnrolledExpressionData::Average(_)
            | UnrolledExpressionData::LineLineIntersection(_, _) => PointHandle(expr.clone()),
            UnrolledExpressionData::Circle(_, _)
            | UnrolledExpressionData::PerpendicularThrough(_, _)
            | UnrolledExpressionData::ParallelThrough(_, _)
            | UnrolledExpressionData::LineFromPoints(_, _)
            | UnrolledExpressionData::SetUnit(_, _)
            | UnrolledExpressionData::PointPointDistance(_, _)
            | UnrolledExpressionData::PointLineDistance(_, _)
            | UnrolledExpressionData::Negate(_)
            | UnrolledExpressionData::Add(_, _)
            | UnrolledExpressionData::Subtract(_, _)
            | UnrolledExpressionData::Multiply(_, _)
            | UnrolledExpressionData::Divide(_, _)
            | UnrolledExpressionData::ThreePointAngle(_, _, _)
            | UnrolledExpressionData::ThreePointAngleDir(_, _, _)
            | UnrolledExpressionData::TwoLineAngle(_, _)
            | UnrolledExpressionData::AngleBisector(_, _, _)
            | UnrolledExpressionData::Number(_)
            | UnrolledExpressionData::PointCollection(_) => panic!("Requested entity is not a point."),
        }
    }

    pub fn get_point_entity_mut(&mut self, handle: &PointHandle) -> Option<&mut Point> {
        match handle.0.data.as_ref() {
            UnrolledExpressionData::Entity(i) => self.get_entity_mut(*i).as_point_mut(),
            _ => None
        }
    }

    pub fn add_circle(&mut self) -> usize {
        self.entities.push(Entity::free_circle());

        self.entities.len() - 1
    }

    pub fn get_circle_by_index(&mut self, index: usize) -> CircleHandle {
        let entity = self.entities.get(index).unwrap();
        if let Entity::Circle(_) = entity {
            return CircleHandle(UnrolledExpression::new_spanless(UnrolledExpressionData::Entity(index), ty::CIRCLE));
        } else {
            panic!("Requested entity is not a point.");
        }
    }

    pub fn get_circle_by_expr(&mut self, expr: &UnrolledExpression) -> CircleHandle {
        match expr.data.as_ref() {
            UnrolledExpressionData::VariableAccess(var) => self.get_circle_by_expr(&var.borrow().definition),
            UnrolledExpressionData::Entity(index) => self.get_circle_by_index(*index),
            UnrolledExpressionData::Boxed(expr) => self.get_circle_by_expr(expr),
            UnrolledExpressionData::Circle(_, _) => CircleHandle(expr.clone()),
            UnrolledExpressionData::IndexCollection(_, _)
            | UnrolledExpressionData::Average(_)
            | UnrolledExpressionData::LineLineIntersection(_, _)
            | UnrolledExpressionData::PerpendicularThrough(_, _)
            | UnrolledExpressionData::ParallelThrough(_, _)
            | UnrolledExpressionData::LineFromPoints(_, _)
            | UnrolledExpressionData::SetUnit(_, _)
            | UnrolledExpressionData::PointPointDistance(_, _)
            | UnrolledExpressionData::PointLineDistance(_, _)
            | UnrolledExpressionData::Negate(_)
            | UnrolledExpressionData::Add(_, _)
            | UnrolledExpressionData::Subtract(_, _)
            | UnrolledExpressionData::Multiply(_, _)
            | UnrolledExpressionData::Divide(_, _)
            | UnrolledExpressionData::ThreePointAngle(_, _, _)
            | UnrolledExpressionData::ThreePointAngleDir(_, _, _)
            | UnrolledExpressionData::TwoLineAngle(_, _)
            | UnrolledExpressionData::AngleBisector(_, _, _)
            | UnrolledExpressionData::Number(_)
            | UnrolledExpressionData::PointCollection(_) => panic!("Requested entity is not a circle."),
        }
    }
}

/// Everything related to circles.
impl CompileContext {
    pub fn point_on_circle(&mut self, lhs: PointHandle, rhs: CircleHandle) {
        if let Some(point) = self.get_point_entity_mut(&lhs) {

        }
    }
}