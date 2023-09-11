use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{Display, Debug},
    ops::{Deref, DerefMut},
    rc::Rc, write, cell::RefCell,
};

use crate::span;

pub use context::{CompileContext, Definition, Entity, Scalar as EntScalar, Point as EntPoint, Line as EntLine, Circle as EntCircle};

use super::{
    builtins::{self, macros::variable},
    parser::{
        BinaryOperator, DisplayProperties, ExplicitIterator, Expression, FlagStatement,
        ImplicitIterator, LetStatement, Parse, PredefinedRuleOperator, Type,
        PropertyValue, Punctuated, RuleOperator, RuleStatement, SimpleExpression, Statement, SimpleExpressionKind
    },
    token::{self, Ident, NamedIdent, PointCollection, Span},
    ty, ComplexUnit, Error, SimpleUnit, unit
};

mod context;

/// A definition for a user-defined rule operator.
#[derive(Debug)]
pub struct RuleOperatorDefinition {
    /// Operator's name.
    pub name: String,
}

/// Meta info about a point.
#[derive(Debug, Clone)]
pub struct PointMeta {
    /// The letter of a point.
    pub letter: char,
    /// The count of `'` in the point's name.
    pub primes: u8,
    /// The point index.
    pub index: Option<u16>,
}

/// A point in variable meta.
#[derive(Debug, Clone)]
pub struct Point {
    /// A point meta is optional, since not every point has a letter.
    pub meta: Option<PointMeta>,
    /// Whether or not to display the point.
    pub display: bool,
}

/// Defines meta information about variables, mostly in regard to the displaying of them.
#[derive(Debug)]
pub enum VariableMeta {
    /// Point variable
    Point(Point),
    Scalar,
    Line,
    PointCollection,
    Circle,
}

impl VariableMeta {
    #[must_use]
    pub fn as_point(&self) -> Option<&Point> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A variable created with a let statement.
#[derive(Debug)]
pub struct Variable {
    /// Variable's name
    pub name: String,
    /// Variable's definition span.
    pub definition_span: Span,
    /// Variable's definition.
    pub definition: UnrolledExpression
}

pub type FlagSet = HashMap<String, Flag>;

pub struct FlagSetConstructor {
    pub flags: Vec<(String, Flag)>,
}

impl FlagSetConstructor {
    #[must_use]
    pub fn new() -> Self {
        Self { flags: Vec::new() }
    }

    #[must_use]
    pub fn add_ident<S: ToString>(mut self, name: &S) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Unset),
                ty: FlagType::String,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_ident_def<S: ToString>(mut self, name: &S, default: &S) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Default(FlagValue::String(
                    default.to_string(),
                ))),
                ty: FlagType::String,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_bool<S: ToString>(mut self, name: &S) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Unset),
                ty: FlagType::Boolean,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_bool_def<S: ToString>(mut self, name: &S, default: bool) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Default(FlagValue::Bool(default))),
                ty: FlagType::Boolean,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_set<S: ToString>(mut self, name: &S, set: FlagSetConstructor) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Set(set.finish()),
                ty: FlagType::Set,
            },
        ));

        self
    }

    #[must_use]
    pub fn finish(self) -> FlagSet {
        self.flags.into_iter().collect()
    }
}

impl Default for FlagSetConstructor {
    fn default() -> Self {
        Self::new()
    }
}

/// A compiler flag.
#[derive(Debug)]
pub struct Flag {
    pub name: String,
    pub kind: FlagKind,
    pub ty: FlagType,
}

impl Flag {
    #[must_use]
    pub fn as_set(&self) -> Option<&FlagSet> {
        match &self.kind {
            FlagKind::Setting(_) => None,
            FlagKind::Set(set) => Some(set),
        }
    }

    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_value().and_then(FlagValue::as_bool).copied(),
            FlagKind::Set(_) => None,
        }
    }

    #[must_use]
    pub fn as_ident(&self) -> Option<&String> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_value().and_then(FlagValue::as_string),
            FlagKind::Set(_) => None,
        }
    }

    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_span(),
            FlagKind::Set(_) => None,
        }
    }
}

#[derive(Debug)]
pub enum FlagType {
    Set,
    Boolean,
    String,
}

/// A compiler flag.
#[derive(Debug)]
pub enum FlagKind {
    Setting(FlagSetting),
    Set(FlagSet),
}

/// A compiler flag value.
#[derive(Debug)]
pub enum FlagSetting {
    Default(FlagValue),
    Unset,
    Set(FlagValue, Span),
}

impl FlagSetting {
    #[must_use]
    pub fn get_value(&self) -> Option<&FlagValue> {
        match self {
            FlagSetting::Default(v) | FlagSetting::Set(v, _) => Some(v),
            FlagSetting::Unset => None,
        }
    }

    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        match self {
            FlagSetting::Default(_) | FlagSetting::Unset => None,
            FlagSetting::Set(_, sp) => Some(*sp),
        }
    }
}

/// A compiler flag value.
#[derive(Debug)]
pub enum FlagValue {
    String(String),
    Bool(bool),
}

impl FlagValue {
    #[must_use]
    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_string(&self) -> Option<&String> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// An overload of a function in `GeoScript`.
#[derive(Debug)]
pub struct RuleOverload {
    /// The parameter types.
    pub params: (Type, Type),
    /// The definition.
    pub definition: RuleDefinition
}

/// geoscript rule declaration
type GeoRule = dyn Fn(&UnrolledExpression, &UnrolledExpression, &mut CompileContext, Option<Properties>);

/// A function definition.
pub struct RuleDefinition(pub Box<GeoRule>);

impl Debug for RuleDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function definition")
    }
}

impl Deref for RuleDefinition {
    type Target = Box<GeoRule>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A function.
#[derive(Debug)]
pub struct Rule {
    /// Rule's name
    pub name: String,
    /// Rule's overloads.
    pub overloads: Vec<RuleOverload>,
}

impl Rule {
    /// Checks if the given params can be converted into the expected params.
    ///
    /// # Panics
    /// Never. Shut up, clippy.
    #[must_use]
    pub fn match_params(
        expected: (&Type, &Type),
        received: (&Type, &Type)
    ) -> bool {
        received.0.can_cast(expected.0) && received.1.can_cast(expected.1)
    }

    /// Tries to find an overload for the given param types.
    #[must_use]
    pub fn get_overload(&self, received: (&Type, &Type)) -> Option<&RuleOverload> {
        self.overloads
            .iter()
            .find(|x| Rule::match_params((&x.params.0, &x.params.1), received))
    }
}

/// An overload of a function in `GeoScript`.
#[derive(Debug)]
pub struct FunctionOverload {
    /// The parameter types.
    pub params: Vec<Type>,
    /// The returned type
    pub returned_type: Type,
    /// The definition.
    pub definition: FunctionDefinition,
    /// Possible parameter group
    pub param_group: Option<Type>,
}

/// geoscript function declaration
type GeoFunc = dyn Fn(&[UnrolledExpression], &mut CompileContext, Option<Properties>) -> UnrolledExpression;

/// A function definition.
pub struct FunctionDefinition(pub Box<GeoFunc>);

impl Debug for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function definition")
    }
}

impl Deref for FunctionDefinition {
    type Target = Box<GeoFunc>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A function.
#[derive(Debug)]
pub struct Function {
    /// Function's name
    pub name: String,
    /// Function's overloads.
    pub overloads: Vec<FunctionOverload>,
}

impl Function {
    /// Checks if the given params can be converted into the expected params.
    ///
    /// # Panics
    /// Never. Shut up, clippy.
    #[must_use]
    pub fn match_params(
        expected: &Vec<Type>,
        received: &Vec<Type>,
        param_group: Option<&Type>,
    ) -> bool {
        if let Some(ty) = param_group {
            if expected.len() < received.len() {
                let mut received = received.iter();

                for param in expected {
                    if !received.next().unwrap().can_cast(param) {
                        return false;
                    }
                }

                for param in received {
                    if !param.can_cast(ty) {
                        return false;
                    }
                }

                true
            } else {
                false
            }
        } else if expected.len() == received.len() {
            for (i, param) in expected.iter().enumerate() {
                if !received[i].can_cast(param) {
                    return false;
                }
            }

            true
        } else {
            false
        }
    }

    /// Tries to find an overload for the given param types.
    #[must_use]
    pub fn get_overload(&self, params: &Vec<Type>) -> Option<&FunctionOverload> {
        self.overloads
            .iter()
            .find(|x| Function::match_params(&x.params, params, x.param_group.as_ref()))
    }

    #[must_use]
    pub fn get_returned(&self, params: &Vec<Type>) -> Type {
        self.get_overload(params)
            .map_or(Type::Undefined, |x| x.returned_type)
    }
}

/// The library of all rules and functions available in GeoScript.
#[derive(Debug)]
pub struct Library {
    /// Functions
    pub functions: HashMap<String, Function>,
    /// The rule operators.
    pub rule_ops: HashMap<String, Rc<Rule>>
}

impl Library {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            rule_ops: HashMap::new()
        }
    }
}

/// Represents complicated iterator structures.
#[derive(Debug)]
pub struct IterTree {
    pub variants: Vec<IterNode>,
    pub id: u8,
    pub span: Span,
}

/// A single node of `IterTree`.
#[derive(Debug)]
pub struct IterNode(Vec<IterTree>);

impl Deref for IterNode {
    type Target = Vec<IterTree>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for IterNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<const ITER: bool> From<&Expression<ITER>> for IterNode {
    fn from(value: &Expression<ITER>) -> Self {
        match value {
            Expression::ImplicitIterator(it) => IterNode::new(vec![it.into()]),
            Expression::Single(expr) => expr.as_ref().into(),
            Expression::Binop(binop) => Self::from2(binop.lhs.as_ref(), binop.rhs.as_ref()),
        }
    }
}

impl From<&SimpleExpression> for IterNode {
    fn from(value: &SimpleExpression) -> Self {
        match &value.kind {
            SimpleExpressionKind::Ident(_) | SimpleExpressionKind::Number(_) => IterNode::new(Vec::new()),
            SimpleExpressionKind::Call(expr) => match &expr.params {
                Some(params) => IterNode::new(
                    params
                        .iter()
                        .flat_map(|v| IterNode::from(v).0.into_iter())
                        .collect(),
                ),
                None => IterNode::new(Vec::new()),
            },
            SimpleExpressionKind::Unop(expr) => expr.rhs.as_ref().into(),
            SimpleExpressionKind::Parenthised(expr) => expr.content.as_ref().into(),
            SimpleExpressionKind::ExplicitIterator(it) => IterNode::new(vec![it.into()]),
            SimpleExpressionKind::PointCollection(col) => IterNode::new(
                col.points
                    .iter()
                    .flat_map(|v| IterNode::from(v).0.into_iter())
                    .collect(),
            ),
        }
    }
}

impl From<&ImplicitIterator> for IterTree {
    fn from(value: &ImplicitIterator) -> Self {
        Self {
            id: 0, // Implicit iterators have an id of 0.
            variants: value.exprs.iter().map(IterNode::from).collect(),
            span: value.get_span(),
        }
    }
}

impl From<&ExplicitIterator> for IterTree {
    fn from(value: &ExplicitIterator) -> Self {
        Self {
            id: value.id,
            variants: value.exprs.iter().map(IterNode::from).collect(),
            span: value.get_span(),
        }
    }
}

impl IterTree {
    /// # Errors
    /// Returns an error when there is an inconsistency in among iterators (same id, different lengths)
    /// or when an iterator with id x contains an iterator with id x.
    pub fn get_iter_lengths(
        &self,
        lengths: &mut HashMap<u8, (usize, Span, Option<Span>)>,
        full_span: Span,
    ) -> Result<(), Error> {
        for variant in &self.variants {
            variant.get_iter_lengths(lengths, full_span)?;
        }

        Ok(())
    }
}

impl IterNode {
    #[must_use]
    pub fn new(content: Vec<IterTree>) -> Self {
        Self(content)
    }

    #[must_use]
    pub fn from2<const ITER1: bool, const ITER2: bool>(
        e1: &Expression<ITER1>,
        e2: &Expression<ITER2>,
    ) -> Self {
        let mut node = Self::from(e1);
        node.extend(Self::from(e2).0.into_iter());
        node
    }

    /// # Panics
    /// Never.
    ///
    /// # Errors
    /// Returns an error when there is an inconsistency in among iterators (same id, different lengths)
    /// or when an iterator with id x contains an iterator with id x.
    pub fn get_iter_lengths(
        &self,
        lengths: &mut HashMap<u8, (usize, Span, Option<Span>)>,
        full_span: Span,
    ) -> Result<(), Error> {
        for iter in &self.0 {
            match lengths.entry(iter.id) {
                Entry::Vacant(entry) => {
                    entry.insert((iter.variants.len(), iter.span, Some(iter.span)));
                }
                Entry::Occupied(mut entry) => {
                    if entry.get().0 != iter.variants.len() {
                        return Err(Error::InconsistentIterators {
                            first_span: entry.get().1,
                            first_length: entry.get().0,
                            occured_span: iter.span,
                            occured_length: iter.variants.len(),
                            error_span: full_span,
                        });
                    } else if let Some(parent) = entry.get().2 {
                        return Err(Error::IteratorWithSameIdIterator {
                            error_span: full_span,
                            parent_span: parent,
                            contained_span: iter.span,
                        });
                    }

                    entry.get_mut().2 = Some(iter.span);
                }
            }

            iter.get_iter_lengths(lengths, full_span)?;
            lengths.get_mut(&iter.id).unwrap().2 = None;
        }

        Ok(())
    }
}

/// A range-like iterator, except multidimensional.
#[derive(Debug)]
pub struct MultiRangeIterator {
    maxes: Vec<usize>,
    currents: Vec<usize>,
}

impl MultiRangeIterator {
    #[must_use]
    pub fn new(maxes: Vec<usize>) -> Self {
        let l = maxes.len();

        Self {
            maxes,
            currents: vec![0].repeat(l),
        }
    }

    pub fn increment(&mut self) -> Option<&Vec<usize>> {
        self.increment_place(self.currents.len() - 1)
    }

    fn increment_place(&mut self, at: usize) -> Option<&Vec<usize>> {
        self.currents[at] += 1;

        if self.currents[at] == self.maxes[at] {
            self.currents[at] = 0;
            if at == 0 {
                None
            } else {
                self.increment_place(at - 1)
            }
        } else {
            Some(&self.currents)
        }
    }

    #[must_use]
    pub fn get_currents(&self) -> &Vec<usize> {
        &self.currents
    }
}

#[derive(Debug)]
pub struct IterTreeIterator<'r> {
    /// List of lists of parallel iterators with their ids, current indices and lengths.
    steps: Vec<(Vec<(&'r IterTree, usize)>, MultiRangeIterator)>,
    currents: Option<HashMap<u8, usize>>,
}

impl<'r> IterTreeIterator<'r> {
    #[must_use]
    pub fn new(tree: &'r IterNode) -> Self {
        let mut this = Self {
            steps: Vec::new(),
            currents: Some(HashMap::new()),
        };

        this.add_node(tree);

        this
    }

    fn add_node(&mut self, node: &'r IterNode) {
        if node.len() > 0 {
            let mut visited = Vec::new();
            let mut lengths = Vec::new();

            for tree in node.iter() {
                if !visited.contains(&tree.id) {
                    visited.push(tree.id);
                    lengths.push(tree.variants.len());
                }
            }

            self.steps.push((
                // `visited` will be in order. We can use that.
                node.iter()
                    .map(|v| {
                        (
                            v,
                            visited
                                .iter()
                                .enumerate()
                                .find(|(_, x)| **x == v.id)
                                .unwrap()
                                .0,
                        )
                    })
                    .collect(),
                MultiRangeIterator::new(lengths),
            ));
            self.update_currents();

            self.update_iterators();
        }
    }

    fn update_iterators(&mut self) {
        let nodes = if let Some(node) = self.steps.last() {
            node.0
                .iter()
                .map(|iter| &iter.0.variants[node.1.get_currents()[iter.1]])
                .collect()
        } else {
            Vec::new()
        };

        for node in nodes {
            self.add_node(node);
        }
    }

    fn update_currents(&mut self) {
        let node = self.steps.last().unwrap();
        let currents = node.1.get_currents();

        for v in &node.0 {
            self.currents
                .as_mut()
                .unwrap()
                .entry(v.0.id)
                .and_modify(|x| *x = currents[v.1])
                .or_insert(currents[v.1]);
        }
    }

    pub fn next(&mut self) {
        while let Some(node) = self.steps.last_mut() {
            if node.1.increment().is_some() {
                self.update_currents();
                self.update_iterators();
                return;
            }

            self.steps.pop();
        }

        self.currents = None;
    }

    #[must_use]
    pub fn get_currents(&self) -> Option<&HashMap<u8, usize>> {
        self.currents.as_ref()
    }
}

/// The kind on the unrolled rule.
#[derive(Debug)]
pub enum UnrolledRuleKind {
    Eq,
    Gt,
    Lt,
}

#[derive(Debug, Clone)]
pub enum UnrolledExpressionData {
    VariableAccess(Rc<RefCell<Variable>>),
    PointCollection(Vec<UnrolledExpression>),
    Number(f64),
    Entity(usize),
    Boxed(UnrolledExpression),
    IndexCollection(UnrolledExpression, usize),
    LineFromPoints(UnrolledExpression, UnrolledExpression),
    SetUnit(UnrolledExpression, ComplexUnit),
    PointPointDistance(UnrolledExpression, UnrolledExpression),
    PointLineDistance(UnrolledExpression, UnrolledExpression),
    Negate(UnrolledExpression),
    Add(UnrolledExpression, UnrolledExpression),
    Subtract(UnrolledExpression, UnrolledExpression),
    Multiply(UnrolledExpression, UnrolledExpression),
    Divide(UnrolledExpression, UnrolledExpression),
    ThreePointAngle(UnrolledExpression, UnrolledExpression, UnrolledExpression),
    ThreePointAngleDir(UnrolledExpression, UnrolledExpression, UnrolledExpression), // Directed angle
    TwoLineAngle(UnrolledExpression, UnrolledExpression),
    AngleBisector(UnrolledExpression, UnrolledExpression, UnrolledExpression),
    Average(Vec<UnrolledExpression>),
    PerpendicularThrough(UnrolledExpression, UnrolledExpression), // Line, Point
    ParallelThrough(UnrolledExpression, UnrolledExpression),      // Line, Point
    LineLineIntersection(UnrolledExpression, UnrolledExpression),
    Circle(UnrolledExpression, UnrolledExpression), // Center, radius
}

impl Definition for UnrolledExpressionData {
    fn order(&self, context: &CompileContext) -> usize {
        match self {
            Self::Entity(i) => context.get_entity(*i).order(context),
            _ => 0 // Everything else is considered a bind.
        }
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        match self {
            UnrolledExpressionData::VariableAccess(var) => var.borrow().definition.contains_entity(entity, context),
            UnrolledExpressionData::Entity(id) => {
                if *id == entity {
                    true
                } else {
                    context.get_entity(*id).contains_entity(entity, context)
                }
            }
            UnrolledExpressionData::PointCollection(v) | UnrolledExpressionData::Average(v) => {
                v.iter().any(|x| x.contains_entity(entity, context))
            }
            UnrolledExpressionData::Number(_) => false,
            UnrolledExpressionData::Boxed(v)
            | UnrolledExpressionData::IndexCollection(v, _)
            | UnrolledExpressionData::CircleCenter(v)
            | UnrolledExpressionData::CircleRadius(v)
            | UnrolledExpressionData::Negate(v) => v.contains_entity(entity, context),
            UnrolledExpressionData::SetUnit(v, _) => {
                v.contains_entity(entity, context)
            }
            UnrolledExpressionData::PointPointDistance(e1, e2)
            | UnrolledExpressionData::PointLineDistance(e1, e2) => e1
                .contains_entity(entity, context)
                || e2.contains_entity(entity, context),
            UnrolledExpressionData::Add(v1, v2)
            | UnrolledExpressionData::Circle(v1, v2)
            | UnrolledExpressionData::LineFromPoints(v1, v2)
            | UnrolledExpressionData::ParallelThrough(v1, v2)
            | UnrolledExpressionData::PerpendicularThrough(v1, v2)
            | UnrolledExpressionData::LineLineIntersection(v1, v2)
            | UnrolledExpressionData::TwoLineAngle(v1, v2)
            | UnrolledExpressionData::Subtract(v1, v2)
            | UnrolledExpressionData::Multiply(v1, v2)
            | UnrolledExpressionData::Divide(v1, v2) => v1
                .contains_entity(entity, context)
                || v2.contains_entity(entity, context),
            UnrolledExpressionData::ThreePointAngle(v1, v2, v3)
            | UnrolledExpressionData::ThreePointAngleDir(v1, v2, v3)
            | UnrolledExpressionData::AngleBisector(v1, v2, v3) => v1
                .contains_entity(entity, context)
                || v2.contains_entity(entity, context)
                || v3.contains_entity(entity, context)
        }
    }
}

impl Display for UnrolledExpressionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnrolledExpressionData::VariableAccess(name) => write!(f, "{}", name.borrow().name),
            UnrolledExpressionData::PointCollection(col) => write!(
                f,
                "col({})",
                col.iter()
                    .map(|v| format!("{v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            UnrolledExpressionData::Average(exprs) => write!(
                f,
                "average({})",
                exprs
                    .iter()
                    .map(|v| format!("{v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            UnrolledExpressionData::Number(num) => write!(f, "{num}"),
            UnrolledExpressionData::Entity(i) => write!(f, "Entity {i}"),
            UnrolledExpressionData::Boxed(expr) | UnrolledExpressionData::SetUnit(expr, _) => {
                write!(f, "{expr}")
            }
            UnrolledExpressionData::IndexCollection(expr, index) => write!(f, "{expr}[{index}]"),
            UnrolledExpressionData::LineFromPoints(e1, e2) => write!(f, "line({e1}, {e2})"),
            UnrolledExpressionData::PointPointDistance(e1, e2)
            | UnrolledExpressionData::PointLineDistance(e1, e2) => write!(f, "dst({e1}, {e2})"),
            UnrolledExpressionData::Negate(e) => write!(f, "-{e}"),
            UnrolledExpressionData::Add(e1, e2) => write!(f, "{e1} + {e2}"),
            UnrolledExpressionData::Multiply(e1, e2) => write!(f, "{e1} * {e2}"),
            UnrolledExpressionData::Divide(e1, e2) => write!(f, "{e1} / {e2}"),
            UnrolledExpressionData::Subtract(e1, e2) => write!(f, "{e1} - {e2}"),
            UnrolledExpressionData::ThreePointAngle(e1, e2, e3) => {
                write!(f, "angle({e1}, {e2}, {e3})")
            }
            UnrolledExpressionData::ThreePointAngleDir(e1, e2, e3) => {
                write!(f, "dir_angle({e1}, {e2}, {e3})")
            }
            UnrolledExpressionData::TwoLineAngle(e1, e2) => write!(f, "angle({e1}, {e2})"),
            UnrolledExpressionData::AngleBisector(e1, e2, e3) => {
                write!(f, "angle-bisector({e1}, {e2}, {e3})")
            }
            UnrolledExpressionData::PerpendicularThrough(l, p) => {
                write!(f, "perpendicular-through({l}, {p})")
            }
            UnrolledExpressionData::ParallelThrough(l, p) => {
                write!(f, "parallel-through({l}, {p})")
            }
            UnrolledExpressionData::LineLineIntersection(l1, l2) => {
                write!(f, "intersection({l1}, {l2})")
            }
            UnrolledExpressionData::Circle(center, radius) => {
                write!(f, "circle({center}, {radius})")
            }
        }
    }
}

pub fn display_vec<T: Display>(v: &[T]) -> String {
    v.iter()
        .map(|x| format!("{x}"))
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Debug, Clone)]
pub struct UnrolledExpression {
    pub data: Rc<UnrolledExpressionData>,
    pub ty: Type,
    pub span: Span,
    pub weight: f64, // Assigned weight.
}

impl Definition for UnrolledExpression {
    fn order(&self, context: &CompileContext) -> usize {
        self.data.order(context)
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        self.data.contains_entity(entity, context)
    }
}

impl Display for UnrolledExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl UnrolledExpression {
    pub fn new_spanless(data: UnrolledExpressionData, ty: Type) -> Self {
        Self {
            span: span!(0,0,0,0),
            ty,
            data: Rc::new(data),
            weight: 1.0
        }
    }

    #[must_use]
    pub fn clone_with_weight(&self, weight: f64) -> Self {
        let mut cloned = self.clone();
        cloned.weight = weight;
        cloned
    }
}

#[derive(Debug)]
pub struct UnrolledRule {
    pub kind: UnrolledRuleKind,
    pub lhs: UnrolledExpression,
    pub rhs: UnrolledExpression,
    pub inverted: bool,
}

impl Display for UnrolledRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}{} {}",
            self.lhs,
            if self.inverted { "!" } else { "" },
            match self.kind {
                UnrolledRuleKind::Eq => "=",
                UnrolledRuleKind::Gt => ">",
                UnrolledRuleKind::Lt => "<",
            },
            self.rhs
        )
    }
}

// /// A point id is a 64-bit integer. First 32 bits are for the letter codepoint, next 8 for the amount of primes, next 16 for the point index.
// fn construct_point_id(letter: char, primes: u8) -> u64 {
//     ((letter as u64) << 8) | u64::from(primes)
// }

/// Constructs the point name based on the letter and the primes.
#[must_use]
pub fn construct_point_name(letter: char, primes: u8) -> String {
    String::from(letter) + &"'".repeat(primes as usize)
}

/// Replaces all Parameter unrolled expressions with the given parameters.
#[allow(clippy::module_name_repetitions)]
#[must_use]
pub fn unroll_parameters(
    definition: &FunctionDefinition,
    params: &[UnrolledExpression],
    display: Option<Properties>,
    context: &mut CompileContext
) -> UnrolledExpression {
    definition(params, context, display)
}

/// Unrolls the conversion of a point collection into the given type.
fn unroll_pc_conversion(
    expr: &UnrolledExpression,
    to: &Type,
    collection_length: usize,
    context: &mut CompileContext
) -> Result<UnrolledExpression, Error> {
    if to == &ty::collection(0) {
        return Ok(expr.clone());
    }

    match collection_length {
        1 => {
            if to == &ty::POINT {
                Ok(UnrolledExpression {
                    weight: 1.0, // Weight is propagated through `IndexCollection`.
                    data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), 0)),
                    ty: ty::POINT,
                    span: expr.span,
                })
            } else {
                Err(Error::implicit_conversion_does_not_exist(
                    expr.span,
                    expr.ty,
                    *to,
                ))
            }
        }
        2 => match to {
            Type::Line => {
                let expr = UnrolledExpression {
                    weight: 1.0, // Weight is propagated through `IndexCollection`.
                    data: Rc::new(UnrolledExpressionData::LineFromPoints(
                        UnrolledExpression {
                            weight: 1.0, // Weight is propagated through `IndexCollection`.
                            data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), 0)),
                            ty: ty::POINT,
                            span: expr.span,
                        },
                        UnrolledExpression {
                            weight: 1.0, // Weight is propagated through `IndexCollection`.
                            data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), 1)),
                            ty: ty::POINT,
                            span: expr.span,
                        },
                    )),
                    ty: ty::LINE,
                    span: expr.span,
                };

                // Any line constructed from a point collection should be displayed.
                context.figure.lines.push(expr.clone());

                Ok(expr)
            }
            Type::Scalar(unit) => {
                if unit == &Some(ComplexUnit::new(SimpleUnit::Distance)) {
                    let a = UnrolledExpression {
                        weight: 1.0, // Weight is propagated through `IndexCollection`.
                        data: Rc::new(UnrolledExpressionData::IndexCollection(
                            expr.clone(),
                            0,
                        )),
                        ty: ty::POINT,
                        span: expr.span,
                    };

                    let b = UnrolledExpression {
                        weight: 1.0, // Weight is propagated through `IndexCollection`.
                        data: Rc::new(UnrolledExpressionData::IndexCollection(
                            expr.clone(),
                            1,
                        )),
                        ty: ty::POINT,
                        span: expr.span,
                    };

                    // Anything constructed like that is treated like a segment.
                    context.figure.segments.push((a.clone(), b.clone()));

                    Ok(UnrolledExpression {
                        weight: 1.0, // Weight is propagated through `IndexCollection`.
                        data: Rc::new(UnrolledExpressionData::PointPointDistance(a, b)),
                        ty: ty::DISTANCE,
                        span: expr.span,
                    })
                } else {
                    Err(Error::implicit_conversion_does_not_exist(
                        expr.span,
                        expr.ty,
                        *to,
                    ))
                }
            }
            _ => Err(Error::implicit_conversion_does_not_exist(
                expr.span,
                expr.ty,
                *to,
            )),
        },
        _ => Err(Error::implicit_conversion_does_not_exist(
            expr.span,
            expr.ty,
            *to,
        )),
    }
}

/// Unrolls the conversion of the given expression of type scalar(none) to a scalar type.
#[allow(clippy::too_many_lines)]
fn unroll_conversion_to_scalar(
    expr: &UnrolledExpression,
    to: &Type,
    context: &mut CompileContext
) -> Result<UnrolledExpression, Error> {
    match expr.data.as_ref() {
        UnrolledExpressionData::Boxed(x) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::new(UnrolledExpressionData::Boxed(unroll_implicit_conversion(
                x.clone(),
                to,
                context
            )?)),
        }),
        UnrolledExpressionData::Number(_) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::clone(&expr.data),
        }),
        UnrolledExpressionData::Negate(x) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::new(UnrolledExpressionData::Negate(unroll_implicit_conversion(
                x.clone(),
                to,
                context
            )?)),
        }),
        UnrolledExpressionData::Add(e1, e2) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::new(UnrolledExpressionData::Add(
                unroll_implicit_conversion(e1.clone(), to, context)?,
                unroll_implicit_conversion(e2.clone(), to, context)?,
            )),
        }),
        UnrolledExpressionData::Subtract(e1, e2) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::new(UnrolledExpressionData::Subtract(
                unroll_implicit_conversion(e1.clone(), to, context)?,
                unroll_implicit_conversion(e2.clone(), to, context)?,
            )),
        }),
        UnrolledExpressionData::Multiply(e1, e2) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::new(UnrolledExpressionData::Multiply(
                unroll_implicit_conversion(e1.clone(), to, context)?,
                unroll_implicit_conversion(
                    e2.clone(),
                    &ty::SCALAR,
                    context
                )?,
            )),
        }),
        UnrolledExpressionData::Divide(e1, e2) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::new(UnrolledExpressionData::Divide(
                unroll_implicit_conversion(e1.clone(), to, context)?,
                unroll_implicit_conversion(
                    e2.clone(),
                    &ty::SCALAR,
                    context
                )?,
            )),
        }),
        UnrolledExpressionData::Average(exprs) => Ok(UnrolledExpression {
            ty: *to,
            span: expr.span,
            weight: expr.weight,
            data: Rc::new(UnrolledExpressionData::Average(
                exprs
                    .iter()
                    .map(|v| unroll_conversion_to_scalar(v, to, context))
                    .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
            )),
        }),
        UnrolledExpressionData::VariableAccess(_)
        | UnrolledExpressionData::PointCollection(_)
        | UnrolledExpressionData::Entity(_)
        | UnrolledExpressionData::IndexCollection(_, _)
        | UnrolledExpressionData::LineFromPoints(_, _)
        | UnrolledExpressionData::SetUnit(_, _)
        | UnrolledExpressionData::PointPointDistance(_, _)
        | UnrolledExpressionData::PointLineDistance(_, _)
        | UnrolledExpressionData::ThreePointAngle(_, _, _)
        | UnrolledExpressionData::ThreePointAngleDir(_, _, _)
        | UnrolledExpressionData::AngleBisector(_, _, _)
        | UnrolledExpressionData::PerpendicularThrough(_, _)
        | UnrolledExpressionData::ParallelThrough(_, _)
        | UnrolledExpressionData::LineLineIntersection(_, _)
        | UnrolledExpressionData::Circle(_, _)
        | UnrolledExpressionData::TwoLineAngle(_, _) => unreachable!(
            "This data should not be of type scalar(none) and yet is: {:#?}",
            expr.data
        ),
    }
}

/// Unrolls implicit conversions between types.
fn unroll_implicit_conversion(
    expr: UnrolledExpression,
    to: &Type,
    context: &mut CompileContext
) -> Result<UnrolledExpression, Error> {
    if to == &expr.ty {
        Ok(expr)
    } else {
        match &expr.ty {
            Type::PointCollection(l) => unroll_pc_conversion(&expr, to, *l, context),
            Type::Scalar(unit) => {
                match unit {
                    Some(unit) => {
                        if unit.0[SimpleUnit::Angle as usize] == 0 {
                            // no angle
                            unroll_conversion_to_scalar(&expr, to, context)
                        } else {
                            Err(Error::implicit_conversion_does_not_exist(
                                expr.span,
                                expr.ty,
                                *to,
                            ))
                        }
                    }
                    None => unroll_conversion_to_scalar(&expr, to, context),
                }
            }
            _ => Err(Error::implicit_conversion_does_not_exist(
                expr.span,
                expr.ty,
                *to,
            )),
        }
    }
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
#[allow(clippy::too_many_lines)]
fn unroll_simple(
    expr: &SimpleExpression,
    context: &mut CompileContext,
    library: &Library,
    it_index: &HashMap<u8, usize>
) -> Result<UnrolledExpression, Error> {
    let display: Option<Properties> = expr.display.as_ref().map(|v| {
        Properties(
            v.properties
                .iter()
                .map(|v| (v.name.ident.clone(), v.value.clone()))
                .collect(),
        )
    });

    Ok(match &expr.kind {
        SimpleExpressionKind::Ident(i) => match i {
            Ident::Named(named) => {
                let var = context.variables.get(&named.ident).ok_or_else(|| {
                    #[allow(clippy::cast_possible_truncation)]
                    let suggested = context
                        .variables
                        .iter()
                        .max_by_key(|v| (strsim::jaro(v.0, &named.ident) * 1000.0).floor() as i64)
                        .map(|x| x.0)
                        .cloned();

                    Error::UndefinedVariable {
                        error_span: expr.get_span(),
                        variable_name: named.ident.clone(),
                        suggested,
                    }
                })?;

                UnrolledExpression {
                    weight: 1.0, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
                    ty: var.borrow().definition.ty,
                    data: Rc::new(UnrolledExpressionData::VariableAccess(Rc::clone(var))),
                    span: named.span,
                }
            }
            Ident::Collection(col) => UnrolledExpression {
                weight: 1.0, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
                ty: ty::collection(col.collection.len()),
                data: Rc::new(UnrolledExpressionData::PointCollection(
                    col.collection
                        .iter()
                        .map(|(letter, primes)| {
                            match context.variables.get(&construct_point_name(*letter, *primes)) {
                                Some(var) => Ok(UnrolledExpression {
                                    weight: 1.0, // Always one.
                                    data: Rc::new(UnrolledExpressionData::VariableAccess(
                                        Rc::clone(var),
                                    )),
                                    ty: ty::POINT,
                                    span: col.span,
                                }),
                                None => Err(Error::UndefinedVariable {
                                    error_span: col.span,
                                    variable_name: construct_point_name(*letter, *primes),
                                    suggested: None, // Pretty much every single-letter point could be considered similar, so no suggestions.
                                }),
                            }
                        })
                        .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
                )),
                span: col.span,
            },
        },
        SimpleExpressionKind::Number(num) => UnrolledExpression {
            weight: 0.0, // Always zero.
            ty: ty::SCALAR_UNKNOWN,
            data: Rc::new(UnrolledExpressionData::Number(num.value.to_float())),
            span: num.get_span(),
        },
        SimpleExpressionKind::Call(e) => {
            let params = match &e.params {
                Some(params) => params
                    .iter()
                    .map(|p| unroll_expression(p, context, library, it_index))
                    .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
                None => Vec::new(),
            };

            if let Some(func) = library.functions.get(&e.name.ident) {
                let param_types = params.iter().map(|ex| ex.ty).collect();

                if let Some(overload) = func.get_overload(&param_types) {
                    let params = params
                        .into_iter()
                        .enumerate()
                        .map(|(i, param)| {
                            if i < overload.params.len() {
                                unroll_implicit_conversion(param, &overload.params[i], context)
                            } else {
                                unroll_implicit_conversion(
                                    param,
                                    overload.param_group.as_ref().unwrap(),
                                    context
                                )
                            }
                        })
                        .collect::<Result<Vec<UnrolledExpression>, Error>>()?;

                    UnrolledExpression {
                        weight: 1.0, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
                        ty: overload.returned_type,
                        data: Rc::new(UnrolledExpressionData::Boxed(unroll_parameters(
                            &overload.definition,
                            &params,
                            display,
                            context
                        ))),
                        span: e.get_span(),
                    }
                } else {
                    return Err(Error::overload_not_found(
                        e.get_span(),
                        e.name.ident.clone(),
                        param_types,
                    ));
                }
            } else {
                #[allow(clippy::cast_possible_truncation)]
                let suggested = library
                    .functions
                    .iter()
                    .max_by_key(|v| (strsim::jaro(v.0, &e.name.ident) * 1000.0).floor() as i64)
                    .map(|x| x.0)
                    .cloned();

                return Err(Error::UndefinedFunction {
                    error_span: e.name.span,
                    function_name: e.name.ident.clone(),
                    suggested,
                });
            }
        }
        SimpleExpressionKind::Unop(op) => {
            let unrolled = unroll_simple(&op.rhs, context, library, it_index)?;
            match &unrolled.ty {
                Type::Scalar(_) => UnrolledExpression {
                    weight: 1.0, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
                    ty: unrolled.ty,
                    span: expr.get_span(),
                    data: Rc::new(UnrolledExpressionData::Negate(unrolled)),
                },
                t => {
                    return Err(Error::InvalidOperandType {
                        error_span: expr.get_span(),
                        got: (*t, op.rhs.get_span()),
                        op: String::from("-"),
                    })
                }
            }
        }
        SimpleExpressionKind::Parenthised(expr) => unroll_expression(&expr.content, context, library, it_index)?,
        SimpleExpressionKind::ExplicitIterator(it) => {
            unroll_expression(it.get(it_index[&it.id]).unwrap(), context, library, it_index)?
        }
        SimpleExpressionKind::PointCollection(col) => UnrolledExpression {
            weight: 1.0, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
            ty: ty::collection(col.points.len()),
            span: col.get_span(),
            data: Rc::new(UnrolledExpressionData::PointCollection(
                col.points
                    .iter()
                    .map(|expr| {
                        let unrolled = unroll_expression(expr, context, library, it_index)?;

                        if unrolled.ty.can_cast(&ty::POINT) {
                            Ok(unroll_implicit_conversion(unrolled, &ty::POINT, context)?)
                        } else {
                            Err(Error::NonPointInPointCollection {
                                error_span: col.get_span(),
                                received: (unrolled.span, unrolled.ty),
                            })
                        }
                    })
                    .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
            )),
        },
    })
}

fn unroll_muldiv(
    this: UnrolledExpression,
    other: &UnrolledExpression,
    context: &mut CompileContext
) -> Result<UnrolledExpression, Error> {
    match this.ty {
        Type::Scalar(None) => match &other.ty {
            Type::Scalar(None) => Ok(this),
            _ => unroll_implicit_conversion(
                this,
                &ty::SCALAR,
                context
            ),
        },
        _ => Ok(this),
    }
}

fn unroll_binop(
    lhs: UnrolledExpression,
    op: &BinaryOperator,
    rhs: UnrolledExpression,
    context: &mut CompileContext
) -> Result<UnrolledExpression, Error> {
    let lhs = match &lhs.ty {
        Type::Scalar(_) => lhs,
        Type::PointCollection(2) => unroll_implicit_conversion(lhs, &ty::DISTANCE, context)?,
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: lhs.span.join(rhs.span),
                got: (lhs.ty, lhs.span),
                op: op.to_string(),
            })
        }
    };

    let rhs = match &rhs.ty {
        Type::Scalar(_) => rhs,
        Type::PointCollection(2) => unroll_implicit_conversion(
            rhs,
            &ty::DISTANCE,
            context
        )?,
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: rhs.span.join(rhs.span),
                got: (rhs.ty, rhs.span),
                op: op.to_string(),
            })
        }
    };

    match op {
        BinaryOperator::Add(_) | BinaryOperator::Sub(_) => {
            let lhs = match lhs.ty {
                Type::Scalar(None) => {
                    unroll_implicit_conversion(lhs, &rhs.ty, context)?
                }
                _ => lhs,
            };

            let rhs = unroll_implicit_conversion(rhs, &lhs.ty, context)?;

            Ok(UnrolledExpression {
                weight: 1.0, // Technically, the only way to assign weight to an arithmetic op is to parenthise it.
                ty: lhs.ty,
                span: lhs.span.join(rhs.span),
                data: Rc::new(match op {
                    BinaryOperator::Add(_) => UnrolledExpressionData::Add(lhs, rhs),
                    BinaryOperator::Sub(_) => UnrolledExpressionData::Subtract(lhs, rhs),
                    _ => unreachable!(),
                }),
            })
        }
        BinaryOperator::Mul(_) | BinaryOperator::Div(_) => {
            let lhs = unroll_muldiv(lhs, &rhs, context)?;

            let rhs = unroll_muldiv(rhs, &lhs, context)?;

            Ok(UnrolledExpression {
                weight: 1.0, // Technically, the only way to assign weight to an arithmetic op is to parenthise it.
                ty: match &lhs.ty {
                    Type::Scalar(None) => lhs.ty,
                    Type::Scalar(Some(left_unit)) => {
                        if let Type::Scalar(Some(right_unit)) = &rhs.ty
                        {
                            Type::Scalar(Some(
                                *left_unit * right_unit,
                            ))
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!(),
                },
                span: lhs.span.join(rhs.span),
                data: Rc::new(match op {
                    BinaryOperator::Mul(_) => UnrolledExpressionData::Multiply(lhs, rhs),
                    BinaryOperator::Div(_) => UnrolledExpressionData::Divide(lhs, rhs),
                    _ => unreachable!(),
                }),
            })
        }
    }
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
fn unroll_expression<const ITER: bool>(
    expr: &Expression<ITER>,
    context: &mut CompileContext,
    library: &Library,
    it_index: &HashMap<u8, usize>
) -> Result<UnrolledExpression, Error> {
    match expr {
        Expression::Single(simple) => unroll_simple(simple.as_ref(), context, library, it_index),
        Expression::ImplicitIterator(it) => unroll_simple(
            it.get(it_index[&0]).unwrap(), // Implicit iterators always have an id of 0.
            context,
            library,
            it_index
        ),
        Expression::Binop(op) => {
            let lhs = unroll_expression(&op.lhs, context, library, it_index)?;
            let rhs = unroll_expression(&op.rhs, context, library, it_index)?;

            unroll_binop(lhs, &op.operator, rhs, context)
        }
    }
}

/// Unpacks the expressed type as a point collection.
fn unpack_expression(
    expr: &UnrolledExpression,
    _context: &CompileContext,
) -> Result<Vec<UnrolledExpression>, Error> {
    match &expr.ty {
        Type::Point => Ok(vec![expr.clone()]),
        Type::PointCollection(l) => Ok((0..*l)
            .map(|i| UnrolledExpression {
                weight: 1.0, // Weight propagated through `IndexCollecttion`
                data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), i)),
                ty: ty::POINT,
                span: expr.span,
            })
            .collect()),
        ty => Err(Error::cannot_unpack(
            expr.span,
            *ty,
        )),
    }
}

#[derive(Debug, Clone)]
struct PointProperties {
    display: bool,
    label: String,
}

impl PointProperties {
    pub fn parse(props: Option<Properties>, default_label: String) -> Result<Self, Error> {
        Ok(match props {
            Some(mut props) => {
                let display = props.get_bool("display", true)?;
                let label = props.get_string("label", default_label)?;

                PointProperties { display, label }
            }
            None => PointProperties {
                display: true,
                label: default_label,
            },
        })
    }
}

#[derive(Debug, Clone)]
pub struct Properties(HashMap<String, PropertyValue>);

impl Properties {
    fn get_bool(&mut self, property: &str, default: bool) -> Result<bool, Error> {
        self.0
            .remove(property)
            .as_ref()
            .map_or(Ok(default), PropertyValue::as_bool)
    }

    fn get_string(&mut self, property: &str, default: String) -> Result<String, Error> {
        self.0
            .remove(property)
            .as_ref()
            .map_or(Ok(default), PropertyValue::as_string)
    }
}

fn create_variable_named(
    stat: &LetStatement,
    context: &mut CompileContext,
    named: &NamedIdent,
    display: Option<&DisplayProperties>,
    rhs_unrolled: UnrolledExpression,
    variables: &mut Vec<Rc<RefCell<Variable>>>
) -> Result<(), Error> {
    let display: Option<Properties> = display.map(|v| {
        Properties(
            v.properties
                .iter()
                .map(|v| (v.name.ident.clone(), v.value.clone()))
                .collect(),
        )
    });

    match context.variables.entry(named.ident.clone()) {
        // If the variable already exists, it's a redefinition error.
        Entry::Occupied(entry) => Err(Error::redefined_variable(
            entry.get().borrow().definition_span,
            stat.get_span(),
            entry.key().clone(),
        )),
        // Otherwise, create a new variable
        Entry::Vacant(entry) => {
            let rhs_ty = rhs_unrolled.ty;

            let var = Variable {
                name: entry.key().clone(),
                definition_span: stat.get_span(),
                definition: rhs_unrolled,
            };

            let v = Rc::new(RefCell::new(var));
            variables.push(Rc::clone(&v));

            if rhs_ty == Type::Point {
                let props = PointProperties::parse(display, named.ident.clone())?;

                if props.display {
                    context.figure.points.push((
                        variable!(v),
                        PointMeta {
                            letter: props.label.chars().next().unwrap(),
                            primes: 0,
                            index: None
                        }
                    ));
                }
            }

            entry.insert(v);

            Ok(())
        }
    }
}

/// If the lhs of let statement is a point collection, the rhs has to be unpacked.
fn create_variable_collection(
    stat: &LetStatement,
    context: &mut CompileContext,
    col: &PointCollection,
    rhs_unrolled: &UnrolledExpression,
    display: Option<&DisplayProperties>,
    variables: &mut Vec<Rc<RefCell<Variable>>>
) -> Result<(), Error> {
    let display: Option<Properties> = display.map(|v| {
        Properties(
            v.properties
                .iter()
                .map(|v| (v.name.ident.clone(), v.value.clone()))
                .collect(),
        )
    });

    let rhs_unpacked = unpack_expression(rhs_unrolled, context)?;

    if rhs_unpacked.len() != col.len() {
        return Err(Error::CannotUnpack {
            error_span: rhs_unrolled.span,
            ty: rhs_unrolled.ty,
        });
    }

    let pt_letter = if col.collection.len() == 1 {
        let props = PointProperties::parse(
            display,
            construct_point_name(col.collection[0].0, col.collection[0].1),
        )?;

        Some((props.label.chars().next().unwrap(), props.display))
    } else {
        None
    };

    let mut rhs_unpacked = rhs_unpacked.into_iter();
    for pt in &col.collection {
        let id = construct_point_name(pt.0, pt.1);

        match context.variables.entry(id) {
            // If the variable already exists, it's a redefinition error.
            Entry::Occupied(entry) => {
                return Err(Error::redefined_variable(
                    entry.get().borrow().definition_span,
                    stat.get_span(),
                    construct_point_name(pt.0, pt.1),
                ))
            }
            // Otherwise, create a new variable
            Entry::Vacant(entry) => {
                let var = Variable {
                    name: construct_point_name(pt.0, pt.1),
                    definition_span: stat.get_span(),
                    definition: rhs_unpacked.next().unwrap(),
                };

                let var = Rc::new(RefCell::new(var));

                if let Some((letter, true)) = pt_letter {
                    context.figure.points.push((
                        variable!(var),
                        PointMeta {
                            letter,
                            primes: 0,
                            index: None
                        }
                    ));
                }

                variables.push(Rc::clone(&var));
                entry.insert(var);
            }
        }
    }

    Ok(())
}

fn create_variables(
    stat: &LetStatement,
    context: &mut CompileContext,
    library: &Library
) -> Result<Vec<Rc<RefCell<Variable>>>, Error> {
    let mut variables = Vec::new();

    let tree = IterNode::from(&stat.expr);

    let ind = if let Some(iter) = tree.first() {
        if stat.ident.len() == 1 {
            return Err(Error::LetStatUnexpectedIterator {
                var_span: stat.ident.get_span(),
                error_span: iter.span,
            });
        }

        for it in tree.iter() {
            if iter.id != it.id {
                return Err(Error::LetStatMoreThanOneIterator {
                    error_span: stat.expr.get_span(),
                    first_span: iter.span,
                    second_span: it.span,
                });
            }

            for variant in &it.variants {
                if let Some(it2) = variant.first() {
                    return Err(Error::LetStatMoreThanOneIterator {
                        error_span: stat.expr.get_span(),
                        first_span: iter.span,
                        second_span: it2.span,
                    });
                }
            }
        }

        let mut lengths = HashMap::new();
        tree.get_iter_lengths(&mut lengths, stat.expr.get_span())?;
        let entry = lengths.get(&iter.id).unwrap();

        if entry.0 != stat.ident.len() {
            return Err(Error::InconsistentIterators {
                first_span: stat.ident.get_span(),
                first_length: stat.ident.len(),
                occured_span: entry.1,
                occured_length: entry.0,
                error_span: stat.get_span(),
            });
        }

        None
    } else {
        Some(HashMap::new())
    };

    let mut it_index = IterTreeIterator::new(&tree);

    // Iterate over each identifier.
    for def in stat.ident.iter() {
        let rhs_unrolled = unroll_expression(
            &stat.expr,
            context,
            library,
            ind.as_ref()
                .unwrap_or_else(|| it_index.get_currents().unwrap())
        )?;
        it_index.next();

        match &def.name {
            Ident::Named(named) => {
                create_variable_named(
                    stat,
                    context,
                    named,
                    def.display_properties.as_ref(),
                    rhs_unrolled,
                    &mut variables
                )?;
            }
            Ident::Collection(col) => {
                create_variable_collection(
                    stat,
                    context,
                    col,
                    &rhs_unrolled,
                    def.display_properties.as_ref(),
                    &mut variables
                )?;
            }
        }
    }

    Ok(variables)
}

fn unroll_let(
    stat: &LetStatement,
    context: &mut CompileContext,
    library: &Library
) -> Result<(), Error> {
    create_variables(stat, context, library)?;

    // First, we construct an iterator out of lhs
    let lhs: Expression<true> = Expression::ImplicitIterator(ImplicitIterator {
        exprs: Punctuated {
            first: Box::new(SimpleExpression {
                kind: SimpleExpressionKind::Ident(stat.ident.first.name.clone()),
                display: None
            }),
            collection: stat
                .ident
                .collection
                .iter()
                .map(|(p, i)| (*p, SimpleExpression {
                    kind: SimpleExpressionKind::Ident(i.name.clone()),
                    display: None
                }))
                .collect(),
        },
    });

    // Then, we run each rule through a tree iterator.
    for (rule, expr) in &stat.rules {
        let tree = IterNode::from2(&lhs, expr);

        // Check the lengths
        tree.get_iter_lengths(&mut HashMap::new(), stat.get_span())?;

        // And create the index
        let mut index = IterTreeIterator::new(&tree);

        while let Some(it_index) = index.get_currents() {
            unroll_rule(
                unroll_expression(&lhs, context, library, it_index)?,
                rule,
                unroll_expression(expr, context, library, it_index)?,
                context,
                library,
                stat.get_span(),
                false
            )?;

            index.next();
        }
    }

    Ok(())
}

fn unroll_eq(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool
) -> Result<(), Error> {
    if (lhs.ty == ty::collection(2) && rhs.ty == ty::collection(2))
        || (lhs.ty == ty::collection(2) && rhs.ty == ty::SCALAR_UNKNOWN)
        || (lhs.ty == ty::SCALAR_UNKNOWN && rhs.ty == ty::collection(2))
    {
        // AB = CD must have different logic as it's implied that this means "equality of distances".
        let rule = UnrolledRule {
            kind: UnrolledRuleKind::Eq,
            lhs: unroll_implicit_conversion(
                lhs,
                &ty::DISTANCE,
                context
            )?,
            rhs: unroll_implicit_conversion(
                rhs,
                &ty::DISTANCE,
                context
            )?,
            inverted
        };

        context.rules.push(rule);

        Ok(())
    } else {
        let (mut lhs, mut rhs) = (lhs, rhs);
        // If any of the two types can be cast onto the other, cast and compare.
        if rhs.ty.can_cast(&lhs.ty) {
            rhs = unroll_implicit_conversion(rhs, &lhs.ty, context)?;
        } else if lhs.ty.can_cast(&rhs.ty) {
            lhs = unroll_implicit_conversion(lhs, &rhs.ty, context)?;
        } else {
            return Err(Error::InconsistentTypes {
                expected: (lhs.ty, Box::new(lhs.span)),
                got: (rhs.ty, Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });
        }

        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Eq,
            lhs,
            rhs,
            inverted: false,
        });

        Ok(())
    }
}

fn unroll_gt(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool
) -> Result<(), Error> {
    let left_unit = match &lhs.ty {
        Type::Scalar(Some(unit)) => Some(*unit),
        Type::Scalar(None) => None,
        Type::PointCollection(2) => Some(unit::DISTANCE),
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: full_span,
                got: (lhs.ty, lhs.span),
                op: String::from(">"),
            })
        }
    };

    if let Some(ltype) = left_unit {
        if rhs
            .ty
            .can_cast(&Type::Scalar(Some(
                ltype,
            )))
        {
            let rule = UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Scalar(Some(ltype)),
                    context
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Scalar(Some(ltype)),
                    context
                )?,
                inverted,
            };

            context.rules.push(rule);
        } else {
            return Err(Error::InconsistentTypes {
                expected: (lhs.ty, Box::new(lhs.span)),
                got: (rhs.ty, Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });
        }
    } else {
        let right_unit = match &rhs.ty {
            Type::Scalar(Some(unit)) => Some(*unit),
            Type::Scalar(None) => None,
            Type::PointCollection(2) => Some(ComplexUnit::new(SimpleUnit::Distance)),
            _ => {
                return Err(Error::InvalidOperandType {
                    error_span: lhs.span.join(rhs.span),
                    got: (rhs.ty, rhs.span),
                    op: String::from(">"),
                })
            }
        };

        if let Some(rtype) = right_unit {
            let rule = UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Scalar(Some(rtype)),
                    context
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Scalar(Some(rtype)),
                    context
                )?,
                inverted,
            };

            context.rules.push(rule);
        } else {
            let common = Type::Scalar(Some(ComplexUnit::new(SimpleUnit::Scalar)));
            let rule = UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(lhs, &common, context)?,
                rhs: unroll_implicit_conversion(rhs, &common, context)?,
                inverted,
            };

            context.rules.push(rule);
        }
    }

    Ok(())
}

fn unroll_lt(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool
) -> Result<(), Error> {
    let left_unit = match &lhs.ty {
        Type::Scalar(Some(unit)) => Some(*unit),
        Type::Scalar(None) => None,
        Type::PointCollection(2) => Some(ComplexUnit::new(SimpleUnit::Distance)),
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: lhs.span.join(rhs.span),
                got: (lhs.ty, lhs.span),
                op: String::from("<"),
            })
        }
    };

    if let Some(ltype) = left_unit {
        if rhs
            .ty
            .can_cast(&Type::Scalar(Some(
                ltype,
            )))
        {
            let rule = UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Scalar(Some(ltype)),
                    context
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Scalar(Some(ltype)),
                    context
                )?,
                inverted,
            };

            context.rules.push(rule);
        } else {
            return Err(Error::InconsistentTypes {
                expected: (lhs.ty, Box::new(lhs.span)),
                got: (rhs.ty, Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });
        }
    } else {
        let right_unit = match &rhs.ty {
            Type::Scalar(Some(unit)) => Some(*unit),
            Type::Scalar(None) => None,
            Type::PointCollection(2) => Some(ComplexUnit::new(SimpleUnit::Distance)),
            _ => {
                return Err(Error::InvalidOperandType {
                    error_span: full_span,
                    got: (rhs.ty, rhs.span),
                    op: String::from("<"),
                })
            }
        };

        if let Some(rtype) = right_unit {
            let rule = UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Scalar(Some(rtype)),
                    context
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Scalar(Some(rtype)),
                    context
                )?,
                inverted,
            };

            context.rules.push(rule);
        } else {
            let common = ty::SCALAR;
            let rule = UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(lhs, &common, context)?,
                rhs: unroll_implicit_conversion(rhs, &common, context)?,
                inverted,
            };

            context.rules.push(rule);
        }
    }

    Ok(())
}

fn unroll_rule(
    lhs: UnrolledExpression,
    op: &RuleOperator,
    rhs: UnrolledExpression,
    context: &mut CompileContext,
    library: &Library,
    full_span: Span,
    invert: bool
) -> Result<(), Error> {
    match op {
        RuleOperator::Predefined(pre) => match pre {
            PredefinedRuleOperator::Eq(_) => unroll_eq(lhs, rhs, context, full_span, invert),
            PredefinedRuleOperator::Lt(_) => unroll_lt(lhs, rhs, context, full_span, invert),
            PredefinedRuleOperator::Gt(_) => unroll_gt(lhs, rhs, context, full_span, invert),
            PredefinedRuleOperator::Lteq(_) => unroll_gt(lhs, rhs, context, full_span, !invert),
            PredefinedRuleOperator::Gteq(_) => unroll_lt(lhs, rhs, context, full_span, !invert),
        },
        RuleOperator::Defined(op) => {
            let (def, lhs, rhs) = if let Some(func) = library.rule_ops.get(&op.ident.ident) {
                if let Some(overload) = func.get_overload((&lhs.ty, &rhs.ty)) {
                    let lhs = unroll_implicit_conversion(lhs, &overload.params.0, context)?;
                    let rhs = unroll_implicit_conversion(rhs, &overload.params.1, context)?;

                    (&overload.definition, lhs, rhs)
                } else {
                    return Err(Error::overload_not_found(
                        op.ident.span,
                        op.ident.ident.clone(),
                        vec![lhs.ty, rhs.ty],
                    ));
                }
            } else {
                #[allow(clippy::cast_possible_truncation)]
                let suggested = library
                    .rule_ops
                    .iter()
                    .max_by_key(|v| (strsim::jaro(v.0, &op.ident.ident) * 1000.0).floor() as i64)
                    .map(|x| x.0)
                    .cloned();

                return Err(Error::UndefinedFunction {
                    error_span: op.ident.span,
                    function_name: op.ident.ident.clone(),
                    suggested,
                });
            };

            def(&lhs, &rhs, context, None);
            Ok(())
        }
        RuleOperator::Inverted(op) => {
            unroll_rule(lhs, &op.operator, rhs, context, library, full_span, !invert)
        }
    }
}

fn unroll_rulestat(
    rule: &RuleStatement,
    context: &mut CompileContext,
    library: &Library
) -> Result<(), Error> {
    let tree = IterNode::from2(&rule.lhs, &rule.rhs);
    tree.get_iter_lengths(&mut HashMap::new(), rule.get_span())?;

    let mut it_index = IterTreeIterator::new(&tree);

    while let Some(index) = it_index.get_currents() {
        unroll_rule(
            unroll_expression(&rule.lhs, context, library, index)?,
            &rule.op,
            unroll_expression(&rule.rhs, context, library, index)?,
            context,
            library,
            rule.get_span(),
            false
        )?;

        it_index.next();
    }

    Ok(())
}

fn set_flag_bool(flag: &mut Flag, stmt: &FlagStatement) -> Result<(), Error> {
    match &stmt.value {
        super::parser::FlagValue::Set(_) => {
            return Err(Error::BooleanExpected {
                error_span: stmt.get_span(),
            })
        }
        super::parser::FlagValue::Ident(ident) => match &mut flag.kind {
            FlagKind::Setting(s) => match s {
                FlagSetting::Default(_) | FlagSetting::Unset => {
                    *s = FlagSetting::Set(
                        FlagValue::Bool(match ident.ident.as_str() {
                            "enabled" | "on" | "true" => true,
                            "disabled" | "off" | "false" => false,
                            _ => {
                                return Err(Error::BooleanExpected {
                                    error_span: stmt.get_span(),
                                })
                            }
                        }),
                        stmt.get_span(),
                    );
                }
                FlagSetting::Set(_, sp) => {
                    return Err(Error::RedefinedFlag {
                        error_span: stmt.get_span(),
                        first_defined: *sp,
                        flag_name: flag.name.clone(),
                    })
                }
            },
            FlagKind::Set(_) => unreachable!(),
        },
        super::parser::FlagValue::Number(num) => match &mut flag.kind {
            FlagKind::Setting(s) => match s {
                FlagSetting::Default(_) | FlagSetting::Unset => {
                    if num.dot.is_none() {
                        *s = FlagSetting::Set(
                            FlagValue::Bool(match num.integral {
                                1 => true,
                                0 => false,
                                _ => {
                                    return Err(Error::BooleanExpected {
                                        error_span: stmt.get_span(),
                                    })
                                }
                            }),
                            stmt.get_span(),
                        );
                    } else {
                        return Err(Error::BooleanExpected {
                            error_span: stmt.get_span(),
                        });
                    }
                }
                FlagSetting::Set(_, sp) => {
                    return Err(Error::RedefinedFlag {
                        error_span: stmt.get_span(),
                        first_defined: *sp,
                        flag_name: flag.name.clone(),
                    })
                }
            },
            FlagKind::Set(_) => unreachable!(),
        },
    }

    Ok(())
}

fn set_flag(set: &mut FlagSet, flag: &FlagStatement) -> Result<(), Error> {
    let Some(mut flag_ref) = set.get_mut(&flag.name.name.first.ident) else {
        let flag_name = flag.name.name.first.ident.clone();

        #[allow(clippy::cast_possible_truncation)]
        let suggested = set
            .iter()
            .max_by_key(|v| (strsim::jaro(v.0, &flag_name) * 1000.0).floor() as i64)
            .map(|x| x.0)
            .cloned();

        return Err(Error::FlagDoesNotExist {
            flag_name,
            flag_span: flag.name.get_span(),
            error_span: flag.get_span(),
            suggested,
        });
    };

    for part in flag.name.name.iter().skip(1) {
        flag_ref = if let Some(v) = if let FlagKind::Set(set) = &mut flag_ref.kind {
            set.get_mut(&part.ident)
        } else {
            return Err(Error::FlagSetExpected {
                error_span: flag.get_span(),
            });
        } {
            v
        } else {
            let flag_name = part.ident.clone();

            #[allow(clippy::cast_possible_truncation)]
            let suggested = set
                .iter()
                .max_by_key(|v| (strsim::jaro(v.0, &flag_name) * 1000.0).floor() as i64)
                .map(|x| x.0)
                .cloned();

            return Err(Error::FlagDoesNotExist {
                flag_name,
                flag_span: flag.name.get_span(),
                error_span: flag.get_span(),
                suggested,
            });
        };
    }

    match flag_ref.ty {
        FlagType::Set => match &flag.value {
            super::parser::FlagValue::Set(set) => match &mut flag_ref.kind {
                FlagKind::Setting(_) => unreachable!(),
                FlagKind::Set(s) => {
                    for stat in &set.flags {
                        set_flag(s, stat)?;
                    }
                }
            },
            super::parser::FlagValue::Ident(_) | super::parser::FlagValue::Number(_) => {
                return Err(Error::FlagSetExpected {
                    error_span: flag.get_span(),
                })
            }
        },
        FlagType::Boolean => set_flag_bool(flag_ref, flag)?,
        FlagType::String => match &flag.value {
            super::parser::FlagValue::Number(_) | super::parser::FlagValue::Set(_) => {
                return Err(Error::StringExpected {
                    error_span: flag.get_span(),
                })
            }
            super::parser::FlagValue::Ident(ident) => match &mut flag_ref.kind {
                FlagKind::Setting(s) => match s {
                    FlagSetting::Default(_) | FlagSetting::Unset => {
                        *s = FlagSetting::Set(
                            FlagValue::String(ident.ident.clone()),
                            flag.get_span(),
                        );
                    }
                    FlagSetting::Set(_, sp) => {
                        return Err(Error::RedefinedFlag {
                            error_span: flag.get_span(),
                            first_defined: *sp,
                            flag_name: flag_ref.name.clone(),
                        })
                    }
                },
                FlagKind::Set(_) => unreachable!(),
            },
        },
    }

    Ok(())
}

/// Unrolls the given script. All iterators are expanded and all conversions applied. The output can be immediately compiled.
///
/// # Errors
/// Specific error descriptions are in `ScriptError` documentation.
pub fn unroll(input: &str) -> Result<CompileContext, Error> {
    // Unfortunately, due to how context-dependent geoscript is, the code must be compiled immediately after parsing.
    let mut context = CompileContext::new();
    let mut library = Library::new();

    builtins::register(&mut library);
    let library = library; // Disable mutation

    let tokens = token::tokenize(input)?;
    let mut it = tokens.iter().peekable();

    let mut statements = Vec::new();

    while it.peek().is_some() {
        statements.push(Statement::parse(&mut it)?);
    }

    for flag in statements.iter().filter_map(Statement::as_flag) {
        set_flag(&mut context.flags, flag)?;
    }

    for stat in statements {
        // Unroll the statement
        match stat {
            Statement::Noop(_) | Statement::Flag(_) => (),
            Statement::Let(stat) => unroll_let(&stat, &mut context, &library)?,
            Statement::Rule(stat) => unroll_rulestat(&stat, &mut context, &library)?,
        }
    }

    // for v in context.variables.values() {
    //     println!("let {} = {}", v.name, v.definition);
    // }

    // for x in &unrolled {
    //     println!("{x}");
    // }

    Ok(context)
}
