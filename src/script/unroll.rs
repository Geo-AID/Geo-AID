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
use std::fmt::Formatter;
use std::mem;
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    rc::Rc,
    write,
};

use crate::span;

use crate::generator::fast_float::FastFloat;
use crate::script::builtins::macros::{distance, index, line2};
use crate::script::ty;
pub use context::{
    Circle as EntCircle, CompileContext, Definition, Entity, Line as EntLine, Point as EntPoint,
    Scalar as EntScalar,
};

use super::figure::MathString;
use super::parser;
use super::token::{LSquare, RSquare};
use super::{
    builtins,
    parser::{
        BinaryOperator, DisplayProperties, ExplicitIterator, Expression, FlagStatement,
        ImplicitIterator, LetStatement, Parse, PredefinedRuleOperator, PropertyValue, Punctuated,
        RuleOperator, RuleStatement, SimpleExpression, SimpleExpressionKind, Statement, Type,
    },
    token::{self, Ident, NamedIdent, PointCollection as PCToken, Span},
    unit, ComplexUnit, Error,
};

mod context;
mod figure;

pub use figure::{
    Node, PointNode, CircleNode, CollectionNode, EmptyNode
};

/// A definition for a user-defined rule operator.
#[derive(Debug)]
pub struct RuleOperatorDefinition {
    /// Operator's name.
    pub name: String,
}

/// A variable created with a let statement.
#[derive(Debug)]
pub struct Variable<T: Displayed> {
    /// Variable's name
    pub name: String,
    /// Variable's definition span.
    pub definition_span: Span,
    /// Variable's definition.
    pub definition: Expr<T>,
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
    pub definition: RuleDefinition,
}

/// geoscript rule declaration
type GeoRule = dyn Fn(&AnyExpr, &AnyExpr, &mut CompileContext, Properties, bool) -> Box<dyn Node>;

/// A function definition.
pub struct RuleDefinition(pub Box<GeoRule>);

impl Debug for RuleDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
    pub fn match_params(expected: (&Type, &Type), received: (&Type, &Type)) -> bool {
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
type GeoFunc = dyn Fn(&[AnyExpr], &mut CompileContext, Properties) -> AnyExpr;

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

/// The library of all rules and functions available in geoscript.
#[derive(Debug, Default)]
pub struct Library {
    /// Functions
    pub functions: HashMap<String, Function>,
    /// The rule operators.
    pub rule_ops: HashMap<String, Rc<Rule>>,
}

impl Library {
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            rule_ops: HashMap::new(),
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
            SimpleExpressionKind::Ident(_) | SimpleExpressionKind::Number(_) => {
                IterNode::new(Vec::new())
            }
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
        node.extend(Self::from(e2).0);
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
                            occurred_span: iter.span,
                            occurred_length: iter.variants.len(),
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
            currents: [0].repeat(l),
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
    PointEq(Expr<Point>, Expr<Point>),
    ScalarEq(Expr<Scalar>, Expr<Scalar>),
    Gt(Expr<Scalar>, Expr<Scalar>),
    Lt(Expr<Scalar>, Expr<Scalar>),
    Alternative(Vec<UnrolledRule>),
}

pub trait ConvertFrom<T>: Displayed {
    /// # Errors
    /// Returns an error if the conversion is invalid.
    fn convert_from(value: T) -> Result<Expr<Self>, Error>;
}

impl<T: Displayed> ConvertFrom<Expr<T>> for T {
    fn convert_from(value: Expr<T>) -> Result<Expr<Self>, Error> {
        Ok(value)
    }
}

pub trait Convert
where
    Self: Sized,
{
    /// # Errors
    /// Returns an error if the conversion is invalid.
    fn convert<T: ConvertFrom<Self>>(self) -> Result<Expr<T>, Error>;
}

impl<T> Convert for T {
    fn convert<U: ConvertFrom<Self>>(self) -> Result<Expr<U>, Error> {
        U::convert_from(self)
    }
}

pub trait GetValueType {
    fn get_value_type(&self) -> Type;
}

pub trait Simplify {
    #[must_use]
    fn simplify(&self, context: &CompileContext) -> Self;
}

macro_rules! impl_from_any {
    ($what:ident) => {
        impl ConvertFrom<AnyExpr> for $what {
            fn convert_from(value: AnyExpr) -> Result<Expr<Self>, Error> {
                match value {
                    AnyExpr::Point(v) => v.convert(),
                    AnyExpr::Line(v) => v.convert(),
                    AnyExpr::Scalar(v) => v.convert(),
                    AnyExpr::Circle(v) => v.convert(),
                    AnyExpr::PointCollection(v) => v.convert(),
                    AnyExpr::Bundle(v) => v.convert(),
                }
            }
        }
    };
}

macro_rules! convert_err {
    ($from:ident($v:expr) -> $to:ident) => {
        Err(Error::ImplicitConversionDoesNotExist {
            error_span: ($v).span,
            from: ($v).data.get_value_type(),
            to: Self::get_type(),
        })
    };
}

macro_rules! impl_convert_err {
    ($from:ident -> $to:ident) => {
        impl ConvertFrom<Expr<$from>> for $to {
            fn convert_from(value: Expr<$from>) -> Result<Expr<Self>, Error> {
                convert_err!($from(value) -> $to)
            }
        }
    }
}

macro_rules! impl_make_variable {
    ($what:ident) => {
        impl Expr<$what> {
            #[must_use]
            pub fn make_variable(self, name: String) -> Self {
                let sp = self.span;

                Expr {
                    span: sp,
                    weight: FastFloat::One,
                    data: Rc::new($what::Generic(Generic::VariableAccess(Rc::new(
                        RefCell::new(Variable {
                            name,
                            definition: self,
                            definition_span: sp,
                        }),
                    )))),
                    node: None // Variable references are NEVER displayed
                }
            }
        }
    };
    ($what:ident { other: $other:ident, data: $data:ident }) => {
        impl Expr<$what> {
            #[must_use]
            pub fn make_variable(self, name: String) -> Self {
                let sp = self.span;

                Expr {
                    span: sp,
                    weight: FastFloat::One,
                    data: Rc::new($what {
                        $other: self.data.$other,
                        data: $data::Generic(Generic::VariableAccess(Rc::new(RefCell::new(
                            Variable {
                                name,
                                definition: self,
                                definition_span: sp,
                            },
                        )))),
                    }),
                    node: None // Variable references are NEVER displayed
                }
            }
        }
    };
}

macro_rules! impl_any_from_x {
    ($what:ident) => {
        impl From<Expr<$what>> for AnyExpr {
            fn from(value: Expr<$what>) -> Self {
                AnyExpr::$what(value)
            }
        }
    };
}

#[derive(Debug, Definition)]
pub enum Generic<T>
where
    T: Definition + Displayed,
{
    VariableAccess(#[def(variable)] Rc<RefCell<Variable<T>>>),
    Boxed(Expr<T>),
}

impl<T: Display + Definition + Displayed> Display for Generic<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableAccess(name) => write!(f, "{}", name.borrow().name),
            Self::Boxed(expr) => {
                write!(f, "{expr}")
            }
        }
    }
}

#[derive(Debug, Definition)]
pub enum Point {
    Generic(Generic<Self>),
    Entity(#[def(entity)] usize),
    Average(#[def(sequence)] Vec<Expr<Point>>),
    LineLineIntersection(Expr<Line>, Expr<Line>),
    CircleCenter(Expr<Circle>),
}

impl Point {
    #[must_use]
    pub fn get_type() -> Type {
        Type::Point
    }
}

impl CloneWithNode {}

impl Displayed for Point {
    type Node = CollectionNode;
}

impl Simplify for Expr<Point> {
    fn simplify(&self, context: &CompileContext) -> Expr<Point> {
        match self.data.as_ref() {
            Point::Generic(generic) => match generic {
                Generic::VariableAccess(var) => var.borrow().definition.simplify(context),
                Generic::Boxed(expr) => expr.simplify(context),
            },
            Point::Entity(index) => context.get_point_by_index(*index),
            Point::CircleCenter(circ) => match circ.simplify(context).data.as_ref() {
                Circle::Circle(center, _) => center.simplify(context),
                _ => unreachable!(),
            },
            Point::Average(_) | Point::LineLineIntersection(_, _) => self.clone(),
        }
    }
}

impl Expr<Point> {
    #[must_use]
    pub fn boxed(mut self, weight: FastFloat, span: Span) -> Self {
        let node = self.node.take();

        Self {
            weight,
            span,
            data: Rc::new(Point::Generic(Generic::Boxed(self))),
            node
        }
    }
}

impl GetValueType for Point {
    fn get_value_type(&self) -> Type {
        ty::POINT
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}"),
            Self::Average(exprs) => write!(
                f,
                "average({})",
                exprs
                    .iter()
                    .map(|v| format!("{v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Entity(i) => write!(f, "Point {i}"),
            Self::LineLineIntersection(l1, l2) => {
                write!(f, "intersection({l1}, {l2})")
            }
            Self::CircleCenter(circle) => {
                write!(f, "{circle}.center")
            }
        }
    }
}

impl_from_any! {Point}
impl_convert_err! {Circle -> Point}
impl_convert_err! {Line -> Point}
impl_convert_err! {Bundle -> Point}
impl_convert_err! {Scalar -> Point}

impl_make_variable! {Point}

impl ConvertFrom<Expr<PointCollection>> for Point {
    fn convert_from(value: Expr<PointCollection>) -> Result<Expr<Self>, Error> {
        if value.data.length == 1 {
            Ok(index!(value, 0))
        } else {
            convert_err!(PointCollection(value) -> Point)
        }
    }
}

#[derive(Debug, Clone, Definition)]
pub enum Circle {
    Generic(Generic<Self>),
    Entity(#[def(entity)] usize),
    Circle(Expr<Point>, Expr<Scalar>), // Center, radius
}

impl_from_any! {Circle}

impl_convert_err! {Point -> Circle}
impl_convert_err! {Line -> Circle}
impl_convert_err! {Bundle -> Circle}
impl_convert_err! {Scalar -> Circle}
impl_convert_err! {PointCollection -> Circle}

impl_make_variable! {Circle}

impl Circle {
    #[must_use]
    pub fn get_type() -> Type {
        ty::CIRCLE
    }
}

impl Displayed for Circle {
    type Node = CollectionNode;
}

impl Simplify for Expr<Circle> {
    fn simplify(&self, context: &CompileContext) -> Self {
        match self.data.as_ref() {
            Circle::Generic(generic) => match generic {
                Generic::VariableAccess(var) => var.borrow().definition.simplify(context),
                Generic::Boxed(expr) => expr.simplify(context),
            },
            Circle::Entity(index) => context.get_circle_by_index(*index),
            Circle::Circle(_, _) => self.clone(),
        }
    }
}

impl Expr<Circle> {
    #[must_use]
    pub fn boxed(mut self, weight: FastFloat, span: Span) -> Self {
        let node = self.node.take();

        Self {
            weight,
            span,
            data: Rc::new(Circle::Generic(Generic::Boxed(self))),
            node
        }
    }
}

impl GetValueType for Circle {
    fn get_value_type(&self) -> Type {
        ty::CIRCLE
    }
}

impl Display for Circle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}"),
            Self::Entity(i) => write!(f, "Entity {i}"),
            Self::Circle(center, radius) => {
                write!(f, "circle({center}, {radius})")
            }
        }
    }
}

#[derive(Debug, Clone, Definition)]
pub enum Line {
    Generic(Generic<Self>),
    Entity(#[def(entity)] usize),
    LineFromPoints(Expr<Point>, Expr<Point>),
    AngleBisector(Expr<Point>, Expr<Point>, Expr<Point>),
    PerpendicularThrough(Expr<Line>, Expr<Point>), // Line, Point
    ParallelThrough(Expr<Line>, Expr<Point>),      // Line, Point
}

impl Line {
    #[must_use]
    pub fn get_type() -> Type {
        ty::LINE
    }
}

impl Displayed for Line {
    type Node = CollectionNode;
}

impl Simplify for Expr<Line> {
    fn simplify(&self, context: &CompileContext) -> Expr<Line> {
        match self.data.as_ref() {
            Line::Generic(generic) => match generic {
                Generic::VariableAccess(var) => var.borrow().definition.simplify(context),
                Generic::Boxed(expr) => expr.simplify(context),
            },
            Line::Entity(index) => context.get_line_by_index(*index),
            Line::LineFromPoints(_, _)
            | Line::AngleBisector(_, _, _)
            | Line::PerpendicularThrough(_, _)
            | Line::ParallelThrough(_, _) => self.clone(),
        }
    }
}

impl Expr<Line> {
    #[must_use]
    pub fn boxed(mut self, weight: FastFloat, span: Span) -> Self {
        let node = self.node.take();

        Self {
            weight,
            span,
            data: Rc::new(Line::Generic(Generic::Boxed(self))),
            node
        }
    }
}

impl_from_any! {Line}
impl_convert_err! {Bundle -> Line}
impl_convert_err! {Circle -> Line}
impl_convert_err! {Point -> Line}
impl_convert_err! {Scalar -> Line}

impl_make_variable! {Line}

impl ConvertFrom<Expr<PointCollection>> for Line {
    fn convert_from(value: Expr<PointCollection>) -> Result<Expr<Self>, Error> {
        if value.data.length == 2 {
            Ok(line2!(index!(value, 0), index!(value, 1)))
        } else {
            convert_err!(PointCollection(value) -> Line)
        }
    }
}

impl GetValueType for Line {
    fn get_value_type(&self) -> Type {
        ty::LINE
    }
}

impl Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}"),
            Self::Entity(i) => write!(f, "Entity {i}"),
            Self::LineFromPoints(e1, e2) => write!(f, "line({e1}, {e2})"),
            Self::AngleBisector(e1, e2, e3) => {
                write!(f, "angle-bisector({e1}, {e2}, {e3})")
            }
            Self::PerpendicularThrough(l, p) => {
                write!(f, "perpendicular-through({l}, {p})")
            }
            Self::ParallelThrough(l, p) => {
                write!(f, "parallel-through({l}, {p})")
            }
        }
    }
}

#[derive(Debug, Clone, Definition)]
pub enum ScalarData {
    Generic(Generic<Scalar>),
    Entity(#[def(entity)] usize),
    Number(#[def(no_entity)] f64),
    DstLiteral(#[def(no_entity)] f64),
    SetUnit(Expr<Scalar>, #[def(no_entity)] ComplexUnit),
    PointPointDistance(Expr<Point>, Expr<Point>),
    PointLineDistance(Expr<Point>, Expr<Line>),
    Negate(Expr<Scalar>),
    Add(Expr<Scalar>, Expr<Scalar>),
    Subtract(Expr<Scalar>, Expr<Scalar>),
    Multiply(Expr<Scalar>, Expr<Scalar>),
    Divide(Expr<Scalar>, Expr<Scalar>),
    ThreePointAngle(Expr<Point>, Expr<Point>, Expr<Point>),
    ThreePointAngleDir(Expr<Point>, Expr<Point>, Expr<Point>), // Directed angle
    TwoLineAngle(Expr<Line>, Expr<Line>),
    Average(#[def(sequence)] Vec<Expr<Scalar>>),
    CircleRadius(Expr<Circle>),
}

impl Display for ScalarData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}"),
            Self::Average(exprs) => write!(
                f,
                "average({})",
                exprs
                    .iter()
                    .map(|v| format!("{v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Number(num) => write!(f, "{num}"),
            Self::DstLiteral(num) => write!(f, "lit {num}"),
            Self::Entity(i) => write!(f, "Entity {i}"),
            Self::SetUnit(expr, _) => {
                write!(f, "{expr}")
            }
            Self::PointPointDistance(e1, e2) => write!(f, "dst({e1}, {e2})"),
            Self::PointLineDistance(e1, e2) => write!(f, "dst({e1}, {e2})"),
            Self::Negate(e) => write!(f, "-{e}"),
            Self::Add(e1, e2) => write!(f, "{e1} + {e2}"),
            Self::Multiply(e1, e2) => write!(f, "{e1} * {e2}"),
            Self::Divide(e1, e2) => write!(f, "{e1} / {e2}"),
            Self::Subtract(e1, e2) => write!(f, "{e1} - {e2}"),
            Self::ThreePointAngle(e1, e2, e3) => {
                write!(f, "angle({e1}, {e2}, {e3})")
            }
            Self::ThreePointAngleDir(e1, e2, e3) => {
                write!(f, "dir_angle({e1}, {e2}, {e3})")
            }
            Self::TwoLineAngle(e1, e2) => write!(f, "angle({e1}, {e2})"),
            Self::CircleRadius(circle) => {
                write!(f, "{circle}.radius")
            }
        }
    }
}

#[derive(Debug, Clone, Definition)]
pub struct Scalar {
    pub unit: Option<ComplexUnit>,
    #[def(entity)]
    pub data: ScalarData,
}

impl_from_any! {Scalar}

impl_convert_err! {Point -> Scalar}
impl_convert_err! {Circle -> Scalar}
impl_convert_err! {Line -> Scalar}
impl_convert_err! {Bundle -> Scalar}

impl_make_variable! {Scalar {other: unit, data: ScalarData}}

impl Displayed for Scalar {
    type Node = CollectionNode;
}

impl ConvertFrom<Expr<PointCollection>> for Scalar {
    fn convert_from(value: Expr<PointCollection>) -> Result<Expr<Self>, Error> {
        if value.data.length == 2 {
            Ok(distance!(PP: index!(value, 0), index!(value, 1)))
        } else {
            convert_err!(PointCollection(value) -> Scalar)
        }
    }
}

impl Scalar {
    #[must_use]
    pub fn get_type() -> Type {
        ty::SCALAR
    }
}

impl Expr<Scalar> {
    #[must_use]
    pub fn boxed(self, weight: FastFloat, span: Span) -> Self {
        Self {
            weight,
            span,
            data: Rc::new(Scalar {
                unit: self.data.unit,
                data: ScalarData::Generic(Generic::Boxed(self)),
            }),
        }
    }

    /// # Errors
    /// Returns a conversion error if it is invalid.
    ///
    /// Only valid conversions are None to another unit or self to self
    pub fn convert_unit(self, unit: Option<ComplexUnit>) -> Result<Self, Error> {
        let err = Err(Error::ImplicitConversionDoesNotExist {
            error_span: self.span,
            from: self.get_value_type(),
            to: Type::Scalar(unit),
        });

        if self.data.unit == unit {
            Ok(self)
        } else if unit.is_none() || self.data.unit.is_some() && self.data.unit != unit {
            err
        } else {
            // `unit` is concrete and self.unit is not
            Ok(Self {
                weight: self.weight,
                span: self.span,
                data: Rc::new(Scalar {
                    unit,
                    data: match &self.data.data {
                        ScalarData::Generic(generic) => match generic {
                            Generic::VariableAccess(_) => unreachable!(), // Always concrete
                            Generic::Boxed(v) => {
                                ScalarData::Generic(Generic::Boxed(v.clone().convert_unit(unit)?))
                            }
                        },
                        v @ (ScalarData::Entity(_) | ScalarData::Number(_)) => v.clone(),
                        ScalarData::DstLiteral(_)
                        | ScalarData::PointPointDistance(_, _)
                        | ScalarData::PointLineDistance(_, _)
                        | ScalarData::ThreePointAngle(_, _, _)
                        | ScalarData::ThreePointAngleDir(_, _, _)
                        | ScalarData::TwoLineAngle(_, _)
                        | ScalarData::CircleRadius(_)
                        | ScalarData::SetUnit(_, _) => unreachable!(), // Always concrete
                        ScalarData::Negate(v) => ScalarData::Negate(v.clone().convert_unit(unit)?),
                        ScalarData::Add(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Add(
                                a.clone().convert_unit(unit)?,
                                b.clone().convert_unit(unit)?,
                            )
                        }
                        ScalarData::Subtract(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Subtract(
                                a.clone().convert_unit(unit)?,
                                b.clone().convert_unit(unit)?,
                            )
                        }
                        ScalarData::Multiply(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Multiply(
                                a.clone().convert_unit(unit)?,
                                b.clone().convert_unit(Some(unit::SCALAR))?,
                            )
                        }
                        ScalarData::Divide(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Divide(
                                a.clone().convert_unit(unit)?,
                                b.clone().convert_unit(Some(unit::SCALAR))?,
                            )
                        }
                        ScalarData::Average(exprs) => {
                            // All will be unit-less.
                            ScalarData::Average(
                                exprs
                                    .iter()
                                    .map(|v| v.clone().convert_unit(unit))
                                    .collect::<Result<Vec<_>, Error>>()?,
                            )
                        }
                    },
                }),
            })
        }
    }

    /// # Panics
    /// If bugged
    #[must_use]
    pub fn specify_unit(self) -> Self {
        if self.data.unit.is_none() {
            self.convert_unit(Some(unit::SCALAR)).unwrap()
        } else {
            self
        }
    }
}

impl GetValueType for Scalar {
    fn get_value_type(&self) -> Type {
        Type::Scalar(self.unit)
    }
}

impl Display for Scalar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

#[derive(Debug, Clone, Definition)]
pub enum PointCollectionData {
    Generic(Generic<PointCollection>),
    PointCollection(#[def(sequence)] Vec<Expr<Point>>),
}

impl PointCollectionData {
    #[must_use]
    pub fn as_collection(&self) -> Option<&Vec<Expr<Point>>> {
        match self {
            PointCollectionData::PointCollection(v) => Some(v),
            PointCollectionData::Generic(_) => None,
        }
    }

    /// # Panics
    /// Panics if the collection isn't long enough
    #[must_use]
    pub fn index(&self, index: usize) -> Expr<Point> {
        match self {
            PointCollectionData::Generic(generic) => match &generic {
                Generic::VariableAccess(var) => var.borrow().definition.index(index),
                Generic::Boxed(expr) => expr.index(index),
            },
            PointCollectionData::PointCollection(col) => col.get(index).unwrap().clone(),
        }
    }
}

impl Display for PointCollectionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}"),
            Self::PointCollection(col) => write!(
                f,
                "col({})",
                col.iter()
                    .map(|v| format!("{v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Clone, Definition)]
pub struct PointCollection {
    pub length: usize,
    #[def(entity)]
    pub data: PointCollectionData,
}

impl_from_any! {PointCollection}

impl_convert_err! {Circle -> PointCollection}
impl_convert_err! {Line -> PointCollection}
impl_convert_err! {Scalar -> PointCollection}
impl_convert_err! {Bundle -> PointCollection}

impl_make_variable! {PointCollection {other: length, data: PointCollectionData}}

impl Displayed for PointCollection {
    type Node = CollectionNode;
}

impl ConvertFrom<Expr<Point>> for PointCollection {
    fn convert_from(value: Expr<Point>) -> Result<Expr<Self>, Error> {
        Ok(Expr {
            weight: FastFloat::One,
            span: value.span,
            data: Rc::new(PointCollection {
                length: 1,
                data: PointCollectionData::PointCollection(vec![value]),
            }),
        })
    }
}

impl PointCollection {
    #[must_use]
    pub fn get_type() -> Type {
        ty::collection(0)
    }
}

impl Expr<PointCollection> {
    #[must_use]
    pub fn boxed(self, weight: FastFloat, span: Span) -> Self {
        Self {
            weight,
            span,
            data: Rc::new(PointCollection {
                length: self.data.length,
                data: PointCollectionData::Generic(Generic::Boxed(self)),
            }),
        }
    }

    /// # Errors
    /// Returns an error if lengths don't match up.
    pub fn check_len(self, length: usize) -> Result<Self, Error> {
        if self.data.length == length || length == 0 {
            Ok(self)
        } else {
            Err(Error::ImplicitConversionDoesNotExist {
                error_span: self.span,
                from: self.get_value_type(),
                to: ty::collection(length),
            })
        }
    }

    #[must_use]
    pub fn index(&self, index: usize) -> Expr<Point> {
        self.data.data.index(index)
    }
}

impl GetValueType for PointCollection {
    fn get_value_type(&self) -> Type {
        ty::collection(self.length)
    }
}

impl Display for PointCollection {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

#[derive(Debug, Clone, Definition)]
pub enum BundleData {
    Generic(Generic<Bundle>),
    ConstructBundle(#[def(map)] HashMap<String, AnyExpr>),
}

impl Display for BundleData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}"),
            Self::ConstructBundle(fields) => {
                write!(
                    f,
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|(field, value)| format!("{field}: {value}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, Definition)]
pub struct Bundle {
    pub name: &'static str,
    #[def(entity)]
    pub data: BundleData,
}

impl_from_any! {Bundle}

impl_convert_err! {Point -> Bundle}
impl_convert_err! {Line -> Bundle}
impl_convert_err! {Circle -> Bundle}
impl_convert_err! {Scalar -> Bundle}
impl_convert_err! {PointCollection -> Bundle}

impl_make_variable! {Bundle {other: name, data: BundleData}}

impl Bundle {
    #[must_use]
    pub fn get_type() -> Type {
        ty::bundle("{}")
    }

    /// # Panics
    /// Panics if the field doesn't exists
    #[must_use]
    pub fn index(&self, field: &str) -> AnyExpr {
        match &self.data {
            BundleData::Generic(generic) => match &generic {
                Generic::VariableAccess(var) => var.borrow().definition.index(field),
                Generic::Boxed(expr) => expr.index(field),
            },
            BundleData::ConstructBundle(map) => map.get(field).unwrap().clone(),
        }
    }
}

impl Displayed for Bundle {
    type Node = CollectionNode;
}

impl Expr<Bundle> {
    #[must_use]
    pub fn boxed(self, weight: FastFloat, span: Span) -> Self {
        Self {
            weight,
            span,
            data: Rc::new(Bundle {
                name: self.data.name,
                data: BundleData::Generic(Generic::Boxed(self)),
            }),
        }
    }

    #[must_use]
    pub fn index(&self, field: &str) -> AnyExpr {
        self.data.index(field)
    }

    /// # Errors
    /// Returns an error if the bundle names don't match
    pub fn check_name(self, name: &'static str) -> Result<Self, Error> {
        if self.data.name == name {
            Ok(self)
        } else {
            Err(Error::ImplicitConversionDoesNotExist {
                error_span: self.span,
                from: self.get_value_type(),
                to: ty::bundle(name),
            })
        }
    }
}

impl GetValueType for Bundle {
    fn get_value_type(&self) -> Type {
        ty::bundle(self.name)
    }
}

impl Display for Bundle {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.name, self.data)
    }
}

#[derive(Debug, Clone, Definition)]
pub enum AnyExpr {
    Point(Expr<Point>),
    Line(Expr<Line>),
    Scalar(Expr<Scalar>),
    Circle(Expr<Circle>),
    PointCollection(Expr<PointCollection>),
    Bundle(Expr<Bundle>),
}

impl_any_from_x! {Point}
impl_any_from_x! {Line}
impl_any_from_x! {Scalar}
impl_any_from_x! {Circle}
impl_any_from_x! {PointCollection}
impl_any_from_x! {Bundle}

impl AnyExpr {
    /// Clones the expression and sets its own span. This will be treated as a separate expression now.
    #[must_use]
    pub fn get_span_mut(&mut self) -> &mut Span {
        match self {
            Self::Point(v) => &mut v.span,
            Self::Line(v) => &mut v.span,
            Self::Scalar(v) => &mut v.span,
            Self::Circle(v) => &mut v.span,
            Self::PointCollection(v) => &mut v.span,
            Self::Bundle(v) => &mut v.span,
        }
    }

    /// Clones the expression and sets its own span. This will be treated as a separate expression now.
    #[must_use]
    pub fn get_span(&self) -> Span {
        match self {
            Self::Point(v) => v.span,
            Self::Line(v) => v.span,
            Self::Scalar(v) => v.span,
            Self::Circle(v) => v.span,
            Self::PointCollection(v) => v.span,
            Self::Bundle(v) => v.span,
        }
    }

    #[must_use]
    pub fn as_point(&self) -> Option<&Expr<Point>> {
        match self {
            Self::Point(v) => Some(v),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_type(&self) -> Type {
        match self {
            AnyExpr::Point(v) => v.get_value_type(),
            AnyExpr::Line(v) => v.get_value_type(),
            AnyExpr::Scalar(v) => v.get_value_type(),
            AnyExpr::Circle(v) => v.get_value_type(),
            AnyExpr::PointCollection(v) => v.get_value_type(),
            AnyExpr::Bundle(v) => v.get_value_type(),
        }
    }

    /// # Errors
    /// Returns an error if the conversion is invalid.
    pub fn convert_to(self, to: Type) -> Result<Self, Error> {
        Ok(match to {
            Type::Point => Self::Point(self.convert()?),
            Type::Line => Self::Line(self.convert()?),
            Type::Scalar(unit) => Self::Scalar(self.convert()?.convert_unit(unit)?),
            Type::PointCollection(len) => Self::PointCollection(self.convert()?.check_len(len)?),
            Type::Circle => Self::Circle(self.convert()?),
            Type::Bundle(name) => Self::Bundle(self.convert()?.check_name(name)?),
            Type::Undefined => unreachable!(),
        })
    }

    #[must_use]
    pub fn boxed(self, weight: FastFloat, span: Span) -> Self {
        match self {
            Self::Point(v) => Self::Point(v.boxed(weight, span)),
            Self::Line(v) => Self::Line(v.boxed(weight, span)),
            Self::Scalar(v) => Self::Scalar(v.boxed(weight, span)),
            Self::Circle(v) => Self::Circle(v.boxed(weight, span)),
            Self::PointCollection(v) => Self::PointCollection(v.boxed(weight, span)),
            Self::Bundle(v) => Self::Bundle(v.boxed(weight, span)),
        }
    }

    /// # Panics
    /// Panics if not a variable.
    #[must_use]
    pub fn get_variable_span(&self) -> Span {
        match self {
            Self::Point(v) => match v.data.as_ref() {
                Point::Generic(Generic::VariableAccess(var)) => var.borrow().definition_span,
                _ => panic!("not a variable"),
            },
            Self::Line(v) => match v.data.as_ref() {
                Line::Generic(Generic::VariableAccess(var)) => var.borrow().definition_span,
                _ => panic!("not a variable"),
            },
            Self::Scalar(v) => match &v.data.data {
                ScalarData::Generic(Generic::VariableAccess(var)) => var.borrow().definition_span,
                _ => panic!("not a variable"),
            },
            Self::Circle(v) => match v.data.as_ref() {
                Circle::Generic(Generic::VariableAccess(var)) => var.borrow().definition_span,
                _ => panic!("not a variable"),
            },
            Self::PointCollection(v) => match &v.data.data {
                PointCollectionData::Generic(Generic::VariableAccess(var)) => {
                    var.borrow().definition_span
                }
                _ => panic!("not a variable"),
            },
            Self::Bundle(v) => match &v.data.data {
                BundleData::Generic(Generic::VariableAccess(var)) => var.borrow().definition_span,
                _ => panic!("not a variable"),
            },
        }
    }

    #[must_use]
    pub fn make_variable(self, name: String) -> Self {
        match self {
            AnyExpr::Point(v) => AnyExpr::Point(v.make_variable(name)),
            AnyExpr::Line(v) => AnyExpr::Line(v.make_variable(name)),
            AnyExpr::Scalar(v) => AnyExpr::Scalar(v.make_variable(name)),
            AnyExpr::Circle(v) => AnyExpr::Circle(v.make_variable(name)),
            AnyExpr::PointCollection(v) => AnyExpr::PointCollection(v.make_variable(name)),
            AnyExpr::Bundle(v) => AnyExpr::Bundle(v.make_variable(name)),
        }
    }
}

impl Display for AnyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyExpr::Point(v) => write!(f, "{v}"),
            AnyExpr::Line(v) => write!(f, "{v}"),
            AnyExpr::Scalar(v) => write!(f, "{v}"),
            AnyExpr::Circle(v) => write!(f, "{v}"),
            AnyExpr::PointCollection(v) => write!(f, "{v}"),
            AnyExpr::Bundle(v) => write!(f, "{v}"),
        }
    }
}

pub fn display_vec<T: Display>(v: &[T]) -> String {
    v.iter()
        .map(|x| format!("{x}"))
        .collect::<Vec<String>>()
        .join(", ")
}

pub trait Displayed {
    type Node: Node;
}

pub trait CloneWithNode {
    #[must_use]
    fn clone_with_node(&mut self) -> Self;

    #[must_use]
    fn clone_without_node(&self) -> Self;
}

impl<T: Clone> CloneWithNode for T {
    fn clone_with_node(&mut self) -> Self {
        self.clone()
    }

    fn clone_without_node(&self) -> Self {
        self.clone()
    }
}

#[derive(Debug)]
pub struct Expr<T: ?Sized + Displayed> {
    pub data: Rc<T>,
    pub span: Span,
    pub weight: FastFloat, // Assigned weight.
    pub node: Option<T::Node>
}

impl<T: ?Sized + Displayed> CloneWithNode for Expr<T> {
    fn clone_with_node(&mut self) -> Self {
        Self {
            data: Rc::clone(&self.data),
            span: self.span,
            weight: self.weight,
            node: self.node.take()
        }
    }

    fn clone_without_node(&self) -> Self {
        Self {
            data: Rc::clone(&self.data),
            span: self.span,
            weight: self.weight,
            node: None
        }
    }
}

impl<T: GetValueType + Displayed> GetValueType for Expr<T> {
    fn get_value_type(&self) -> Type {
        self.data.get_value_type()
    }
}

impl<T: Definition + Displayed> Definition for Expr<T> {
    fn order(&self, context: &CompileContext) -> usize {
        self.data.order(context)
    }

    fn contains_entity(&self, entity: usize, context: &CompileContext) -> bool {
        self.data.contains_entity(entity, context)
    }
}

impl<T: Display + Displayed> Display for Expr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl<T: Clone + Displayed> Expr<T> {
    pub fn new_spanless(data: T) -> Self {
        Self {
            span: span!(0, 0, 0, 0),
            data: Rc::new(data),
            weight: FastFloat::One,
        }
    }

    #[must_use]
    pub fn clone_with_weight(&self, weight: FastFloat) -> Self {
        let mut cloned = self.clone();
        cloned.weight = weight;
        cloned
    }
}

#[derive(Debug)]
pub struct UnrolledRule {
    pub kind: UnrolledRuleKind,
    pub inverted: bool,
}

impl Display for UnrolledRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inv = if self.inverted { "!" } else { "" };

        match &self.kind {
            UnrolledRuleKind::PointEq(a, b) => write!(f, "{a} {inv}= {b}"),
            UnrolledRuleKind::ScalarEq(a, b) => write!(f, "{a} {inv}= {b}"),
            UnrolledRuleKind::Gt(a, b) => {
                write!(f, "{a} {} {b}", if self.inverted { "<=" } else { ">" })
            }
            UnrolledRuleKind::Lt(a, b) => {
                write!(f, "{a} {} {b}", if self.inverted { ">=" } else { "<" })
            }
            UnrolledRuleKind::Alternative(v) => {
                write!(
                    f,
                    "{}",
                    v.iter()
                        .map(|x| format!("{x}"))
                        .collect::<Vec<String>>()
                        .join(" || ")
                )
            }
        }
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
    params: &[AnyExpr],
    display: Properties,
    context: &mut CompileContext
) -> AnyExpr {
    definition(params, context, display)
}

fn fetch_variable(
    context: &CompileContext,
    name: &str,
    variable_span: Span,
) -> Result<AnyExpr, Error> {
    let mut var = context
        .variables
        .get(name)
        .ok_or_else(|| {
            #[allow(clippy::cast_possible_truncation)]
            let suggested = context
                .variables
                .iter()
                .max_by_key(|v| (strsim::jaro(v.0, name) * 1000.0).floor() as i64)
                .map(|x| x.0)
                .cloned();

            Error::UndefinedVariable {
                error_span: variable_span,
                variable_name: name.to_string(),
                suggested,
            }
        })?
        .clone();

    *var.get_span_mut() = variable_span;
    Ok(var)
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
#[allow(clippy::too_many_lines)]
fn unroll_simple(
    expr: &SimpleExpression,
    context: &mut CompileContext,
    library: &Library,
    it_index: &HashMap<u8, usize>,
    external_display: Option<&DisplayProperties>
) -> Result<AnyExpr, Error> {
    let display = Properties::from(DisplayProperties::merge(expr.display.clone(), external_display.cloned()));

    Ok(match &expr.kind {
        SimpleExpressionKind::Ident(i) => match i {
            Ident::Named(named) => fetch_variable(context, &named.ident, named.span)?,
            Ident::Collection(col) => AnyExpr::PointCollection(Expr {
                weight: FastFloat::One, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
                data: Rc::new(PointCollection {
                    length: col.collection.len(),
                    data: PointCollectionData::PointCollection(
                        col.collection
                            .iter()
                            .map(|item| {
                                fetch_variable(
                                    context,
                                    &format!("{item}"),
                                    col.span,
                                )
                                .and_then(|var| var.convert::<Point>())
                            })
                            .collect::<Result<Vec<Expr<Point>>, Error>>()?,
                    ),
                }),
                span: col.span,
            }),
        },
        SimpleExpressionKind::Number(num) => AnyExpr::Scalar(Expr {
            weight: FastFloat::Zero, // Always zero.
            data: Rc::new(Scalar {
                unit: None,
                data: ScalarData::Number(num.value.to_float()),
            }),
            span: num.get_span(),
        }),
        SimpleExpressionKind::Call(e) => {
            let params = match &e.params {
                Some(params) => params
                    .iter()
                    .map(|p| unroll_expression(p, context, library, it_index, None))
                    .collect::<Result<Vec<AnyExpr>, Error>>()?,
                None => Vec::new(),
            };

            if let Some(func) = library.functions.get(&e.name.ident) {
                let param_types = params.iter().map(AnyExpr::get_type).collect();

                if let Some(overload) = func.get_overload(&param_types) {
                    let params = params
                        .into_iter()
                        .enumerate()
                        .map(|(i, param)| {
                            if i < overload.params.len() {
                                param.convert_to(overload.params[i])
                            } else {
                                param.convert_to(overload.param_group.unwrap())
                            }
                        })
                        .collect::<Result<Vec<AnyExpr>, Error>>()?;

                    let ret = unroll_parameters(&overload.definition, &params, display, context);

                    // TODO: UPDATE FOR WEIGHING SUPPORT
                    ret.boxed(FastFloat::One, e.get_span())
                } else {
                    return Err(Error::OverloadNotFound {
                        error_span: e.get_span(),
                        function_name: e.name.ident.clone(),
                        params: param_types,
                    });
                }
            } else {
                #[allow(clippy::cast_possible_truncation)]
                let suggested = library
                    .functions
                    .iter()
                    .map(|v| {
                        (
                            v.0,
                            v.1,
                            (strsim::jaro(v.0, &e.name.ident) * 1000.0).floor() as i64,
                        )
                    })
                    .filter(|v| v.2 > 600)
                    .max_by_key(|v| v.2)
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
            let unrolled: Expr<Scalar> =
                unroll_simple(&op.rhs, context, library, it_index, external_display)?.convert()?;

            AnyExpr::Scalar(Expr {
                weight: FastFloat::One,
                span: expr.get_span(),
                data: Rc::new(Scalar {
                    unit: unrolled.data.unit,
                    data: ScalarData::Negate(unrolled),
                }),
            })
        }
        SimpleExpressionKind::Parenthised(expr) => {
            unroll_expression(&expr.content, context, library, it_index, None)?
        }
        SimpleExpressionKind::ExplicitIterator(it) => unroll_expression(
            it.get(it_index[&it.id]).unwrap(),
            context,
            library,
            it_index,
            None
        )?,
        SimpleExpressionKind::PointCollection(col) => AnyExpr::PointCollection(Expr {
            weight: FastFloat::One, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
            span: col.get_span(),
            data: Rc::new(PointCollection {
                length: col.points.len(),
                data: PointCollectionData::PointCollection(
                    col.points
                        .iter()
                        .map(|expr| {
                            unroll_expression(expr, context, library, it_index, None)?
                                .convert()
                                .map_err(|v| {
                                    let err = v.as_implicit_does_not_exist().unwrap();
                                    Error::NonPointInPointCollection {
                                        error_span: col.get_span(),
                                        received: (*err.0, *err.1), // (span, from)
                                    }
                                })
                        })
                        .collect::<Result<Vec<Expr<Point>>, Error>>()?,
                ),
            }),
        }),
    })
}

fn unroll_binop(lhs: &AnyExpr, op: &BinaryOperator, rhs: &AnyExpr) -> Result<AnyExpr, Error> {
    let full_span = lhs.get_span().join(rhs.get_span());

    let lhs = lhs
        .clone()
        .convert::<Scalar>()
        .map_err(|_| Error::InvalidOperandType {
            error_span: full_span,
            got: (lhs.get_type(), lhs.get_span()),
            op: op.to_string(),
        })?;

    let rhs = rhs
        .clone()
        .convert::<Scalar>()
        .map_err(|_| Error::InvalidOperandType {
            error_span: full_span,
            got: (rhs.get_type(), rhs.get_span()),
            op: op.to_string(),
        })?;

    match op {
        BinaryOperator::Add(_) | BinaryOperator::Sub(_) => {
            let lhs = if lhs.data.unit.is_none() {
                lhs.convert_unit(rhs.data.unit)?
            } else {
                lhs
            };

            let rhs = rhs.convert_unit(lhs.data.unit)?;

            Ok(AnyExpr::Scalar(Expr {
                weight: FastFloat::One, // Technically, the only way to assign weight to an arithmetic op is to put it in parentheses it.
                span: full_span,
                data: Rc::new(Scalar {
                    unit: rhs.data.unit,
                    data: match op {
                        BinaryOperator::Add(_) => ScalarData::Add(lhs, rhs),
                        BinaryOperator::Sub(_) => ScalarData::Subtract(lhs, rhs),
                        _ => unreachable!(),
                    },
                }),
            }))
        }
        BinaryOperator::Mul(_) | BinaryOperator::Div(_) => {
            let lhs = lhs.specify_unit();

            let rhs = rhs.specify_unit();

            Ok(AnyExpr::Scalar(Expr {
                weight: FastFloat::One, // Technically, the only way to assign weight to an arithmetic op is to put it in parentheses.
                span: full_span,
                data: Rc::new(Scalar {
                    unit: Some(lhs.data.unit.unwrap() * rhs.data.unit.as_ref().unwrap()),
                    data: match op {
                        BinaryOperator::Mul(_) => ScalarData::Multiply(lhs, rhs),
                        BinaryOperator::Div(_) => ScalarData::Divide(lhs, rhs),
                        _ => unreachable!(),
                    },
                }),
            }))
        }
    }
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
fn unroll_expression<const ITER: bool>(
    expr: &Expression<ITER>,
    context: &mut CompileContext,
    library: &Library,
    it_index: &HashMap<u8, usize>,
    external_display: Option<&DisplayProperties>
) -> Result<AnyExpr, Error> {
    match expr {
        Expression::Single(simple) => {
            unroll_simple(simple.as_ref(), context, library, it_index, external_display)
        }
        Expression::ImplicitIterator(it) => unroll_simple(
            it.get(it_index[&0]).unwrap(), // Implicit iterators always have an id of 0.
            context,
            library,
            it_index,
            external_display
        ),
        Expression::Binop(op) => {
            let lhs = unroll_expression(&op.lhs, context, library, it_index, None)?;
            let rhs = unroll_expression(&op.rhs, context, library, it_index, None)?;

            unroll_binop(&lhs, &op.operator, &rhs)
        }
    }
}

// /// Unpacks the expressed type as a point collection.
// fn unpack_expression(
//     expr: &AnyExpr,
//     _context: &CompileContext,
// ) -> Result<Vec<Expr<Point>>, Error> {
//     match expr {
//
//         Type::Point => Ok(vec![expr.clone()]),
//         Type::PointCollection(l) => Ok((0..*l)
//             .map(|i| Expr {
//                 weight: FastFloat::One, // Weight propagated through `IndexCollecttion`
//                 data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), i)),
//                 ty: ty::POINT,
//                 span: expr.span,
//             })
//             .collect()),
//         ty => Err(Error::cannot_unpack(
//             expr.span,
//             *ty,
//         )),
//     }
// }

#[derive(Debug, Clone)]
pub struct Properties(HashMap<String, PropertyValue>);

impl Properties {
    fn get_bool(&mut self, property: &str) -> Result<Property<bool>, Error> {
        self.0
            .remove(property)
            .as_ref()
            .map_or(
                Ok(Property { value: None }),
                |x| Ok(Property {
                    value: Some(RealProperty {
                        value: x.as_bool()?,
                        span: x.get_span()
                    })
                })
            )
    }

    fn get_string(&mut self, property: &str) -> Result<Property<String>, Error> {
        self.0
            .remove(property)
            .as_ref()
            .map_or(
                Ok(Property { value: None }),
                |x| Ok(Property {
                    value: Some(RealProperty {
                        value: x.as_string()?,
                        span: x.get_span()
                    })
                })
            )
    }
}

impl From<Option<DisplayProperties>> for Properties {
    fn from(value: Option<DisplayProperties>) -> Self {
        Self(
            value.into_iter().flat_map(
                |x| x.properties.into_iter()
            )
                .map(|v| (v.name.ident.clone(), v.value.clone()))
                .collect(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct Property<T> {
    value: Option<RealProperty<T>>
}

impl<T> Property<T> {
    #[must_use]
    pub fn get(self) -> Option<T> {
        self.value.map(|x| x.value)
    }

    #[must_use]
    pub fn get_or(self, default: T) -> T {
        self.value.map(|x| x.value).unwrap_or(default)
    }

    #[must_use]
    pub fn get_prop(self) -> Option<RealProperty<T>> {
        self.value
    }

    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        self.value.as_ref().map(|x| x.span)
    }
}

#[derive(Debug, Clone)]
pub struct RealProperty<T> {
    pub value: T,
    pub span: Span
}

fn create_variable_named(
    stat: &LetStatement,
    context: &mut CompileContext,
    named: &NamedIdent,
    rhs_unrolled: AnyExpr,
    variables: &mut Vec<AnyExpr>
) -> Result<(), Error> {
    match context.variables.entry(named.ident.clone()) {
        // If the variable already exists, it's a redefinition error.
        Entry::Occupied(entry) => Err(Error::RedefinedVariable {
            defined_at: entry.get().get_variable_span(),
            error_span: stat.get_span(),
            variable_name: entry.key().clone(),
        }),
        // Otherwise, create a new variable
        Entry::Vacant(entry) => {
            let var = rhs_unrolled.make_variable(entry.key().clone());
            variables.push(var.clone());

            entry.insert(var);

            Ok(())
        }
    }
}

/// If the lhs of let statement is a point collection, the rhs has to be unpacked.
fn create_variable_collection(
    stat: &LetStatement,
    context: &mut CompileContext,
    col: &PCToken,
    rhs_unrolled: &AnyExpr,
    variables: &mut Vec<AnyExpr>
) -> Result<(), Error> {
    // let display = Properties::from(display);

    let rhs_unpacked = rhs_unrolled.clone().convert::<PointCollection>()?;

    if rhs_unpacked.data.length != col.len() {
        return Err(Error::CannotUnpack {
            error_span: rhs_unrolled.get_span(),
            ty: rhs_unrolled.get_type(),
        });
    }

    // let pt_node = if col.collection.len() == 1 {
    //     Some(FigureNode::new_point(
    //         rhs_unpacked.index(0),
    //         display,
    //         IdentOrItem::PointCollectionItem(col.collection[0].clone())
    //     )?)
    // } else {
    //     None
    // };

    let mut rhs_unpacked = rhs_unpacked.data.data.as_collection().unwrap().iter().cloned();
    for pt in &col.collection {
        let id = format!("{pt}");

        match context.variables.entry(id.clone()) {
            // If the variable already exists, it's a redefinition error.
            Entry::Occupied(entry) => {
                return Err(Error::RedefinedVariable {
                    defined_at: entry.get().get_variable_span(),
                    error_span: stat.get_span(),
                    variable_name: id,
                })
            }
            // Otherwise, create a new variable
            Entry::Vacant(entry) => {
                let var = rhs_unpacked
                    .next()
                    .unwrap()
                    .make_variable(entry.key().clone());

                // if let Some(node) = pt_node {
                //     todo!()
                // }

                let var = AnyExpr::Point(var);
                variables.push(var.clone());
                entry.insert(var);
            }
        }
    }

    Ok(())
}

fn create_variables(
    stat: LetStatement,
    context: &mut CompileContext,
    library: &Library
) -> Result<Vec<AnyExpr>, Error> {
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
                occurred_span: entry.1,
                occurred_length: entry.0,
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
        let default_label = match &def.name {
            Ident::Named(named) => MathString::parse(&named.ident, named.span).ok(),
            Ident::Collection(col) => if col.len() == 1 {
                Some(MathString::from(col.collection[0].clone()))
            } else {
                None
            },
        }.and_then(|x| x.displayed_by_default()).unwrap_or_else(|| MathString::new(span!(0, 0, 0, 0)));

        let display = DisplayProperties::merge(
            def.display_properties.clone(),
            Some(DisplayProperties {
                lsquare: LSquare { span: span!(0, 0, 0, 0) },
                properties: Punctuated {
                    first: Box::new(parser::Property {
                        name: NamedIdent {
                            span: span!(0, 0, 0, 0),
                            ident: String::from("default-label"),
                            collection_likeness: 0.0
                        },
                        eq: token::Eq { span: span!(0, 0, 0, 0) },
                        value: PropertyValue::MathString(default_label)
                    }),
                    collection: Vec::new()
                },
                rsquare: RSquare { span: span!(0, 0, 0, 0) }
            })
        );

        let rhs_unrolled = unroll_expression(
            &stat.expr,
            context,
            library,
            ind.as_ref()
                .unwrap_or_else(|| it_index.get_currents().unwrap()),
            display.as_ref()
        )?;
        it_index.next();

        match &def.name {
            Ident::Named(named) => {
                create_variable_named(
                    &stat,
                    context,
                    named,
                    rhs_unrolled,
                    &mut variables
                )?;
            }
            Ident::Collection(col) => {
                create_variable_collection(
                    &stat,
                    context,
                    col,
                    &rhs_unrolled,
                    &mut variables
                )?;
            }
        }
    }

    Ok(variables)
}

fn unroll_let(
    mut stat: LetStatement,
    context: &mut CompileContext,
    library: &Library
) -> Result<(), Error> {
    // First, we construct an iterator out of lhs
    let lhs: Expression<true> = Expression::ImplicitIterator(ImplicitIterator {
        exprs: Punctuated {
            first: Box::new(SimpleExpression {
                kind: SimpleExpressionKind::Ident(stat.ident.first.name.clone()),
                display: None,
            }),
            collection: stat
                .ident
                .collection
                .iter()
                .map(|(p, i)| {
                    (
                        *p,
                        SimpleExpression {
                            kind: SimpleExpressionKind::Ident(i.name.clone()),
                            display: None,
                        },
                    )
                })
                .collect(),
        },
    });

    let stat_span = stat.get_span();
    let rules = mem::take(&mut stat.rules);
    
    create_variables(stat, context, library)?;

    // Then, we run each rule through a tree iterator.
    for (rule, expr) in rules {
        let tree = IterNode::from2(&lhs, &expr);

        // Check the lengths
        tree.get_iter_lengths(&mut HashMap::new(), stat_span)?;

        // And create the index
        let mut index = IterTreeIterator::new(&tree);

        while let Some(it_index) = index.get_currents() {
            unroll_rule(
                unroll_expression(&lhs, context, library, it_index, None)?,
                &rule,
                unroll_expression(&expr, context, library, it_index, None)?,
                context,
                library,
                stat_span,
                false
            )?;

            index.next();
        }
    }

    Ok(())
}

fn unroll_eq(
    lhs: AnyExpr,
    rhs: AnyExpr,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool
) -> Result<(), Error> {
    let lhs_type = lhs.get_type();
    let rhs_type = rhs.get_type();

    if (lhs_type == ty::collection(2) && rhs_type == ty::collection(2))
        || (lhs_type == ty::collection(2) && rhs_type == ty::SCALAR_UNKNOWN)
        || (lhs_type == ty::SCALAR_UNKNOWN && rhs_type == ty::collection(2))
    {
        // AB = CD must have different logic as it's implied that this means "equality of distances".
        let rule = UnrolledRule {
            kind: UnrolledRuleKind::ScalarEq(
                lhs.convert()?.convert_unit(Some(unit::DISTANCE))?,
                rhs.convert()?.convert_unit(Some(unit::DISTANCE))?,
            ),
            inverted,
        };

        context.rules.push(rule);

        Ok(())
    } else {
        // If any of the two types can be cast onto the other, cast and compare.
        let (lhs, rhs) = if let Ok(rhs) = rhs.clone().convert_to(lhs.get_type()) {
            (lhs, rhs)
        } else if let Ok(lhs) = lhs.clone().convert_to(rhs.get_type()) {
            (lhs, rhs)
        } else {
            return Err(Error::InconsistentTypes {
                expected: (lhs.get_type(), Box::new(lhs.get_span())),
                got: (rhs.get_type(), Box::new(rhs.get_span())),
                error_span: Box::new(full_span),
            });
        };

        match rhs.get_type() {
            Type::Point => context.rules.push(UnrolledRule {
                kind: UnrolledRuleKind::PointEq(lhs.convert()?, rhs.convert()?),
                inverted,
            }),
            Type::Scalar(_) => context.rules.push(UnrolledRule {
                kind: UnrolledRuleKind::ScalarEq(
                    lhs.convert()?.specify_unit(),
                    rhs.convert()?.specify_unit(),
                ),
                inverted,
            }),
            ty => {
                return Err(Error::ComparisonDoesNotExist {
                    error_span: full_span,
                    ty,
                })
            }
        }

        Ok(())
    }
}

fn unroll_gt(
    lhs: Expr<Scalar>,
    rhs: Expr<Scalar>,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool,
) -> Result<(), Error> {
    if lhs.data.unit.is_some() {
        let rhs =
            rhs.clone()
                .convert_unit(lhs.data.unit)
                .map_err(|_| Error::InconsistentTypes {
                    expected: (lhs.get_value_type(), Box::new(lhs.span)),
                    got: (rhs.get_value_type(), Box::new(rhs.span)),
                    error_span: Box::new(full_span),
                })?;

        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Gt(lhs, rhs),
            inverted,
        });
    } else if rhs.data.unit.is_some() {
        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Gt(lhs.convert_unit(rhs.data.unit)?, rhs),
            inverted,
        });
    } else {
        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Gt(
                lhs.convert_unit(Some(unit::SCALAR))?,
                rhs.convert_unit(Some(unit::SCALAR))?,
            ),
            inverted,
        });
    }

    Ok(())
}

fn unroll_lt(
    lhs: Expr<Scalar>,
    rhs: Expr<Scalar>,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool,
) -> Result<(), Error> {
    if lhs.data.unit.is_some() {
        let rhs =
            rhs.clone()
                .convert_unit(lhs.data.unit)
                .map_err(|_| Error::InconsistentTypes {
                    expected: (lhs.get_value_type(), Box::new(lhs.span)),
                    got: (rhs.get_value_type(), Box::new(rhs.span)),
                    error_span: Box::new(full_span),
                })?;

        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Lt(lhs, rhs),
            inverted,
        });
    } else if rhs.data.unit.is_some() {
        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Lt(lhs.convert_unit(rhs.data.unit)?, rhs),
            inverted,
        });
    } else {
        context.rules.push(UnrolledRule {
            kind: UnrolledRuleKind::Lt(
                lhs.convert_unit(Some(unit::SCALAR))?,
                rhs.convert_unit(Some(unit::SCALAR))?,
            ),
            inverted,
        });
    }

    Ok(())
}

fn unroll_rule(
    lhs: AnyExpr,
    op: &RuleOperator,
    rhs: AnyExpr,
    context: &mut CompileContext,
    library: &Library,
    full_span: Span,
    invert: bool
) -> Result<(), Error> {
    match op {
        RuleOperator::Predefined(pre) => match pre {
            PredefinedRuleOperator::Eq(_) => unroll_eq(lhs, rhs, context, full_span, invert),
            PredefinedRuleOperator::Lt(_) => {
                unroll_lt(lhs.convert()?, rhs.convert()?, context, full_span, invert)
            }
            PredefinedRuleOperator::Gt(_) => {
                unroll_gt(lhs.convert()?, rhs.convert()?, context, full_span, invert)
            }
            PredefinedRuleOperator::Lteq(_) => {
                unroll_gt(lhs.convert()?, rhs.convert()?, context, full_span, !invert)
            }
            PredefinedRuleOperator::Gteq(_) => {
                unroll_lt(lhs.convert()?, rhs.convert()?, context, full_span, !invert)
            }
        },
        RuleOperator::Defined(op) => {
            let (def, lhs, rhs) = if let Some(func) = library.rule_ops.get(&op.ident.ident) {
                if let Some(overload) = func.get_overload((&lhs.get_type(), &rhs.get_type())) {
                    let lhs = lhs.convert_to(overload.params.0)?;
                    let rhs = rhs.convert_to(overload.params.1)?;

                    (&overload.definition, lhs, rhs)
                } else {
                    return Err(Error::OverloadNotFound {
                        error_span: op.ident.span,
                        function_name: op.ident.ident.clone(),
                        params: vec![lhs.get_type(), rhs.get_type()],
                    });
                }
            } else {
                #[allow(clippy::cast_possible_truncation)]
                let suggested = library
                    .rule_ops
                    .iter()
                    .map(|v| {
                        (
                            v.0,
                            v.1,
                            (strsim::jaro(v.0, &op.ident.ident) * 1000.0).floor() as i64,
                        )
                    })
                    .filter(|v| v.2 > 600)
                    .max_by_key(|v| v.2)
                    .map(|x| x.0)
                    .cloned();

                return Err(Error::UndefinedFunction {
                    error_span: op.ident.span,
                    function_name: op.ident.ident.clone(),
                    suggested,
                });
            };

            def(&lhs, &rhs, context, Properties::from(None), invert);
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
            unroll_expression(&rule.lhs, context, library, index, None)?,
            &rule.op,
            unroll_expression(&rule.rhs, context, library, index, None)?,
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
pub fn unroll(input: &str) -> Result<(CompileContext, CollectionNode), Error> {
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

    let figure = CollectionNode::new();

    for stat in statements {
        // Unroll the statement
        match stat {
            Statement::Noop(_) | Statement::Flag(_) => (),
            Statement::Let(stat) => unroll_let(stat, &mut context, &library)?,
            Statement::Rule(stat) => unroll_rulestat(&stat, &mut context, &library)?,
        }
    }

    // for v in context.variables.values() {
    //     println!("let {} = {}", v.name, v.definition);
    // }

    // for x in &unrolled {
    //     println!("{x}");
    // }

    Ok((context, figure))
}
