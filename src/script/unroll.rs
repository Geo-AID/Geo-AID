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

use geo_aid_derive::{Definition, CloneWithNode};
use std::fmt::Formatter;
use std::mem;
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    rc::Rc,
    hash::Hash,
    write,
};

use crate::span;

use crate::generator::fast_float::FastFloat;
use crate::script::builtins::macros::index;
use crate::script::ty;
pub use context::{
    Circle as EntCircle, CompileContext, Definition, Entity, Line as EntLine, Point as EntPoint,
    Scalar as EntScalar
};

use self::figure::FromExpr;

use super::figure::MathString;
use super::parser::PropGetValue;
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
    Node, PointNode, CircleNode, CollectionNode, EmptyNode, PCNode, BundleNode, AnyExprNode, HierarchyNode,
    AssociatedData, BuildAssociated, LineNode, MaybeUnset, ScalarNode
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
type GeoRule = dyn Fn(AnyExpr, AnyExpr, &mut CompileContext, Properties, bool);

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
type GeoFunc = dyn Fn(Vec<AnyExpr>, &mut CompileContext, Properties) -> AnyExpr;

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
            .map_or(Type::Unknown, |x| x.returned_type)
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
    Display // No content, used only for displaying stuff
}

pub trait ConvertFrom<T>: Displayed {
    /// # Errors
    /// Returns an error if the conversion is invalid.
    fn convert_from(value: T, context: &CompileContext) -> Expr<Self>;

    fn can_convert_from(value: &T) -> bool; 
}

impl<T: Displayed> ConvertFrom<Expr<T>> for T {
    fn convert_from(value: Expr<T>, _context: &CompileContext) -> Expr<Self> {
        value
    }

    fn can_convert_from(_value: &Expr<T>) -> bool {
        true
    }
}

pub trait Convert
where
    Self: Sized,
{
    /// # Errors
    /// Returns an error if the conversion is invalid.
    fn convert<T: ConvertFrom<Self>>(self, context: &CompileContext) -> Expr<T>;

    /// # Errors
    /// Returns an error if the conversion is invalid.
    fn can_convert<T: ConvertFrom<Self>>(&self) -> bool;
}

impl<T> Convert for T {
    fn convert<U: ConvertFrom<Self>>(self, context: &CompileContext) -> Expr<U> {
        U::convert_from(self, context)
    }

    fn can_convert<U: ConvertFrom<Self>>(&self) -> bool {
        U::can_convert_from(self)        
    }
}

pub trait GetValueType {
    fn get_value_type(&self) -> Type;
}

pub trait Simplify {
    /// Simlpifies the expression. WARNING: DOES NOT PRESERVE NODE.
    #[must_use]
    fn simplify(&self, context: &CompileContext) -> Self;
}

macro_rules! impl_from_any {
    ($what:ident) => {
        impl ConvertFrom<AnyExpr> for $what {
            fn convert_from(value: AnyExpr, context: &CompileContext) -> Expr<Self> {
                match value {
                    AnyExpr::Point(v) => v.convert(context),
                    AnyExpr::Line(v) => v.convert(context),
                    AnyExpr::Scalar(v) => v.convert(context),
                    AnyExpr::Circle(v) => v.convert(context),
                    AnyExpr::PointCollection(v) => v.convert(context),
                    AnyExpr::Bundle(v) => v.convert(context),
                    AnyExpr::Unknown(v) => v.convert(context)
                }
            }

            fn can_convert_from(value: &AnyExpr) -> bool {
                match value {
                    AnyExpr::Point(v) => Self::can_convert_from(v),
                    AnyExpr::Line(v) => Self::can_convert_from(v),
                    AnyExpr::Scalar(v) => Self::can_convert_from(v),
                    AnyExpr::Circle(v) => Self::can_convert_from(v),
                    AnyExpr::PointCollection(v) => Self::can_convert_from(v),
                    AnyExpr::Bundle(v) => Self::can_convert_from(v),
                    AnyExpr::Unknown(v) => Self::can_convert_from(v)
                } 
            }
        }
    };
}

macro_rules! convert_err {
    ($from:ident($v:expr) -> $to:ident with $context:ident) => {{
        let v = $v;

        $context.push_error(Error::ImplicitConversionDoesNotExist {
            error_span: v.span,
            from: v.data.get_value_type(),
            to: Self::get_type(),
        });

        Expr {
            span: v.span,
            weight: FastFloat::One,
            data: Rc::new($to::dummy()),
            node: None
        }
    }};
}

macro_rules! impl_convert_err {
    ($from:ident -> $to:ident) => {
        impl ConvertFrom<Expr<$from>> for $to {
            fn convert_from(value: Expr<$from>, context: &CompileContext) -> Expr<Self> {
                convert_err!($from(value) -> $to with context)
            }

            fn can_convert_from(_value: &Expr<$from>) -> bool {
                false
            }
        }
    }
}

macro_rules! impl_from_unknown {
    {$what:ident} => {
        impl ConvertFrom<Expr<Unknown>> for $what {
            fn convert_from(value: Expr<Unknown>, _context: &CompileContext) -> Expr<Self> {
                Expr {
                    span: value.span,
                    weight: FastFloat::One,
                    data: Rc::new($what::dummy()),
                    node: None
                }
            }

            fn can_convert_from(_value: &Expr<Unknown>) -> bool {
                true
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

#[derive(Debug, Definition, CloneWithNode)]
pub enum Generic<T>
where
    T: Definition + Displayed,
{
    VariableAccess(#[def(variable)] Rc<RefCell<Variable<T>>>),
    Boxed(Expr<T>),
    /// Dummy is a value specifically for continued unrolling after error occurence.
    /// It should never show up in compilation step.
    Dummy 
}

impl<T: Display + Definition + Displayed> Display for Generic<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableAccess(name) => write!(f, "{}", name.borrow().name),
            Self::Boxed(expr) => {
                write!(f, "{expr}")
            }
            Self::Dummy => write!(f, "invalid (dummy)")
        }
    }
}

#[derive(Debug, Definition, CloneWithNode)]
pub enum Point {
    Generic(Generic<Self>),
    Entity(#[def(entity)] usize),
    Average(#[def(sequence)] ClonedVec<Expr<Point>>),
    LineLineIntersection(Expr<Line>, Expr<Line>),
    CircleCenter(Expr<Circle>),
}

impl Point {
    #[must_use]
    pub fn get_type() -> Type {
        Type::Point
    }

    #[must_use]
    pub fn dummy() -> Self {
        Self::Generic(Generic::Dummy)
    }
}

impl Displayed for Point {
    type Node = PointNode;
}

impl Simplify for Expr<Point> {
    fn simplify(&self, context: &CompileContext) -> Expr<Point> {
        match self.data.as_ref() {
            Point::Generic(generic) => match generic {
                Generic::VariableAccess(var) => var.borrow().definition.simplify(context),
                Generic::Boxed(expr) => expr.simplify(context),
                Generic::Dummy => self.clone_without_node()
            },
            Point::Entity(index) => context.get_point_by_index(*index),
            Point::CircleCenter(circ) => match circ.simplify(context).data.as_ref() {
                Circle::Circle(center, _) => center.simplify(context),
                _ => unreachable!(),
            },
            Point::Average(_) | Point::LineLineIntersection(_, _) => self.clone_without_node(),
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
impl_from_unknown! {Point}
impl_convert_err! {Circle -> Point}
impl_convert_err! {Line -> Point}
impl_convert_err! {Bundle -> Point}
impl_convert_err! {Scalar -> Point}

impl_make_variable! {Point}

impl ConvertFrom<Expr<PointCollection>> for Point {
    fn convert_from(mut value: Expr<PointCollection>, context: &CompileContext) -> Expr<Self> {
        if value.data.length == 1 {
            index!(node value, 0)
        } else {
            convert_err!(PointCollection(value) -> Point with context)
        }
    }

    fn can_convert_from(value: &Expr<PointCollection>) -> bool {
        value.data.length == 1
    }
}

#[derive(Debug, CloneWithNode, Definition)]
pub enum Circle {
    Generic(Generic<Self>),
    Entity(#[def(entity)] usize),
    Circle(Expr<Point>, Expr<Scalar>), // Center, radius
}

impl_from_any! {Circle}
impl_from_unknown! {Circle}

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

    #[must_use]
    pub const fn dummy() -> Self {
        Self::Generic(Generic::Dummy)
    }
}

impl Displayed for Circle {
    type Node = CircleNode;
}

impl Simplify for Expr<Circle> {
    fn simplify(&self, context: &CompileContext) -> Self {
        match self.data.as_ref() {
            Circle::Generic(generic) => match generic {
                Generic::VariableAccess(var) => var.borrow().definition.simplify(context),
                Generic::Boxed(expr) => expr.simplify(context),
                Generic::Dummy => self.clone_without_node()
            },
            Circle::Entity(index) => context.get_circle_by_index(*index),
            Circle::Circle(_, _) => self.clone_without_node(),
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

#[derive(Debug, CloneWithNode, Definition)]
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

    #[must_use]
    pub const fn dummy() -> Self {
        Self::Generic(Generic::Dummy)
    }
}

impl Displayed for Line {
    type Node = LineNode;
}

impl Simplify for Expr<Line> {
    fn simplify(&self, context: &CompileContext) -> Expr<Line> {
        match self.data.as_ref() {
            Line::Generic(generic) => match generic {
                Generic::VariableAccess(var) => var.borrow().definition.simplify(context),
                Generic::Boxed(expr) => expr.simplify(context),
                Generic::Dummy => self.clone_without_node()
            },
            Line::Entity(index) => context.get_line_by_index(*index),
            Line::LineFromPoints(_, _)
            | Line::AngleBisector(_, _, _)
            | Line::PerpendicularThrough(_, _)
            | Line::ParallelThrough(_, _) => self.clone_without_node(),
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
impl_from_unknown! {Line}
impl_convert_err! {Bundle -> Line}
impl_convert_err! {Circle -> Line}
impl_convert_err! {Point -> Line}
impl_convert_err! {Scalar -> Line}

impl_make_variable! {Line}

impl ConvertFrom<Expr<PointCollection>> for Line {
    fn convert_from(mut value: Expr<PointCollection>, context: &CompileContext) -> Expr<Self> {
        if value.data.length == 2 {
            context.line(index!(node value, 0), index!(node value, 1))
        } else {
            convert_err!(PointCollection(value) -> Line with context)
        }
    }

    fn can_convert_from(value: &Expr<PointCollection>) -> bool {
        value.data.length == 2
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

#[derive(Debug, CloneWithNode, Definition)]
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
    Average(#[def(sequence)] ClonedVec<Expr<Scalar>>),
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

#[derive(Debug, CloneWithNode, Definition)]
pub struct Scalar {
    pub unit: Option<ComplexUnit>,
    #[def(entity)]
    pub data: ScalarData,
}

impl_from_any! {Scalar}
impl_from_unknown! {Scalar}

impl_convert_err! {Point -> Scalar}
impl_convert_err! {Circle -> Scalar}
impl_convert_err! {Line -> Scalar}
impl_convert_err! {Bundle -> Scalar}

impl_make_variable! {Scalar {other: unit, data: ScalarData}}

impl Displayed for Scalar {
    type Node = ScalarNode;
}

impl ConvertFrom<Expr<PointCollection>> for Scalar {
    fn convert_from(mut value: Expr<PointCollection>, context: &CompileContext) -> Expr<Self> {
        if value.data.length == 2 {
            context.distance_pp(index!(node value, 0), index!(node value, 1))
        } else {
            convert_err!(PointCollection(value) -> Scalar with context)
        }
    }

    fn can_convert_from(value: &Expr<PointCollection>) -> bool {
        value.data.length == 2
    }
}

impl Scalar {
    #[must_use]
    pub fn get_type() -> Type {
        ty::SCALAR
    }

    #[must_use]
    pub const fn dummy() -> Self {
        Self {
            unit: None,
            data: ScalarData::Generic(Generic::Dummy)
        }
    }
}

impl Expr<Scalar> {
    #[must_use]
    pub fn boxed(mut self, weight: FastFloat, span: Span) -> Self {
        let node = self.node.take();

        Self {
            weight,
            span,
            data: Rc::new(Scalar {
                unit: self.data.unit,
                data: ScalarData::Generic(Generic::Boxed(self)),
            }),
            node
        }
    }

    /// # Errors
    /// Returns a conversion error if it is invalid.
    ///
    /// Only valid conversions are None to another unit or self to self
    pub fn convert_unit(mut self, unit: Option<ComplexUnit>, context: &CompileContext) -> Self {
        let err = Error::ImplicitConversionDoesNotExist {
            error_span: self.span,
            from: self.get_value_type(),
            to: Type::Scalar(unit),
        };

        if self.data.unit == unit {
            self
        } else if unit.is_none() || self.data.unit.is_some() && self.data.unit != unit {
            context.push_error(err);

            // And pretend the conversion is valid.
            Expr {
                span: self.span,
                weight: FastFloat::One,
                data: Rc::new(Scalar {
                    unit,
                    data: self.data.data.clone_without_node(),
                }),
                node: self.node.take()
            }
        } else {
            // `unit` is concrete and self.unit is not
            Self {
                data: Rc::new(Scalar {
                    unit,
                    data: match &self.data.data {
                        ScalarData::Generic(generic) => match generic {
                            Generic::VariableAccess(_) => unreachable!(), // Always concrete
                            Generic::Boxed(v) => {
                                ScalarData::Generic(Generic::Boxed(v.clone_without_node().convert_unit(unit, context)))
                            }
                            Generic::Dummy => ScalarData::Generic(Generic::Dummy)
                        },
                        v @ (ScalarData::Entity(_) | ScalarData::Number(_)) => v.clone_without_node(),
                        ScalarData::DstLiteral(_)
                        | ScalarData::PointPointDistance(_, _)
                        | ScalarData::PointLineDistance(_, _)
                        | ScalarData::ThreePointAngle(_, _, _)
                        | ScalarData::ThreePointAngleDir(_, _, _)
                        | ScalarData::TwoLineAngle(_, _)
                        | ScalarData::CircleRadius(_)
                        | ScalarData::SetUnit(_, _) => unreachable!(), // Always concrete
                        ScalarData::Negate(v) => ScalarData::Negate(v.clone_without_node().convert_unit(unit, context)),
                        ScalarData::Add(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Add(
                                a.clone_without_node().convert_unit(unit, context),
                                b.clone_without_node().convert_unit(unit, context),
                            )
                        }
                        ScalarData::Subtract(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Subtract(
                                a.clone_without_node().convert_unit(unit, context),
                                b.clone_without_node().convert_unit(unit, context),
                            )
                        }
                        ScalarData::Multiply(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Multiply(
                                a.clone_without_node().convert_unit(unit, context),
                                b.clone_without_node().convert_unit(Some(unit::SCALAR), context),
                            )
                        }
                        ScalarData::Divide(a, b) => {
                            // Both operands are guaranteed to be unit-less here.
                            ScalarData::Divide(
                                a.clone_without_node().convert_unit(unit, context),
                                b.clone_without_node().convert_unit(Some(unit::SCALAR), context),
                            )
                        }
                        ScalarData::Average(exprs) => {
                            // All will be unit-less.
                            ScalarData::Average(
                                exprs
                                    .iter()
                                    .map(|v| v.clone_without_node().convert_unit(unit, context))
                                    .collect::<Vec<_>>()
                                    .into(),
                            )
                        }
                    },
                }),
                ..self
            }
        }
    }

    /// Checks whether a unit conversion can be performed
    pub fn can_convert_unit(&self, unit: Option<ComplexUnit>) -> bool {
        !(unit.is_none() || self.data.unit.is_some() && self.data.unit != unit)
    }

    /// # Panics
    /// If bugged
    #[must_use]
    pub fn specify_unit(self, context: &CompileContext) -> Self {
        if self.data.unit.is_none() {
            self.convert_unit(Some(unit::SCALAR), context)
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

#[derive(Debug, CloneWithNode, Definition)]
pub enum PointCollectionData {
    Generic(Generic<PointCollection>),
    PointCollection(#[def(sequence)] ClonedVec<Expr<Point>>),
}

impl PointCollectionData {
    #[must_use]
    pub fn as_collection(&self) -> Option<&ClonedVec<Expr<Point>>> {
        match self {
            PointCollectionData::PointCollection(v) => Some(v),
            PointCollectionData::Generic(_) => None,
        }
    }

    #[must_use]
    pub fn as_collection_mut(&mut self) -> Option<&mut ClonedVec<Expr<Point>>> {
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
            PointCollectionData::Generic(generic) => match generic {
                Generic::VariableAccess(var) => var.borrow().definition.index_without_node(index),
                Generic::Boxed(expr) => expr.index_without_node(index),
                Generic::Dummy => Expr::new_spanless(Point::Generic(Generic::Dummy))
            },
            PointCollectionData::PointCollection(col) => col.get(index).map_or_else(
                || Expr::new_spanless(Point::dummy()),
                |x| x.clone_without_node()
            ),
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

#[derive(Debug, CloneWithNode, Definition)]
pub struct PointCollection {
    pub length: usize,
    #[def(entity)]
    pub data: PointCollectionData,
}

impl_from_any! {PointCollection}
impl_from_unknown! {PointCollection}

impl_convert_err! {Circle -> PointCollection}
impl_convert_err! {Line -> PointCollection}
impl_convert_err! {Scalar -> PointCollection}
impl_convert_err! {Bundle -> PointCollection}

impl_make_variable! {PointCollection {other: length, data: PointCollectionData}}

impl Displayed for PointCollection {
    type Node = PCNode;
}

impl ConvertFrom<Expr<Point>> for PointCollection {
    fn convert_from(mut value: Expr<Point>, _context: &CompileContext) -> Expr<Self> {
        let mut node = PCNode::new();
        node.push(value.node.take());

        Expr {
            weight: FastFloat::One,
            span: value.span,
            data: Rc::new(PointCollection {
                length: 1,
                data: PointCollectionData::PointCollection(vec![value].into()),
            }),
            node: Some(HierarchyNode::new(node))
        }
    }

    fn can_convert_from(_value: &Expr<Point>) -> bool {
        true
    }
}

impl PointCollection {
    #[must_use]
    pub fn get_type() -> Type {
        ty::collection(0)
    }

    #[must_use]
    pub const fn dummy() -> Self {
        Self {
            length: 0,
            data: PointCollectionData::Generic(Generic::Dummy)
        }
    }
}

impl Expr<PointCollection> {
    #[must_use]
    pub fn boxed(mut self, weight: FastFloat, span: Span) -> Self {
        let node = self.node.take();

        Self {
            weight,
            span,
            data: Rc::new(PointCollection {
                length: self.data.length,
                data: PointCollectionData::Generic(Generic::Boxed(self)),
            }),
            node
        }
    }

    /// # Errors
    /// Returns an error if lengths don't match up.
    pub fn check_len(self, length: usize, context: &CompileContext) -> Self {
        if self.data.length == length || length == 0 || matches!(self.data.data, PointCollectionData::Generic(Generic::Dummy)) {
            self
        } else {
            context.push_error(Error::ImplicitConversionDoesNotExist {
                error_span: self.span,
                from: self.get_value_type(),
                to: ty::collection(length),
            });
            
            Self {
                weight: self.weight,
                span: self.span,
                data: Rc::new(PointCollection::dummy()),
                node: self.node
            }
        }
    }

    #[must_use]
    pub fn index_without_node(&self, index: usize) -> Expr<Point> {
        self.data.data.index(index)
    }

    /// Also takes the collection's node.
    #[must_use]
    pub fn index_with_node(&mut self, index: usize) -> Expr<Point> {
        let mut point = self.data.data.index(index);
        point.node = self.node.as_mut().and_then(|x| x.root.children[index].take());

        point
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

#[derive(Debug, CloneWithNode, Definition)]
pub enum BundleData {
    Generic(Generic<Bundle>),
    ConstructBundle(#[def(map)] ClonedMap<String, AnyExpr>),
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

#[derive(Debug, CloneWithNode, Definition)]
pub struct Bundle {
    pub name: &'static str,
    #[def(entity)]
    pub data: BundleData,
}

impl_from_any! {Bundle}
impl_from_unknown! {Bundle}

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

    #[must_use]
    pub const fn dummy() -> Self {
        Self {
            name: "dummy",
            data: BundleData::Generic(Generic::Dummy)
        }
    }

    #[must_use]
    pub fn index(&self, field: &str) -> AnyExpr {
        match &self.data {
            BundleData::Generic(generic) => match &generic {
                Generic::VariableAccess(var) => var.borrow().definition.index_without_node(field),
                Generic::Boxed(expr) => expr.index_without_node(field),
                Generic::Dummy => AnyExpr::Unknown(Expr::new_spanless(Unknown::dummy()))
            },
            BundleData::ConstructBundle(map) => map.get(field).map_or_else(
                || AnyExpr::Unknown(Expr::new_spanless(Unknown::dummy())),
                |v| v.clone_without_node()
            ),
        }
    }
}

impl Displayed for Bundle {
    type Node = BundleNode;
}

impl Expr<Bundle> {
    #[must_use]
    pub fn boxed(mut self, weight: FastFloat, span: Span) -> Self {
        let node = self.node.take();

        Self {
            weight,
            span,
            data: Rc::new(Bundle {
                name: self.data.name,
                data: BundleData::Generic(Generic::Boxed(self)),
            }),
            node
        }
    }

    #[must_use]
    pub fn index_without_node(&self, field: &str) -> AnyExpr {
        self.data.index(field)
    }

    #[must_use]
    pub fn index_with_node(&mut self, field: &str) -> AnyExpr {
        let mut expr = self.data.index(field);
        expr.replace_node(self.node.as_mut().and_then(|v| v.root.children.get_mut(field).unwrap().take()));

        expr
    }

    /// # Errors
    /// Returns an error if the bundle names don't match
    pub fn check_name(self, name: &'static str, context: &CompileContext) -> Self {
        if self.data.name == name || matches!(self.data.data, BundleData::Generic(Generic::Dummy)) {
            self
        } else {
            context.push_error(Error::ImplicitConversionDoesNotExist {
                error_span: self.span,
                from: self.get_value_type(),
                to: ty::bundle(name),
            });
            
            Self {
                weight: self.weight,
                span: self.span,
                data: Rc::new(Bundle::dummy()),
                node: self.node
            }
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

#[derive(Debug, CloneWithNode, Definition)]
pub enum Unknown {
    #[def(no_entity)]
    Generic(Generic<Unknown>)
}

impl_make_variable!{Unknown}

impl Unknown {
    #[must_use]
    pub const fn dummy() -> Self {
        Self::Generic(Generic::Dummy)
    }
}

impl Display for Unknown {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(v) => write!(f, "{v}")
        }
    }
}

impl Displayed for Unknown {
    type Node = EmptyNode;
}

impl GetValueType for Unknown {
    fn get_value_type(&self) -> Type {
        Type::Unknown
    }
}

impl Expr<Unknown> {
    pub fn boxed(mut self, weight: FastFloat, span: Span) -> Self {
        let node = self.take_node();

        Self {
            weight,
            data: Rc::new(Unknown::Generic(Generic::Boxed(self))),
            span,
            node
        }
    }
}

#[derive(Debug, CloneWithNode, Definition)]
pub enum AnyExpr {
    Point(Expr<Point>),
    Line(Expr<Line>),
    Scalar(Expr<Scalar>),
    Circle(Expr<Circle>),
    PointCollection(Expr<PointCollection>),
    Bundle(Expr<Bundle>),
    Unknown(Expr<Unknown>)
}

impl_any_from_x! {Point}
impl_any_from_x! {Line}
impl_any_from_x! {Scalar}
impl_any_from_x! {Circle}
impl_any_from_x! {PointCollection}
impl_any_from_x! {Bundle}
impl_any_from_x! {Unknown}

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
            Self::Unknown(v) => &mut v.span
        }
    }

    #[must_use]
    pub fn get_span(&self) -> Span {
        match self {
            Self::Point(v) => v.span,
            Self::Line(v) => v.span,
            Self::Scalar(v) => v.span,
            Self::Circle(v) => v.span,
            Self::PointCollection(v) => v.span,
            Self::Bundle(v) => v.span,
            Self::Unknown(v) => v.span
        }
    }

    #[must_use]
    pub const fn get_weight(&self) -> FastFloat {
        match self {
            Self::Point(v) => v.weight,
            Self::Line(v) => v.weight,
            Self::Scalar(v) => v.weight,
            Self::Circle(v) => v.weight,
            Self::PointCollection(v) => v.weight,
            Self::Bundle(v) => v.weight,
            Self::Unknown(v) => v.weight
        }
    }

    pub fn replace_node(&mut self, with: Option<AnyExprNode>) -> Option<AnyExprNode> {
        Some(match self {
            Self::Point(v) => AnyExprNode::Point(
                    mem::replace(
                        &mut v.node,
                        with.map(|x| x.as_point())
                    )
                ?),
            Self::Line(v) => AnyExprNode::Line(
                    mem::replace(
                        &mut v.node,
                        with.map(|x| x.as_line())
                    )
                ?),
            Self::Scalar(v) => AnyExprNode::Scalar(
                    mem::replace(
                        &mut v.node,
                        with.map(|x| x.as_scalar())
                    )
                ?),
            Self::Circle(v) => AnyExprNode::Circle(
                    mem::replace(
                        &mut v.node,
                        with.map(|x| x.as_circle())
                    )
                ?),
            Self::PointCollection(v) => AnyExprNode::PointCollection(
                    mem::replace(
                        &mut v.node,
                        with.map(|x| x.as_point_collection())
                    )
                ?),
            Self::Bundle(v) => AnyExprNode::Bundle(
                    mem::replace(
                        &mut v.node,
                        with.map(|x| x.as_bundle())
                    )
                ?),
            Self::Unknown(v) => AnyExprNode::Unknown(
                    mem::replace(
                        &mut v.node,
                        with.map(|x| x.as_unknown())
                    )
                ?),
        })
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
            AnyExpr::Unknown(v) => v.get_value_type()
        }
    }

    /// # Errors
    /// Returns an error if the conversion is invalid.
    pub fn convert_to(self, to: Type, context: &CompileContext) -> Self {
        match to {
            Type::Point => Self::Point(self.convert(context)),
            Type::Line => Self::Line(self.convert(context)),
            Type::Scalar(unit) => Self::Scalar(self.convert(context).convert_unit(unit, context)),
            Type::PointCollection(len) => Self::PointCollection(self.convert(context).check_len(len, context)),
            Type::Circle => Self::Circle(self.convert(context)),
            Type::Bundle(name) => Self::Bundle(self.convert(context).check_name(name, context)),
            Type::Unknown => unreachable!("This shouldn't be possible"),
        }
    }

    pub fn can_convert_to(&self, to: Type) -> bool {
        match to {
            Type::Point => self.can_convert::<Point>(),
            Type::Line => self.can_convert::<Line>(),
            Type::Scalar(unit) => self.can_convert_to_scalar(unit),
            Type::PointCollection(len) => self.can_convert_to_collection(len),
            Type::Circle => self.can_convert::<Circle>(),
            Type::Bundle(name) => self.can_convert_to_bundle(name),
            Type::Unknown => unreachable!("This shouldn't be possible"),
        }
    }

    pub fn can_convert_to_scalar(&self, unit: Option<ComplexUnit>) -> bool {
        match self {
            Self::Line(_) | Self::Bundle(_) | Self::Circle(_) | Self::Point(_) => false,
            Self::PointCollection(v) => v.data.length == 2 && (unit == Some(unit::DISTANCE) || unit.is_none()),
            Self::Scalar(u) => u.data.unit == unit || unit.is_none(),
            Self::Unknown(_) => true
        }
    }

    pub fn can_convert_to_collection(&self, len: usize) -> bool {
        match self {
            Self::Line(_) | Self::Bundle(_) | Self::Circle(_) | Self::Scalar(_) => false,
            Self::PointCollection(v) => v.data.length == len,
            Self::Point(_) => len == 1,
            Self::Unknown(_) => true
        }
    }

    pub fn can_convert_to_bundle(&self, name: &str) -> bool {
        match self {
            Self::Bundle(bundle) => bundle.data.name == name,
            Self::Unknown(_) => true,
            _ => false,
        }
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
            Self::Unknown(v) => Self::Unknown(v.boxed(weight, span)),
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
            Self::Unknown(v) => match v.data.as_ref() {
                Unknown::Generic(Generic::VariableAccess(var)) => {
                    var.borrow().definition_span
                },
                _ => panic!("not a variable")
            }
        }
    }

    #[must_use]
    pub fn make_variable(self, name: String) -> Self {
        match self {
            Self::Point(v) => Self::Point(v.make_variable(name)),
            Self::Line(v) => Self::Line(v.make_variable(name)),
            Self::Scalar(v) => Self::Scalar(v.make_variable(name)),
            Self::Circle(v) => Self::Circle(v.make_variable(name)),
            Self::PointCollection(v) => Self::PointCollection(v.make_variable(name)),
            Self::Bundle(v) => Self::Bundle(v.make_variable(name)),
            Self::Unknown(v) => Self::Unknown(v.make_variable(name)),
        }
    }
}

impl Display for AnyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Point(v) => write!(f, "{v}"),
            Self::Line(v) => write!(f, "{v}"),
            Self::Scalar(v) => write!(f, "{v}"),
            Self::Circle(v) => write!(f, "{v}"),
            Self::PointCollection(v) => write!(f, "{v}"),
            Self::Bundle(v) => write!(f, "{v}"),
            Self::Unknown(v) => write!(f, "{v}")
        }
    }
}

pub fn display_vec<T: Display>(v: &[T]) -> String {
    v.iter()
        .map(|x| format!("{x}"))
        .collect::<Vec<String>>()
        .join(", ")
}

pub trait Displayed: Sized {
    type Node: Node;
}

pub trait CloneWithNode {
    #[must_use]
    fn clone_with_node(&mut self) -> Self;

    #[must_use]
    fn clone_without_node(&self) -> Self;
}

/// Wrapper struct for a cloned vec.
#[derive(Debug)]
pub struct ClonedVec<T>(pub Vec<T>);

impl<T> From<Vec<T>> for ClonedVec<T> {
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}

impl<T> Deref for ClonedVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for ClonedVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Wrapper struct for a cloned map.
#[derive(Debug)]
pub struct ClonedMap<K, V>(pub HashMap<K, V>);

impl<K, V> From<HashMap<K, V>> for ClonedMap<K, V> {
    fn from(value: HashMap<K, V>) -> Self {
        Self(value)
    }
}

impl<K, V> Deref for ClonedMap<K, V> {
    type Target = HashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> DerefMut for ClonedMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Clone> CloneWithNode for T {
    fn clone_with_node(&mut self) -> Self {
        self.clone()
    }

    fn clone_without_node(&self) -> Self {
        self.clone()
    }
}

impl<T: CloneWithNode> CloneWithNode for ClonedVec<T> {
    fn clone_with_node(&mut self) -> Self {
        self.iter_mut().map(CloneWithNode::clone_with_node).collect::<Vec<_>>().into()
    }

    fn clone_without_node(&self) -> Self {
        self.iter().map(CloneWithNode::clone_without_node).collect::<Vec<_>>().into()
    }
}

impl<K: Hash + CloneWithNode + Eq, V: CloneWithNode> CloneWithNode for ClonedMap<K, V> {
    fn clone_with_node(&mut self) -> Self {
        self.iter_mut().map(|(k, v)| (k.clone_without_node(), v.clone_with_node())).collect::<HashMap<_, _>>().into()
    }

    fn clone_without_node(&self) -> Self {
        self.iter().map(|(k, v)| (k.clone_without_node(), v.clone_without_node())).collect::<HashMap<_, _>>().into()
    }
}

#[derive(Debug)]
pub struct Expr<T: ?Sized + Displayed> {
    pub data: Rc<T>,
    pub span: Span,
    pub weight: FastFloat, // Assigned weight.
    pub node: Option<HierarchyNode<T::Node>>
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

impl<T: CloneWithNode + Displayed> Expr<T> {
    /// Creates a new expression without a declared span. WARNING: the expression has no node.
    pub fn new_spanless(data: T) -> Self {
        Self {
            span: span!(0, 0, 0, 0),
            data: Rc::new(data),
            weight: FastFloat::One,
            node: None
        }
    }

    pub fn with_weight(mut self, weight: FastFloat) -> Self {
        self.weight = weight;
        self
    }

    pub fn take_node(&mut self) -> Option<HierarchyNode<T::Node>> {
        self.node.take()
    }
}

impl<T: CloneWithNode + Displayed> Expr<T> where Expr<T>: Simplify {
    #[must_use]
    pub fn simplify_with_node(&mut self, context: &CompileContext) -> Self {
        let node = self.node.take();

        let mut expr = self.simplify(context);

        expr.node = node;
        expr
    }
}

#[derive(Debug)]
pub struct UnrolledRule {
    pub kind: UnrolledRuleKind,
    pub inverted: bool,
    pub node: Option<Box<dyn Node>>
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
            UnrolledRuleKind::Display => write!(f, "display rule")
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
    params: Vec<AnyExpr>,
    display: Properties,
    context: &mut CompileContext
) -> AnyExpr {
    definition(params, context, display)
}

fn fetch_variable(
    context: &CompileContext,
    name: &str,
    variable_span: Span,
) -> AnyExpr {
    let mut var = if let Some(var) = context.variables.get(name) {
        var.clone_without_node()
    } else {
        let suggested = most_similar(context.variables.keys(), name);

        context.push_error(Error::UndefinedVariable {
            error_span: variable_span,
            variable_name: name.to_string(),
            suggested,
        });

        Expr::new_spanless(Unknown::dummy()).make_variable(name.to_string()).into()
    };

    *var.get_span_mut() = variable_span;
    var
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
#[allow(clippy::too_many_lines)]
fn unroll_simple(
    expr: &SimpleExpression,
    context: &mut CompileContext,
    library: &Library,
    it_index: &HashMap<u8, usize>,
    external_display: Properties
) -> AnyExpr {
    let display = Properties::from(expr.display.clone()).merge_with(external_display);

    match &expr.kind {
        SimpleExpressionKind::Ident(i) => match i {
            Ident::Named(named) => {
                // No options are expected, as var refs don't generate nodes.
                display.finish(&[], context);

                fetch_variable(context, &named.ident, named.span)
            },
            Ident::Collection(col) => {
                // No options are expected, as pcs don't generate nodes.
                display.finish(&[], context);

                AnyExpr::PointCollection(Expr {
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
                                    ).convert::<Point>(context)
                                })
                                .collect::<Vec<_>>()
                                .into(),
                        ),
                    }),
                    span: col.span,
                    node: None // Point collections always reference variables. The is no point generating a node.
                })
            }
        },
        SimpleExpressionKind::Number(num) => AnyExpr::Scalar(Expr {
            weight: FastFloat::Zero, // Always zero.
            data: Rc::new(Scalar {
                unit: None,
                data: ScalarData::Number(num.value.to_float()),
            }),
            span: num.get_span(),
            node: None
        }),
        SimpleExpressionKind::Call(e) => {
            let params = match &e.params {
                Some(params) => params
                    .iter()
                    .map(|p| unroll_expression(p, context, library, it_index, Properties::from(None)))
                    .collect::<Vec<_>>(),
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
                                param.convert_to(overload.params[i], context)
                            } else {
                                param.convert_to(overload.param_group.unwrap(), context)
                            }
                        })
                        .collect();

                    let ret = unroll_parameters(&overload.definition, params, display, context);

                    // TODO: UPDATE FOR WEIGHING SUPPORT
                    ret.boxed(FastFloat::One, e.get_span())
                } else {
                    context.push_error(Error::OverloadNotFound {
                        error_span: e.get_span(),
                        function_name: e.name.ident.clone(),
                        params: param_types,
                    });

                    Expr::new_spanless(Unknown::dummy()).into()
                }
            } else {
                let suggested = most_similar(library.functions.keys(), &e.name.ident);

                context.push_error(Error::UndefinedFunction {
                    error_span: e.name.span,
                    function_name: e.name.ident.clone(),
                    suggested,
                });

                Expr::new_spanless(Unknown::dummy()).into()
            }
        }
        SimpleExpressionKind::Unop(op) => {
            let mut unrolled: Expr<Scalar> =
                unroll_simple(&op.rhs, context, library, it_index, display).convert(context);

            let node = unrolled.node.take();

            AnyExpr::Scalar(Expr {
                weight: FastFloat::One,
                span: expr.get_span(),
                data: Rc::new(Scalar {
                    unit: unrolled.data.unit,
                    data: ScalarData::Negate(unrolled),
                }),
                node
            })
        }
        SimpleExpressionKind::Parenthised(expr) => {
            unroll_expression(&expr.content, context, library, it_index, display)
        }
        SimpleExpressionKind::ExplicitIterator(it) => unroll_expression(
            it.get(it_index[&it.id]).unwrap(),
            context,
            library,
            it_index,
            display
        ),
        SimpleExpressionKind::PointCollection(col) => AnyExpr::PointCollection(Expr {
            weight: FastFloat::One, // TODO: UPDATE FOR WEIGHING SUPPORT IN GEOSCRIPT
            span: col.get_span(),
            data: Rc::new(PointCollection {
                length: col.points.len(),
                data: PointCollectionData::PointCollection({
                    let mut points = Vec::new();

                    for expr in col.points.iter() {
                        let mut unrolled = unroll_expression(expr, context, library, it_index, Properties::default());

                        if unrolled.can_convert_to(ty::POINT) {
                            points.push(unrolled.convert(context));
                        } else {
                            context.push_error(Error::NonPointInPointCollection {
                                error_span: col.get_span(),
                                received: (expr.get_span(), unrolled.get_type()), // (span, from)
                            });

                            // Pretend the point is valid.
                            points.push(Expr {
                                weight: unrolled.get_weight(),
                                span: unrolled.get_span(),
                                data: Rc::new(Point::dummy()),
                                node: unrolled.replace_node(None).map(AnyExprNode::as_point)
                            });
                        }
                    }

                    points.into()
                }),
            }),
            node: None // Point collections always reference variables. No point in creating a node.
        }),
    }
}

fn unroll_binop(lhs: AnyExpr, op: &BinaryOperator, rhs: AnyExpr, display: Properties, context: &CompileContext) -> AnyExpr {
    let full_span = lhs.get_span().join(rhs.get_span());

    let mut lhs = if lhs.can_convert_to(ty::SCALAR_UNKNOWN) {
        lhs.convert::<Scalar>(context)
    } else {
        context.push_error(Error::InvalidOperandType {
            error_span: full_span,
            got: (lhs.get_type(), lhs.get_span()),
            op: op.to_string(),
        });

        Expr {
            weight: lhs.get_weight(),
            span: lhs.get_span(),
            data: Rc::new(Scalar::dummy()),
            node: None
        }
    };

    let mut rhs = if rhs.can_convert_to(ty::SCALAR_UNKNOWN) {
        rhs.convert::<Scalar>(context)
    } else {
        context.push_error(Error::InvalidOperandType {
            error_span: full_span,
            got: (rhs.get_type(), rhs.get_span()),
            op: op.to_string(),
        });

        Expr {
            weight: rhs.get_weight(),
            span: rhs.get_span(),
            data: Rc::new(Scalar::dummy()),
            node: None
        }
    };

    // Binary operators generate collection nodes for the lhs and rhs nodes.
    let lhs_node = lhs.take_node();
    let rhs_node = rhs.take_node();

    match op {
        BinaryOperator::Add(_) | BinaryOperator::Sub(_) => {
            let lhs = if lhs.data.unit.is_none() {
                lhs.convert_unit(rhs.data.unit, context)
            } else {
                lhs
            };

            let rhs = rhs.convert_unit(lhs.data.unit, context);
            let mut expr = Expr {
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
                node: None
            };

            let mut node = HierarchyNode::new(ScalarNode::from_expr(&expr, display, context));
            node.extend_children(lhs_node);
            node.extend_children(rhs_node);

            expr.node = Some(node);

            AnyExpr::Scalar(expr)
        }
        BinaryOperator::Mul(_) | BinaryOperator::Div(_) => {
            let lhs = lhs.specify_unit(context);

            let rhs = rhs.specify_unit(context);
            let mut expr = Expr {
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
                node: None
            };

            let mut node = HierarchyNode::new(ScalarNode::from_expr(&expr, display, context));
            node.extend_children(lhs_node);
            node.extend_children(rhs_node);

            expr.node = Some(node);

            AnyExpr::Scalar(expr)
        }
    }
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
fn unroll_expression<const ITER: bool>(
    expr: &Expression<ITER>,
    context: &mut CompileContext,
    library: &Library,
    it_index: &HashMap<u8, usize>,
    external_display: Properties
) -> AnyExpr {
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
            let lhs = unroll_expression(&op.lhs, context, library, it_index, Properties::from(None));
            let rhs = unroll_expression(&op.rhs, context, library, it_index, Properties::from(None));

            let display = Properties::from(external_display);

            unroll_binop(lhs, &op.operator, rhs, display, context)
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

pub fn most_similar<'r, I: IntoIterator<Item = &'r T>, T: AsRef<str> + ?Sized + 'r>(expected: I, received: &str) -> Option<String> {
    expected
        .into_iter()
        .map(|v| (v.as_ref(), (strsim::jaro(v.as_ref(), received) * 1000.0).floor() as i64))
        .filter(|v| v.1 > 600)
        .max_by_key(|v| v.1)
        .map(|v| v.0.to_string())
}

// pub fn most_similar_by_key<'r, T: 'r, F: Fn(&'r T) -> &'r str, I: IntoIterator<Item = &'r T>>(f: F, expected: I, received: &str) -> Option<String> {
//     expected
//         .into_iter()
//         .map(|v| f(v))
//         .map(|v| (v, (strsim::jaro(v, received) * 1000.0).floor() as i64))
//         .filter(|v| v.1 > 600)
//         .max_by_key(|v| v.1)
//         .map(|v| v.0.to_string())
// }

#[derive(Debug)]
pub struct Properties{
    props: HashMap<String, PropertyValue>,
    finished: bool,
    errors: Vec<Error>
}

impl Clone for Properties {
    fn clone(&self) -> Self {
        Self {
            props: self.props.clone(),
            finished: false,
            errors: Vec::new()
        }
    }
}

impl Default for Properties {
    fn default() -> Self {
        Self {
            props: HashMap::new(),
            finished: false,
            errors: Vec::new()
        }
    }
}

impl Properties {
    pub fn finish(mut self, expected_fields: &'static [&'static str], context: &CompileContext) {
        self.finished = true;

        let props = mem::take(&mut self.props);
        self.errors.extend(props.into_iter().map(
            |(key, value)| {
                let suggested = most_similar(expected_fields, &key);

                Error::UnexpectedDisplayOption {
                    error_span: value.get_span(),
                    option: key,
                    suggested
                }
            }
        ));

        context.extend_errors(mem::take(&mut self.errors))
    }

    pub fn get<T>(&mut self, property: &str) -> Property<T>
    where PropertyValue: PropGetValue<T> {
        if let Some(prop) = self.props.remove(property) {
            let prop_span = prop.get_span();

            Property {
                value: match prop.get() {
                    Ok(v) => Some(v),
                    Err(err) => {
                        self.errors.push(err);
                        None
                    }
                },
                span: Some(prop_span)
            }
        } else {
            Property {
                value: None,
                span: None
            }
        }
    }

    pub fn merge_with(mut self, mut other: Properties) -> Self {
        self.errors.extend(mem::take(&mut other.errors));

        for (k, v) in mem::take(&mut other.props) {
            let error_span = v.get_span();
            let option = k.clone();
            let old = self.props.insert(k, v);

            if let Some(old) = old {
                self.errors.push(Error::RepeatedDisplayOption {
                    error_span,
                    first_span: old.get_span(),
                    option
                });
            }
        }

        // no finish(), because that would require CompileContext. Here, however, no errors would happen anyway.
        other.finished = true;

        self
    }
}

impl From<Option<DisplayProperties>> for Properties {
    fn from(value: Option<DisplayProperties>) -> Self {
        Self {
            props: value.into_iter().flat_map(
                |x| x.properties.into_iter()
            )
                .map(|v| (v.name.ident.clone(), v.value.clone()))
                .collect(),
            finished: false,
            errors: Vec::new()
        }
    }
}

impl Drop for Properties {
    fn drop(&mut self) {
        if !self.finished {
            eprintln!("Properties dropped before finishing parsing: {:#?}. Please report this error.", self.props);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Property<T> {
    value: Option<T>,
    span: Option<Span>
}

impl<T> Property<T> {
    #[must_use]
    pub fn get(self) -> Option<T> {
        self.value
    }

    #[must_use]
    pub fn get_or(self, default: T) -> T {
        self.value.unwrap_or(default)
    }

    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        self.span
    }

    #[must_use]
    pub fn maybe_unset(self, default: T) -> MaybeUnset<T> {
        let mut value = MaybeUnset::new(default);
        value.try_set(self.get());
        
        value
    }
}

fn create_variable_named(
    stat: &LetStatement,
    context: &mut CompileContext,
    named: &NamedIdent,
    mut rhs_unrolled: AnyExpr,
    variable_nodes: &mut Vec<Box<dyn Node>>
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
            variable_nodes.extend(rhs_unrolled.replace_node(None).map(|v| v.as_dyn()));

            let var = rhs_unrolled.make_variable(entry.key().clone());
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
    rhs_unrolled: AnyExpr,
    variable_nodes: &mut Vec<Box<dyn Node>>
) -> Result<(), Error> {
    // let display = Properties::from(display);

    let maybe_error = Error::CannotUnpack {
        error_span: rhs_unrolled.get_span(),
        ty: rhs_unrolled.get_type(),
    };
    let mut rhs_unpacked = rhs_unrolled.convert::<PointCollection>(context);

    if rhs_unpacked.data.length != col.len() {
        return Err(maybe_error);
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

    let mut rhs_node = rhs_unpacked.take_node();
    let mut rhs_unpacked = rhs_unpacked.data.data.as_collection().unwrap()
        .iter()
        .map(|x| x.clone_without_node());

    for (i, pt) in col.collection.iter().enumerate() {
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
                    .unwrap();

                if let Some(rhs_node) = &mut rhs_node {
                    let pt_node = rhs_node.root.children
                        .get_mut(i)
                        .and_then(|v| v.take());

                    variable_nodes.extend(pt_node.map(|x| Box::new(x) as Box<dyn Node>));
                }

                let var = var.make_variable(entry.key().clone());

                // if let Some(node) = pt_node {
                //     todo!()
                // }

                let var = AnyExpr::Point(var);
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
) -> Result<Vec<Box<dyn Node>>, Error> {
    let mut variable_nodes = Vec::new();

    let tree = IterNode::from(&stat.expr);

    let ind = if let Some(iter) = tree.first() {
        if stat.ident.len() == 1 {
            context.push_error(Error::LetStatUnexpectedIterator {
                var_span: stat.ident.get_span(),
                error_span: iter.span,
            });

            // Ignore every but first entry in the iterator.
        }

        for it in tree.iter() {
            if iter.id != it.id {
                context.push_error(Error::LetStatMoreThanOneIterator {
                    error_span: stat.expr.get_span(),
                    first_span: iter.span,
                    second_span: it.span,
                });

                // Ignore every other iterator.
            }

            for variant in &it.variants {
                if let Some(it2) = variant.first() {
                    context.push_error(Error::LetStatMoreThanOneIterator {
                        error_span: stat.expr.get_span(),
                        first_span: iter.span,
                        second_span: it2.span,
                    });

                    // Ignore every other iterator.
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

        let mut external = Properties::default();
        external.props.insert(
            String::from("default-label"),
            PropertyValue::MathString(default_label)
        );

        let display = Properties::from(def.display_properties.clone()).merge_with(external);

        let rhs_unrolled = unroll_expression(
            &stat.expr,
            context,
            library,
            ind.as_ref()
                .unwrap_or_else(|| it_index.get_currents().unwrap()),
            display
        );
        it_index.next();

        match &def.name {
            Ident::Named(named) => {
                create_variable_named(
                    &stat,
                    context,
                    named,
                    rhs_unrolled,
                    &mut variable_nodes
                )?;
            }
            Ident::Collection(col) => {
                create_variable_collection(
                    &stat,
                    context,
                    col,
                    rhs_unrolled,
                    &mut variable_nodes
                )?;
            }
        }
    }

    Ok(variable_nodes)
}

fn unroll_let(
    mut stat: LetStatement,
    context: &mut CompileContext,
    library: &Library
) -> Result<Vec<Box<dyn Node>>, Error> {
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
    
    let nodes = create_variables(stat, context, library)?;

    // Then, we run each rule through a tree iterator.
    for (rule, expr) in rules {
        let tree = IterNode::from2(&lhs, &expr);

        // Check the lengths
        tree.get_iter_lengths(&mut HashMap::new(), stat_span)?;

        // And create the index
        let mut index = IterTreeIterator::new(&tree);

        while let Some(it_index) = index.get_currents() {
            unroll_rule(
                unroll_expression(&lhs, context, library, it_index, Properties::default()),
                &rule,
                unroll_expression(&expr, context, library, it_index, Properties::default()),
                context,
                library,
                stat_span,
                false
            );

            index.next();
        }
    }

    Ok(nodes)
}

fn unroll_eq(
    lhs: AnyExpr,
    rhs: AnyExpr,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool
) {
    let lhs_type = lhs.get_type();
    let rhs_type = rhs.get_type();

    if (lhs_type == ty::collection(2) && rhs_type == ty::collection(2))
        || (lhs_type == ty::collection(2) && rhs_type == ty::SCALAR_UNKNOWN)
        || (lhs_type == ty::SCALAR_UNKNOWN && rhs_type == ty::collection(2))
    {
        // AB = CD must have different logic as it's implied that this means "equality of distances".
        let lhs = lhs.convert(context).convert_unit(Some(unit::DISTANCE), context);
        let rhs = rhs.convert(context).convert_unit(Some(unit::DISTANCE), context);
        context.gt(lhs, rhs, inverted);
    } else {
        // If any of the two types can be cast onto the other, cast and compare.
        let (lhs, rhs) = if rhs.can_convert_to(lhs_type) {
            (lhs, rhs.convert_to(lhs_type, context))
        } else if lhs.can_convert_to(rhs_type) {
            (lhs.convert_to(rhs_type, context), rhs)
        } else {
            context.push_error(Error::InconsistentTypes {
                expected: (lhs_type, Box::new(lhs.get_span())),
                got: (rhs_type, Box::new(rhs.get_span())),
                error_span: Box::new(full_span),
            });

            // "Cast" rhs to lhs.
            (lhs, AnyExpr::Point(Expr {
                span: rhs.get_span(),
                weight: rhs.get_weight(),
                data: Rc::new(Point::dummy()),
                node: None
            }).convert_to(lhs_type, context))
        };

        match rhs_type {
            Type::Point => {
                let lhs = lhs.convert(context);
                let rhs = rhs.convert(context);

                context.point_eq(lhs, rhs, inverted);
            }
            Type::Scalar(_) => {
                let lhs = lhs.convert(context);
                let rhs = rhs.convert(context);

                context.scalar_eq(lhs, rhs, inverted);
            }
            ty => {
                context.push_error(Error::ComparisonDoesNotExist {
                    error_span: full_span,
                    ty,
                });

                // Pretend there is no rule.
            }
        }
    }
}

fn unroll_gt(
    lhs: Expr<Scalar>,
    rhs: Expr<Scalar>,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool,
) {
    if lhs.data.unit.is_some() {
        let rhs = if rhs.can_convert_unit(lhs.data.unit) {
            rhs.convert_unit(lhs.data.unit, context)
        } else {
            context.push_error(Error::InconsistentTypes {
                expected: (lhs.get_value_type(), Box::new(lhs.span)),
                got: (rhs.get_value_type(), Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });

            Expr {
                span: rhs.span,
                weight: rhs.weight,
                node: rhs.node,
                data: Rc::new(Scalar::dummy())
            }.convert_unit(lhs.data.unit, context)
        };

        context.gt(lhs, rhs, inverted);
    } else if rhs.data.unit.is_some() {
        let lhs = lhs.convert_unit(rhs.data.unit, context);
        context.gt(lhs, rhs, inverted);
    } else {
        let lhs = lhs.convert_unit(Some(unit::SCALAR), context);
        let rhs = rhs.convert_unit(Some(unit::SCALAR), context);
        context.gt(lhs, rhs, inverted);
    }
}

fn unroll_lt(
    lhs: Expr<Scalar>,
    rhs: Expr<Scalar>,
    context: &mut CompileContext,
    full_span: Span,
    inverted: bool,
) {
    if lhs.data.unit.is_some() {
        let rhs = if rhs.can_convert_unit(lhs.data.unit) {
            rhs.convert_unit(lhs.data.unit, context)
        } else {
            context.push_error(Error::InconsistentTypes {
                expected: (lhs.get_value_type(), Box::new(lhs.span)),
                got: (rhs.get_value_type(), Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });

            Expr {
                span: rhs.span,
                weight: rhs.weight,
                node: rhs.node,
                data: Rc::new(Scalar::dummy())
            }.convert_unit(lhs.data.unit, context)
        };

        context.lt(lhs, rhs, inverted);
    } else if rhs.data.unit.is_some() {
        let lhs = lhs.convert_unit(rhs.data.unit, context);
        context.lt(lhs, rhs, inverted);
    } else {
        let lhs = lhs.convert_unit(Some(unit::SCALAR), context);
        let rhs = rhs.convert_unit(Some(unit::SCALAR), context);
        context.lt(lhs, rhs, inverted);
    }
}

fn unroll_rule(
    lhs: AnyExpr,
    op: &RuleOperator,
    rhs: AnyExpr,
    context: &mut CompileContext,
    library: &Library,
    full_span: Span,
    invert: bool
) {
    match op {
        RuleOperator::Predefined(pre) => match pre {
            PredefinedRuleOperator::Eq(_) => unroll_eq(lhs, rhs, context, full_span, invert),
            PredefinedRuleOperator::Lt(_) => {
                unroll_lt(lhs.convert(context), rhs.convert(context), context, full_span, invert)
            }
            PredefinedRuleOperator::Gt(_) => {
                unroll_gt(lhs.convert(context), rhs.convert(context), context, full_span, invert)
            }
            PredefinedRuleOperator::Lteq(_) => {
                unroll_gt(lhs.convert(context), rhs.convert(context), context, full_span, !invert)
            }
            PredefinedRuleOperator::Gteq(_) => {
                unroll_lt(lhs.convert(context), rhs.convert(context), context, full_span, !invert)
            }
        },
        RuleOperator::Defined(op) => {
            let (def, lhs, rhs) = if let Some(func) = library.rule_ops.get(&op.ident.ident) {
                if let Some(overload) = func.get_overload((&lhs.get_type(), &rhs.get_type())) {
                    let lhs = lhs.convert_to(overload.params.0, context);
                    let rhs = rhs.convert_to(overload.params.1, context);

                    (&overload.definition, lhs, rhs)
                } else {
                    context.push_error(Error::OverloadNotFound {
                        error_span: op.ident.span,
                        function_name: op.ident.ident.clone(),
                        params: vec![lhs.get_type(), rhs.get_type()],
                    });

                    // Pretend the rule doesn't exist.
                    return;
                }
            } else {
                let suggested = most_similar(library.rule_ops.keys(), &op.ident.ident);

                context.push_error(Error::UndefinedFunction {
                    error_span: op.ident.span,
                    function_name: op.ident.ident.clone(),
                    suggested,
                });

                    // Pretend the rule doesn't exist.
                return;
            };

            def(lhs, rhs, context, Properties::from(None), invert);
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
            unroll_expression(&rule.lhs, context, library, index, Properties::default()),
            &rule.op,
            unroll_expression(&rule.rhs, context, library, index, Properties::default()),
            context,
            library,
            rule.get_span(),
            false
        );

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

fn set_flag(set: &mut FlagSet, flag: &FlagStatement, context: &CompileContext) {
    set_flag_recursive(set, flag, context, 0)

    // let Some(mut flag_ref) = set.get_mut(&flag.name.name.first.ident) else {
    //     let flag_name = flag.name.name.first.ident.clone();

    //     let suggested = most_similar(set.keys(), &flag_name);

    //     context.push_error(Error::FlagDoesNotExist {
    //         flag_name,
    //         flag_span: flag.name.get_span(),
    //         error_span: flag.get_span(),
    //         suggested,
    //     });

    //     // Pretend the flag doesn't exist.
    //     return;
    // };

    // let mut old_head = flag_ref;

    // for part in flag.name.name.iter().skip(1) {
    //     flag_ref = if let FlagKind::Set(set) = &mut flag_ref.kind {
    //         if let Some(v) = set.get_mut(&part.ident) {
    //             v
    //         } else {
    //             let flag_name = part.ident.clone();
    
    //             let suggested = most_similar(set.keys(), &flag_name);
    
    //             context.push_error(Error::FlagDoesNotExist {
    //                 flag_name,
    //                 flag_span: flag.name.get_span(),
    //                 error_span: flag.get_span(),
    //                 suggested,
    //             });
    
    //             // And pretend it doesn't exist.
    //             continue;
    //         }
    //     } else {
    //         context.push_error(Error::FlagSetExpected {
    //             error_span: flag.get_span(),
    //         });

    //         // Further parsing could cause ambiguous errors.
    //         break;
    //     };
    // }

    // match flag_ref.ty {
    //     FlagType::Set => match &flag.value {
    //         super::parser::FlagValue::Set(set) => match &mut flag_ref.kind {
    //             FlagKind::Setting(_) => unreachable!(),
    //             FlagKind::Set(s) => {
    //                 for stat in &set.flags {
    //                     set_flag(s, stat, context);
    //                 }
    //             }
    //         },
    //         super::parser::FlagValue::Ident(_) | super::parser::FlagValue::Number(_) => {
    //             context.push_error(Error::FlagSetExpected {
    //                 error_span: flag.get_span(),
    //             });
    //         }
    //     },
    //     FlagType::Boolean => {context.ok(set_flag_bool(flag_ref, flag));},
    //     FlagType::String => match &flag.value {
    //         super::parser::FlagValue::Number(_) | super::parser::FlagValue::Set(_) => {
    //             context.push_error(Error::StringExpected {
    //                 error_span: flag.get_span(),
    //             });
    //         }
    //         super::parser::FlagValue::Ident(ident) => match &mut flag_ref.kind {
    //             FlagKind::Setting(s) => match s {
    //                 FlagSetting::Default(_) | FlagSetting::Unset => {
    //                     *s = FlagSetting::Set(
    //                         FlagValue::String(ident.ident.clone()),
    //                         flag.get_span(),
    //                     );
    //                 }
    //                 FlagSetting::Set(_, sp) => {
    //                     context.push_error(Error::RedefinedFlag {
    //                         error_span: flag.get_span(),
    //                         first_defined: *sp,
    //                         flag_name: flag_ref.name.clone(),
    //                     });
    //                 }
    //             },
    //             FlagKind::Set(_) => unreachable!(),
    //         },
    //     },
    // }
}

fn set_flag_recursive(set: &mut FlagSet, flag: &FlagStatement, context: &CompileContext, depth: usize) {
    let Some(flag_ref) = set.get_mut(&flag.name.name.first.ident) else {
        let flag_name = flag.name.name.first.ident.clone();

        let suggested = most_similar(set.keys(), &flag_name);

        context.push_error(Error::FlagDoesNotExist {
            flag_name,
            flag_span: flag.name.get_span(),
            error_span: flag.get_span(),
            suggested,
        });

        // Pretend the flag doesn't exist.
        return;
    };

    if flag.name.name.len() == depth {
        match flag_ref.ty {
            FlagType::Set => match &flag.value {
                super::parser::FlagValue::Set(set) => match &mut flag_ref.kind {
                    FlagKind::Setting(_) => unreachable!(),
                    FlagKind::Set(s) => {
                        for stat in &set.flags {
                            set_flag(s, stat, context);
                        }
                    }
                },
                super::parser::FlagValue::Ident(_) | super::parser::FlagValue::Number(_) => {
                    context.push_error(Error::FlagSetExpected {
                        error_span: flag.get_span(),
                    });
                }
            },
            FlagType::Boolean => {context.ok(set_flag_bool(flag_ref, flag));},
            FlagType::String => match &flag.value {
                super::parser::FlagValue::Number(_) | super::parser::FlagValue::Set(_) => {
                    context.push_error(Error::StringExpected {
                        error_span: flag.get_span(),
                    });
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
                            context.push_error(Error::RedefinedFlag {
                                error_span: flag.get_span(),
                                first_defined: *sp,
                                flag_name: flag_ref.name.clone(),
                            });
                        }
                    },
                    FlagKind::Set(_) => unreachable!(),
                },
            },
        }
    } else {
        if let FlagKind::Set(set) = &mut flag_ref.kind {
            set_flag_recursive(set, flag, context, depth+1);
        } else {
            context.push_error(Error::FlagSetExpected {
                error_span: flag.get_span(),
            });
        }
    }
}

/// Unrolls the given script. All iterators are expanded and all conversions applied. The output can be immediately compiled.
///
/// # Errors
/// Specific error descriptions are in `ScriptError` documentation.
pub fn unroll(input: &str) -> Result<(CompileContext, CollectionNode), Vec<Error>> {
    // Unfortunately, due to how context-dependent geoscript is, the code must be compiled immediately after parsing.
    let mut context = CompileContext::new();
    let mut library = Library::new();

    let mut figure = CollectionNode::new();

    builtins::register(&mut library);
    let library = library; // Disable mutation

    let tokens = match token::tokenize(input) {
        Ok(v) => v,
        Err(err) => return Err(vec![err])
    };
    let mut it = tokens.iter().peekable();

    let mut statements = Vec::new();

    while it.peek().is_some() {
        statements.push(match Statement::parse(&mut it) {
            Ok(v) => v,
            Err(err) => return Err(vec![err])
        });
    }

    let mut flags = FlagSetConstructor::new()
        .add_set(
            &"optimizations",
            FlagSetConstructor::new().add_bool_def(&"identical_expressions", true),
        )
        .add_bool_def(&"point_bounds", false)
        .finish();

    for flag in statements.iter().filter_map(Statement::as_flag) {
        set_flag(&mut flags, flag, &context);
    }

    context.flags = flags;

    for stat in statements {
        // Unroll the statement
        let res = match stat {
            Statement::Noop(_) | Statement::Flag(_) => Ok(()),
            Statement::Let(stat) => match unroll_let(stat, &mut context, &library) {
                Ok(nodes) => {
                    for node in nodes {
                        figure.push_boxed(node);
                    }

                    Ok(())
                },
                Err(x) => Err(x)
            },
            Statement::Rule(stat) => unroll_rulestat(&stat, &mut context, &library),
        };
        context.ok(res);
    }

    // for v in context.variables.values() {
    //     println!("let {} = {}", v.name, v.definition);
    // }

    // for x in &unrolled {
    //     println!("{x}");
    // }

    if context.valid() {
        Ok((context, figure))
    } else {
        Err(context.take_errors())
    }
}
