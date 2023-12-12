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

use std::{ops::Deref, fmt::Debug, collections::HashMap};

use crate::{script::{figure::{MathString, Figure, Mode}, token::{PointCollectionItem, NamedIdent}, compile::{Compiler, Compile}, parser::{FromProperty, PropertyValue, Parse}, Error}, span};

use super::{Expr, Point, Circle, Displayed, Line, Scalar, PointCollection, Bundle, Properties, CompileContext, CloneWithNode, Unknown};

#[derive(Debug, Clone)]
pub enum IdentOrItem {
    Ident(NamedIdent),
    PointCollectionItem(PointCollectionItem)
}

/// A node is a trait characterising objects meant to be parts of the figure's display tree.
pub trait Node: Debug {
    fn set_display(&mut self, display: bool);

    fn get_display(&self) -> bool;

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure);
}

pub trait FromExpr<T: Displayed>: Node + Sized {
    #[must_use]
    fn from_expr(expr: &Expr<T>, display: Properties, context: &CompileContext) -> Self;
}

#[derive(Debug, Clone)]
pub struct MaybeUnset<T> {
    value: T,
    set: bool
}

impl<T> MaybeUnset<T> {
    pub fn new(default: T) -> Self {
        Self {
            value: default,
            set: false
        }
    }

    pub fn new_or(default: T, value: Option<T>) -> Self {
        let set = value.is_some();

        Self {
            value: value.unwrap_or(default),
            set
        }
    }

    pub fn is_set(&self) -> bool {
        self.set
    }

    pub fn set(&mut self, value: T) {
        self.value = value;
        self.set = true;
    }

    /// Will set the value if `value` is `Some`. If `value` is `None`, the state of the object is unchanged.
    pub fn try_set(&mut self, value: Option<T>) {
        if let Some(value) = value {
            self.value = value;
            self.set = true;
        }
    }

    pub fn unwrap(self) -> T {
        self.value
    }

    #[must_use]
    pub fn map<U, P: FnOnce(T) -> U>(self, f: P) -> MaybeUnset<U> {
        MaybeUnset {
            value: f(self.value),
            set: self.set
        }
    }
}

impl<T> AsRef<T> for MaybeUnset<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T: Clone> MaybeUnset<T> {
    pub fn cloned(&self) -> T {
        self.value.clone()
    }
}

impl<T: Copy> MaybeUnset<T> {
    pub fn copied(&self) -> T {
        self.value
    }
}

impl<T> Deref for MaybeUnset<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Default> Default for MaybeUnset<T> {
    fn default() -> Self {
        Self {
            value: T::default(),
            set: false
        }
    }
}

#[derive(Debug)]
pub struct CollectionNode {
    pub display: MaybeUnset<bool>,
    pub children: Vec<Box<dyn Node>>
}

impl CollectionNode {
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: Vec::new()
        }
    }

    pub fn push<T: Node + 'static>(&mut self, node: T) {
        self.children.push(Box::new(node));
    }

    pub fn push_boxed(&mut self, node: Box<dyn Node>) {
        self.children.push(node);
    }

    pub fn extend<T: Node + 'static, U: IntoIterator<Item = T>>(&mut self, nodes: U) {
        self.children.extend(nodes.into_iter().map(|x| Box::new(x) as Box::<dyn Node>));
    }
}

impl Node for CollectionNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.copied()
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.copied() {
            for child in &self.children {
                child.build(compiler, figure);
            }
        }
    }
}

pub trait BuildAssociated<T: Node>: Debug {
    fn build_associated(&self, compiler: &mut Compiler, figure: &mut Figure, associated: &HierarchyNode<T>);
}

#[derive(Debug)]
pub enum AssociatedData {
    Bool(MaybeUnset<bool>)
}

impl AssociatedData {
    #[must_use]
    pub fn as_bool(&self) -> MaybeUnset<bool> {
        match self {
            Self::Bool(v) => v,
        }
    }
}

/// Contains a root node, apart from its children. Simulates a hierarchy.
#[derive(Debug)]
pub struct HierarchyNode<T: Node> {
    pub root: Box<T>,
    pub children: Vec<Box<dyn Node>>,
    pub associated: Option<Box<dyn BuildAssociated<T>>>,
    pub associated_data: HashMap<&'static str, AssociatedData>
}

impl<T: Node> Node for HierarchyNode<T> {
    fn get_display(&self) -> bool {
        self.root.get_display()
    }

    fn set_display(&mut self, display: bool) {
        self.root.set_display(display);
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        if self.root.get_display() {
            self.root.build(compiler, figure);

            if let Some(associated) = &self.associated {
                associated.build_associated(compiler, figure, self);
            }

            for child in &self.children {
                child.build(compiler, figure);
            }
        }
    }
}

impl<U: Displayed, T: FromExpr<U>> FromExpr<U> for HierarchyNode<T> {
    fn from_expr(expr: &Expr<U>, props: Properties, context: &CompileContext) -> Self {
        Self {
            root: Box::new(T::from_expr(expr, props, context)),
            children: Vec::new(),
            associated: None,
            associated_data: HashMap::new()
        }
    }
}

impl<T: Node> HierarchyNode<T> {
    pub fn new(root: T) -> Self {
        Self {
            root: Box::new(root),
            children: Vec::new(),
            associated: None,
            associated_data: HashMap::new()
        }
    }

    pub fn push_child<U: Node + 'static>(&mut self, node: U) {
        self.children.push(Box::new(node));
    }

    pub fn extend_boxed<Iter: IntoIterator<Item = Box<dyn Node>>>(&mut self, nodes: Iter) {
        self.children.extend(nodes.into_iter());
    }

    pub fn extend_children<U: Node + 'static, Iter: IntoIterator<Item = U>>(&mut self, nodes: Iter) {
        self.children.extend(nodes.into_iter().map(|x| Box::new(x) as Box::<dyn Node>));
    }

    pub fn set_associated(&mut self, associated: Box<dyn BuildAssociated<T>>) {
        self.associated = Some(associated);
    }

    pub fn insert_data(&mut self, key: &'static str, data: AssociatedData) {
        self.associated_data.insert(key, data);
    }

    pub fn get_data(&self, key: &'static str) -> Option<&AssociatedData> {
        self.associated_data.get(key)
    }
}

#[derive(Debug)]
pub struct PCNode {
    pub display: MaybeUnset<bool>,
    pub children: Vec<Option<HierarchyNode<<Point as Displayed>::Node>>>
}

impl PCNode {
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: Vec::new()
        }
    }

    pub fn push(&mut self, node: Option<HierarchyNode<<Point as Displayed>::Node>>) {
        self.children.push(node);
    }

    pub fn extend<U: IntoIterator<Item = Option<HierarchyNode<<Point as Displayed>::Node>>>>(&mut self, nodes: U) {
        self.children.extend(nodes);
    }
}

impl Node for PCNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.copied()
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.copied() {
            for child in self.children.iter().flatten() {
                child.build(compiler, figure);
            }
        }
    }
}

macro_rules! impl_from_for_any {
    ($from:ident) => {
        impl From<HierarchyNode<<$from as Displayed>::Node>> for AnyExprNode {
            fn from(value: HierarchyNode<<$from as Displayed>::Node>) -> Self {
                Self::$from(value)
            }
        }
    }
}

#[derive(Debug)]
pub enum AnyExprNode {
    Point(HierarchyNode<<Point as Displayed>::Node>),
    Line(HierarchyNode<<Line as Displayed>::Node>),
    Circle(HierarchyNode<<Circle as Displayed>::Node>),
    Scalar(HierarchyNode<<Scalar as Displayed>::Node>),
    PointCollection(HierarchyNode<<PointCollection as Displayed>::Node>),
    Bundle(HierarchyNode<<Bundle as Displayed>::Node>),
    Unknown(HierarchyNode<<Unknown as Displayed>::Node>)
}

impl_from_for_any! {Point}
// impl_from_for_any! {Line}
// impl_from_for_any! {Circle}
// impl_from_for_any! {Scalar}
impl_from_for_any! {PointCollection}
impl_from_for_any! {Bundle}

impl AnyExprNode {
    #[must_use]
    pub fn as_dyn(self) -> Box<dyn Node> {
        match self {
            Self::Point(v) => Box::new(v),
            Self::Line(v) => Box::new(v),
            Self::Circle(v) => Box::new(v),
            Self::Scalar(v) => Box::new(v),
            Self::PointCollection(v) => Box::new(v),
            Self::Bundle(v) => Box::new(v),
            Self::Unknown(v) => Box::new(v),
        }
    }

    /// # Panics
    /// If the node is not a point node.
    #[must_use]
    pub fn as_point(self) -> HierarchyNode<<Point as Displayed>::Node> {
        if let Self::Point(v) = self {
            v
        } else {
            panic!("not a point")
        }
    }

    /// # Panics
    /// If the node is not a line node.
    #[must_use]
    pub fn as_line(self) -> HierarchyNode<<Line as Displayed>::Node> {
        if let Self::Line(v) = self {
            v
        } else {
            panic!("not a line")
        }
    }

    /// # Panics
    /// If the node is not a circle node.
    #[must_use]
    pub fn as_circle(self) -> HierarchyNode<<Circle as Displayed>::Node> {
        if let Self::Circle(v) = self {
            v
        } else {
            panic!("not a circle")
        }
    }

    /// # Panics
    /// If the node is not a scalar node.
    #[must_use]
    pub fn as_scalar(self) -> HierarchyNode<<Scalar as Displayed>::Node> {
        if let Self::Scalar(v) = self {
            v
        } else {
            panic!("not a scalar")
        }
    }

    /// # Panics
    /// If the node is not a point collection node.
    #[must_use]
    pub fn as_point_collection(self) -> HierarchyNode<<PointCollection as Displayed>::Node> {
        if let Self::PointCollection(v) = self {
            v
        } else {
            panic!("not a point collection")
        }
    }

    /// # Panics
    /// If the node is not a bundle node.
    #[must_use]
    pub fn as_bundle(self) -> HierarchyNode<<Bundle as Displayed>::Node> {
        if let Self::Bundle(v) = self {
            v
        } else {
            panic!("not a bundle")
        }
    }

    /// # Panics
    /// If the node is not an unknown node.
    #[must_use]
    pub fn as_unknown(self) -> HierarchyNode<<Unknown as Displayed>::Node> {
        if let Self::Unknown(v) = self {
            v
        } else {
            panic!("not a unknown")
        }
    }
}

impl Node for AnyExprNode {
    fn set_display(&mut self, display: bool) {
        match self {
            Self::Point(v) => v.set_display(display),
            Self::Line(v) => v.set_display(display),
            Self::Circle(v) => v.set_display(display),
            Self::Scalar(v) => v.set_display(display),
            Self::PointCollection(v) => v.set_display(display),
            Self::Bundle(v) => v.set_display(display),
            Self::Unknown(v) => v.set_display(display)
        }
    }

    fn get_display(&self) -> bool {
        match self {
            Self::Point(v) => v.get_display(),
            Self::Line(v) => v.get_display(),
            Self::Circle(v) => v.get_display(),
            Self::Scalar(v) => v.get_display(),
            Self::PointCollection(v) => v.get_display(),
            Self::Bundle(v) => v.get_display(),
            Self::Unknown(v) => v.get_display()
        }
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        match self {
            Self::Point(v) => v.build(compiler, figure),
            Self::Line(v) => v.build(compiler, figure),
            Self::Circle(v) => v.build(compiler, figure),
            Self::Scalar(v) => v.build(compiler, figure),
            Self::PointCollection(v) => v.build(compiler, figure),
            Self::Bundle(v) => v.build(compiler, figure),
            Self::Unknown(v) => v.build(compiler, figure)
        }
    }
}

type BundleNodeItem = Option<AnyExprNode>;

#[derive(Debug)]
pub struct BundleNode {
    pub display: MaybeUnset<bool>,
    pub children: HashMap<String, BundleNodeItem>
}

impl BundleNode {
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: HashMap::new()
        }
    }

    pub fn insert<T>(&mut self, key: String, node: Option<T>)
        where AnyExprNode: From<T> {
        self.children.insert(key, node.map(AnyExprNode::from));
    }

    pub fn extend<U: IntoIterator<Item = (String, BundleNodeItem)>>(&mut self, nodes: U) {
        self.children.extend(nodes);
    }
}

impl Node for BundleNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.copied()
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.copied() {
            for child in self.children.values().flatten() {
                child.build(compiler, figure);
            }
        }
    }
}

#[derive(Debug)]
pub struct EmptyNode;

impl Node for EmptyNode {
    fn get_display(&self) -> bool {
        false
    }

    fn set_display(&mut self, _display: bool) {}

    fn build(&self, _compiler: &mut Compiler, _figure: &mut Figure) {}
}

#[derive(Debug)]
pub struct PointNode {
    pub display: MaybeUnset<bool>,
    pub label: MaybeUnset<MathString>,
    pub display_label: MaybeUnset<bool>,
    pub display_dot: MaybeUnset<bool>,
    pub default_label: MathString,
    pub expr: Expr<Point>
}

impl Node for PointNode {
    fn get_display(&self) -> bool {
        self.display.copied()
    }

    fn set_display(&mut self, display: bool) {
        self.display.set(display)
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.copied() {
            figure.points.push((
                compiler.compile(&self.expr),
                if self.display_label.copied() {
                    let label = self.label.as_ref();

                    if label.is_empty() {
                        self.default_label.clone()
                    } else {
                        label.clone()
                    }
                } else {
                    MathString::new(span!(0, 0, 0, 0))
                }
            ))
        }
    }
}

impl FromExpr<Point> for PointNode {
    fn from_expr(expr: &Expr<Point>, mut props: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props.get("label").maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(true),
            display_dot: props.get("display_dot").maybe_unset(true),
            default_label: props.get("default-label").get_or(MathString::new(span!(0, 0, 0, 0))),
            expr: expr.clone_without_node()
        };

        props.finish(&["display", "label", "display_label", "display_dot"], context);

        node
    }
}

#[derive(Debug)]
pub struct CircleNode {
    pub display: MaybeUnset<bool>,
    pub label: MaybeUnset<MathString>,
    pub display_label: MaybeUnset<bool>,
    pub default_label: MathString,
    pub expr: Expr<Circle>
}

impl Node for CircleNode {
    fn get_display(&self) -> bool {
        self.display.copied()
    }

    fn set_display(&mut self, display: bool) {
        self.display.set(display)
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.copied() {
            figure.circles.push((
                compiler.compile(&self.expr),
                Mode::Default
                // if self.display_label.unwrap() {
                //     let label = self.label.unwrap();

                //     if label.is_empty() {
                //         self.default_label
                //     } else {
                //         label
                //     }
                // } else {
                //     MathString::new()
                // }
            ))
        }
    }
}

impl FromExpr<Circle> for CircleNode {
    fn from_expr(expr: &Expr<Circle>, mut props: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props.get("label").maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(false),
            default_label: props.get("default-label").get_or(MathString::new(span!(0, 0, 0, 0))),
            expr: expr.clone_without_node()
        };

        props.finish(&["display", "label", "display_label"], context);

        node
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LineType {
    Line,
    Ray,
    Segment
}

impl FromProperty for LineType {
    fn from_property(property: PropertyValue) -> Result<Self, Error> {
        match property {
            PropertyValue::Number(n) => Err(Error::StringOrIdentExpected {
                error_span: n.get_span(),
            }),
            PropertyValue::Ident(i) => match i.to_string().to_lowercase().as_str() {
                "line" => Ok(Self::Line),
                "ray" => Ok(Self::Ray),
                "segment" => Ok(Self::Segment),
                &_ => Err(Error::StringOrIdentExpected {
                    error_span: i.get_span(),
                })
            },
            PropertyValue::MathString(_) => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct LineNode {
    pub display: MaybeUnset<bool>,
    pub label: MaybeUnset<MathString>,
    pub display_label: MaybeUnset<bool>,
    pub line_type: MaybeUnset<LineType>,
    pub expr: Expr<Line>
}

impl Node for LineNode {
    fn get_display(&self) -> bool {
        self.display.copied()
    }

    fn set_display(&mut self, display: bool) {
        self.display.set(display)
    }

    fn build(&self, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.copied() {
            figure.lines.push((
                compiler.compile(&self.expr),
                Mode::Default
                // if self.display_label.unwrap() {
                //     let label = self.label.unwrap();

                //     if label.is_empty() {
                //         self.default_label
                //     } else {
                //         label
                //     }
                // } else {
                //     MathString::new()
                // }
            ))
        }
    }
}

impl FromExpr<Line> for LineNode {
    fn from_expr(expr: &Expr<Line>, mut props: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props.get("label").maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(false),
            line_type: props.get("line_type").maybe_unset(LineType::Line),
            expr: expr.clone_without_node()
        };

        props.finish(&["display", "label", "display_label", "line_type"], context);

        node
    }
}

#[derive(Debug)]
pub struct ScalarNode {
    pub display: MaybeUnset<bool>
}

impl Node for ScalarNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.copied()
    }

    fn build(&self, _compiler: &mut Compiler, _figure: &mut Figure) {}
}

impl FromExpr<Scalar> for ScalarNode {
    fn from_expr(_expr: &Expr<Scalar>, mut props: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: props.get("display").maybe_unset(true)
        };

        props.finish(&["display"], context);

        node
    }
}