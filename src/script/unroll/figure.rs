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

use crate::{script::{figure::{MathString, Figure, Mode}, token::{PointCollectionItem, NamedIdent}, compile::{Compiler, Compile}, Error}, span};

use super::{Expr, Point, Circle, Displayed, Line, Scalar, PointCollection, Bundle, Properties};

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
    pub fn map<U, P: FnOnce(T) -> U>(self, func: P) -> Self {
        Self {
            value: func(self.value),
            ..self
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

/// Contains a root node, apart from its children. Simulates a hierarchy.
#[derive(Debug)]
pub struct HierarchyNode<T: Node> {
    pub root: T,
    pub children: Vec<Box<dyn Node>>
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

            for child in &self.children {
                child.build(compiler, figure);
            }
        }
    }
}

impl<T: Node> HierarchyNode<T> {
    pub fn new(root: T) -> Self {
        Self {
            root,
            children: Vec::new()
        }
    }

    pub fn push_child<U: Node + 'static>(&mut self, node: U) {
        self.children.push(Box::new(node));
    }

    pub fn extend_children<U: Node + 'static, Iter: IntoIterator<Item = U>>(&mut self, nodes: Iter) {
        self.children.extend(nodes.into_iter().map(|x| Box::new(x) as Box::<dyn Node>));
    }
}

#[derive(Debug)]
pub struct PCNode {
    pub display: MaybeUnset<bool>,
    pub children: Vec<Option<<Point as Displayed>::Node>>
}

impl PCNode {
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: Vec::new()
        }
    }

    pub fn push(&mut self, node: Option<<Point as Displayed>::Node>) {
        self.children.push(node);
    }

    pub fn extend<U: IntoIterator<Item = Option<<Point as Displayed>::Node>>>(&mut self, nodes: U) {
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
        impl From<<$from as Displayed>::Node> for AnyExprNode {
            fn from(value: <$from as Displayed>::Node) -> Self {
                Self::$from(value)
            }
        }
    }
}

#[derive(Debug)]
pub enum AnyExprNode {
    Point(<Point as Displayed>::Node),
    Line(<Line as Displayed>::Node),
    Circle(<Circle as Displayed>::Node),
    Scalar(<Scalar as Displayed>::Node),
    PointCollection(<PointCollection as Displayed>::Node),
    Bundle(<Bundle as Displayed>::Node),
}

impl_from_for_any! {Point}
// impl_from_for_any! {Line}
// impl_from_for_any! {Circle}
// impl_from_for_any! {Scalar}
impl_from_for_any! {PointCollection}
impl_from_for_any! {Bundle}

impl AnyExprNode {
    /// # Panics
    /// If the node is not a point node.
    #[must_use]
    pub fn as_point(self) -> <Point as Displayed>::Node {
        if let Self::Point(v) = self {
            v
        } else {
            panic!("not a point")
        }
    }

    /// # Panics
    /// If the node is not a line node.
    #[must_use]
    pub fn as_line(self) -> <Line as Displayed>::Node {
        if let Self::Line(v) = self {
            v
        } else {
            panic!("not a line")
        }
    }

    /// # Panics
    /// If the node is not a circle node.
    #[must_use]
    pub fn as_circle(self) -> <Circle as Displayed>::Node {
        if let Self::Circle(v) = self {
            v
        } else {
            panic!("not a circle")
        }
    }

    /// # Panics
    /// If the node is not a scalar node.
    #[must_use]
    pub fn as_scalar(self) -> <Scalar as Displayed>::Node {
        if let Self::Scalar(v) = self {
            v
        } else {
            panic!("not a scalar")
        }
    }

    /// # Panics
    /// If the node is not a point collection node.
    #[must_use]
    pub fn as_point_collection(self) -> <PointCollection as Displayed>::Node {
        if let Self::PointCollection(v) = self {
            v
        } else {
            panic!("not a point collection")
        }
    }

    /// # Panics
    /// If the node is not a bundle node.
    #[must_use]
    pub fn as_bundle(self) -> <Bundle as Displayed>::Node {
        if let Self::Bundle(v) = self {
            v
        } else {
            panic!("not a bundle")
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

impl PointNode {
    #[must_use]
    fn new(expr: Expr<Point>, mut props: Properties) -> Result<Self, Error> {
        let node = Self {
            display: props.get_bool("display")?.maybe_unset(true),
            label: props.get_math_string("label")?.maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get_bool("display_label")?.maybe_unset(true),
            display_dot: props.get_bool("display_dot")?.maybe_unset(true),
            default_label: props.get_math_string("default-label")?.get_or(MathString::new(span!(0, 0, 0, 0))),
            expr
        };

        props.finish();

        Ok(node)
    }
}

#[derive(Debug)]
pub struct CircleNode {
    pub display: MaybeUnset<bool>,
    pub label: MaybeUnset<MathString>,
    pub display_label: MaybeUnset<bool>,
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