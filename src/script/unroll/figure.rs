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

use std::{collections::HashMap, fmt::Debug, ops::Deref};

use crate::{
    script::{
        compile::{Compile, Compiler},
        figure::{Figure, MathString, Style},
        parser::{FromProperty, Parse, PropertyValue},
        Error,
    },
    span,
};

use super::{
    Bundle, Circle, CloneWithNode, CompileContext, Displayed, Expr, Line, Point, PointCollection,
    Properties, Scalar, Unknown, AnyExpr,
};

/// A node is a trait characterising objects meant to be parts of the figure's display tree.
pub trait Node: Debug {
    fn set_display(&mut self, display: bool);

    fn get_display(&self) -> bool;

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure);

    fn build_unboxed(self, compiler: &mut Compiler, figure: &mut Figure) where Self: Sized {
        Box::new(self).build(compiler, figure);
    }
}

pub trait FromExpr<T: Displayed>: Node + Sized {
    #[must_use]
    fn from_expr(expr: &Expr<T>, display: Properties, context: &CompileContext) -> Self;
}

#[derive(Debug, Clone, Copy)]
pub struct MaybeUnset<T> {
    value: T,
    set: bool,
}

impl<T> MaybeUnset<T> {
    pub fn new(default: T) -> Self {
        Self {
            value: default,
            set: false,
        }
    }

    pub fn new_or(default: T, value: Option<T>) -> Self {
        let set = value.is_some();

        Self {
            value: value.unwrap_or(default),
            set,
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

    /// Only sets the value if the `set` flag is false.
    /// Returns whether a new value was set.
    pub fn set_if_unset(&mut self, value: T) -> bool {
        let set = !self.set;

        if set {
            self.set(value);
        }

        set
    }

    /// Only sets the value if the `set` flag is false and `value` is `Some`.
    /// Returns whether a new value was set.
    pub fn try_set_if_unset(&mut self, value: Option<T>) -> bool {
        let set = !self.set && value.is_some();

        if let Some(value) = value {
            if set {
                self.set(value);
            }
        }

        set
    }

    #[must_use]
    pub fn get(&self) -> &T {
        &self.value
    }

    /// Will return a `Some` only if the value has been set.
    pub fn try_get(&self) -> Option<&T> {
        if self.set {
            Some(self.get())
        } else {
            None
        }
    }

    pub fn unwrap(self) -> T {
        self.value
    }

    #[must_use]
    pub fn map<U, P: FnOnce(T) -> U>(self, f: P) -> MaybeUnset<U> {
        MaybeUnset {
            value: f(self.value),
            set: self.set,
        }
    }
}

impl<T> AsRef<T> for MaybeUnset<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T: Clone> MaybeUnset<T> {
    #[must_use]
    pub fn get_cloned(&self) -> T {
        self.value.clone()
    }

    #[must_use]
    pub fn cloned(&self) -> Self {
        self.clone()
    }
}

impl<T: Copy> MaybeUnset<T> {
    #[must_use]
    pub fn get_copied(&self) -> T {
        self.value
    }

    #[must_use]
    pub fn copied(&self) -> Self {
        *self
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
            set: false,
        }
    }
}

#[derive(Debug)]
pub struct CollectionNode {
    pub display: MaybeUnset<bool>,
    pub children: Vec<Box<dyn Node>>,
}

impl Default for CollectionNode {
    fn default() -> Self {
        Self::new()
    }
}

impl CollectionNode {
    #[must_use]
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: Vec::new(),
        }
    }

    #[must_use]
    pub fn from_display(mut display: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: display.get("display").maybe_unset(true),
            children: Vec::new()
        };

        display.finish(context);

        node
    }

    pub fn push<T: Node + 'static>(&mut self, node: T) {
        self.children.push(Box::new(node));
    }

    pub fn push_boxed(&mut self, node: Box<dyn Node>) {
        self.children.push(node);
    }

    pub fn extend<T: Node + 'static, U: IntoIterator<Item = T>>(&mut self, nodes: U) {
        self.children
            .extend(nodes.into_iter().map(|x| Box::new(x) as Box<dyn Node>));
    }
}

impl Node for CollectionNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.unwrap() {
            for child in self.children {
                child.build(compiler, figure);
            }
        }
    }
}

pub trait BuildAssociated<T: Node>: Debug {
    fn build_associated(
        self: Box<Self>,
        compiler: &mut Compiler,
        figure: &mut Figure,
        associated: &mut HierarchyNode<T>,
    );
}

#[derive(Debug)]
pub enum AssociatedData {
    Bool(MaybeUnset<bool>),
    Style(MaybeUnset<Style>),
    LineType(MaybeUnset<LineType>)
}

impl AssociatedData {
    #[must_use]
    pub fn as_bool(&self) -> Option<MaybeUnset<bool>> {
        match self {
            Self::Bool(v) => Some(v.copied()),
            _ => None
        }
    }

    #[must_use]
    pub fn as_style(&self) -> Option<MaybeUnset<Style>> {
        match self {
            Self::Style(v) => Some(v.copied()),
            _ => None
        }
    }

    #[must_use]
    pub fn as_line_type(&self) -> Option<MaybeUnset<LineType>> {
        match self {
            Self::LineType(v) => Some(v.copied()),
            _ => None
        }
    }
}

impl From<MaybeUnset<bool>> for AssociatedData {
    fn from(value: MaybeUnset<bool>) -> Self {
        Self::Bool(value)
    }
}

impl From<MaybeUnset<Style>> for AssociatedData {
    fn from(value: MaybeUnset<Style>) -> Self {
        Self::Style(value)
    }
}

impl From<MaybeUnset<LineType>> for AssociatedData {
    fn from(value: MaybeUnset<LineType>) -> Self {
        Self::LineType(value)
    }
}

/// Contains a root node, apart from its children. Simulates a hierarchy.
#[derive(Debug)]
pub struct HierarchyNode<T: Node> {
    pub root: Box<T>,
    pub children: Vec<Box<dyn Node>>,
    pub associated: Option<Box<dyn BuildAssociated<T>>>,
    pub associated_data: HashMap<&'static str, AssociatedData>,
}

impl<T: Node> Node for HierarchyNode<T> {
    fn get_display(&self) -> bool {
        self.root.get_display()
    }

    fn set_display(&mut self, display: bool) {
        self.root.set_display(display);
    }

    fn build(mut self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        if self.root.get_display() {
            if let Some(associated) = self.associated.take() {
                associated.build_associated(compiler, figure, &mut self);
            }

            self.root.build(compiler, figure);

            for child in self.children {
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
            associated_data: HashMap::new(),
        }
    }
}

impl<T: Node> HierarchyNode<T> {
    pub fn new(root: T) -> Self {
        Self {
            root: Box::new(root),
            children: Vec::new(),
            associated: None,
            associated_data: HashMap::new(),
        }
    }

    pub fn push_child<U: Node + 'static>(&mut self, node: U) {
        self.children.push(Box::new(node));
    }

    pub fn extend_boxed<Iter: IntoIterator<Item = Box<dyn Node>>>(&mut self, nodes: Iter) {
        self.children.extend(nodes);
    }

    pub fn extend_children<U: Node + 'static, Iter: IntoIterator<Item = U>>(
        &mut self,
        nodes: Iter,
    ) {
        self.children
            .extend(nodes.into_iter().map(|x| Box::new(x) as Box<dyn Node>));
    }

    pub fn set_associated<U: BuildAssociated<T> + 'static>(&mut self, associated: U) {
        self.associated = Some(Box::new(associated));
    }

    pub fn insert_data<U: Into<AssociatedData>>(&mut self, key: &'static str, data: U) {
        self.associated_data.insert(key, data.into());
    }

    #[must_use]
    pub fn get_data(&self, key: &'static str) -> Option<&AssociatedData> {
        self.associated_data.get(key)
    }
}

#[derive(Debug)]
pub struct PCNode {
    pub display: MaybeUnset<bool>,
    pub children: Vec<Option<HierarchyNode<<Point as Displayed>::Node>>>,
    pub props: Option<Properties>,
}

impl Default for PCNode {
    fn default() -> Self {
        Self::new()
    }
}

impl PCNode {
    #[must_use]
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: Vec::new(),
            props: None,
        }
    }

    pub fn push(&mut self, node: Option<HierarchyNode<<Point as Displayed>::Node>>) {
        self.children.push(node);
    }

    pub fn extend<U: IntoIterator<Item = Option<HierarchyNode<<Point as Displayed>::Node>>>>(
        &mut self,
        nodes: U,
    ) {
        self.children.extend(nodes);
    }
}

impl Node for PCNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.unwrap() {
            for child in self.children.into_iter().flatten() {
                child.build_unboxed(compiler, figure);
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
    };
}

#[derive(Debug)]
pub enum AnyExprNode {
    Point(HierarchyNode<<Point as Displayed>::Node>),
    Line(HierarchyNode<<Line as Displayed>::Node>),
    Circle(HierarchyNode<<Circle as Displayed>::Node>),
    Scalar(HierarchyNode<<Scalar as Displayed>::Node>),
    PointCollection(HierarchyNode<<PointCollection as Displayed>::Node>),
    Bundle(HierarchyNode<<Bundle as Displayed>::Node>),
    Unknown(HierarchyNode<<Unknown as Displayed>::Node>),
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
            Self::Unknown(v) => v.set_display(display),
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
            Self::Unknown(v) => v.get_display(),
        }
    }

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        match *self {
            Self::Point(v) => v.build_unboxed(compiler, figure),
            Self::Line(v) => v.build_unboxed(compiler, figure),
            Self::Circle(v) => v.build_unboxed(compiler, figure),
            Self::Scalar(v) => v.build_unboxed(compiler, figure),
            Self::PointCollection(v) => v.build_unboxed(compiler, figure),
            Self::Bundle(v) => v.build_unboxed(compiler, figure),
            Self::Unknown(v) => v.build_unboxed(compiler, figure),
        }
    }
}

#[derive(Debug)]
pub struct BundleNode {
    pub display: MaybeUnset<bool>,
    pub children: HashMap<String, AnyExpr>,
}

impl Default for BundleNode {
    fn default() -> Self {
        Self::new()
    }
}

impl BundleNode {
    #[must_use]
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: HashMap::new(),
        }
    }

    pub fn insert<T: Displayed>(&mut self, key: String, expr: Expr<T>)
    where AnyExpr: From<Expr<T>> {
        self.children.insert(key, AnyExpr::from(expr));
    }

    pub fn extend<U: IntoIterator<Item = (String, AnyExpr)>>(&mut self, nodes: U) {
        self.children.extend(nodes);
    }
}

impl Node for BundleNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.get_copied() {
            for mut child in self.children.into_values() {
                if let Some(node) = child.replace_node(None) {
                    node.build_unboxed(compiler, figure);
                }
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

    fn build(self: Box<Self>, _compiler: &mut Compiler, _figure: &mut Figure) {}
}

#[derive(Debug)]
pub struct PointNode {
    pub display: MaybeUnset<bool>,
    pub label: MaybeUnset<MathString>,
    pub display_label: MaybeUnset<bool>,
    pub display_dot: MaybeUnset<bool>,
    pub default_label: MathString,
    pub expr: Expr<Point>,
}

impl Node for PointNode {
    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.unwrap() {
            figure.points.push((
                compiler.compile(&self.expr),
                if self.display_label.unwrap() {
                    if self.label.as_ref().is_empty() {
                        self.default_label
                    } else {
                        self.label.unwrap()
                    }
                } else {
                    MathString::new(span!(0, 0, 0, 0))
                },
            ));
        }
    }
}

impl FromExpr<Point> for PointNode {
    fn from_expr(expr: &Expr<Point>, mut props: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props
                .get("label")
                .maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(true),
            display_dot: props.get("display_dot").maybe_unset(true),
            default_label: props
                .get("default-label")
                .get_or(MathString::new(span!(0, 0, 0, 0))),
            expr: expr.clone_without_node(),
        };

        props.finish(context);

        node
    }
}

#[derive(Debug)]
pub struct CircleNode {
    pub display: MaybeUnset<bool>,
    pub label: MaybeUnset<MathString>,
    pub display_label: MaybeUnset<bool>,
    pub default_label: MathString,
    pub style: MaybeUnset<Style>,
    pub expr: Expr<Circle>,
}

impl Node for CircleNode {
    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.unwrap() {
            figure.circles.push((
                compiler.compile(&self.expr),
                Style::Solid, // if self.display_label.unwrap() {
                               //     let label = self.label.unwrap();

                               //     if label.is_empty() {
                               //         self.default_label
                               //     } else {
                               //         label
                               //     }
                               // } else {
                               //     MathString::new()
                               // }
            ));
        }
    }
}

impl FromExpr<Circle> for CircleNode {
    fn from_expr(expr: &Expr<Circle>, mut props: Properties, context: &CompileContext) -> Self {
        let _ = props.get::<MathString>("default-label");
        
        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props
                .get("label")
                .maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(false),
            default_label: props
                .get("default-label")
                .get_or(MathString::new(span!(0, 0, 0, 0))),
            style: props.get("style").maybe_unset(Style::default()),
            expr: expr.clone_without_node(),
        };

        props.finish(context);

        node
    }
}

macro_rules! property_enum_impl {
    ($name:ident { $($variant:ident: $key:literal),* $(,)? }) => {
        impl FromProperty for $name {
            fn from_property(property: PropertyValue) -> Result<Self, Error> {
                match property {
                    PropertyValue::Number(n) => Err(Error::StringOrIdentExpected {
                        error_span: n.get_span(),
                    }),
                    PropertyValue::Ident(i) => match i.to_string().to_lowercase().as_str() {
                        $($key => Ok(Self::$variant),)*
                        &_ => Err(Error::EnumInvalidValue {
                            error_span: i.get_span(),
                            available_values: &[$($key),*],
                            received_value: i.to_string(),
                        }),
                    },
                    PropertyValue::String(s) => match s.content.to_lowercase().as_str() {
                        $($key => Ok(Self::$variant),)*
                        &_ => Err(Error::EnumInvalidValue {
                            error_span: s.get_span(),
                            available_values: &[$($key),*],
                            received_value: s.content,
                        }),
                    },
                    PropertyValue::RawString(s) => Err(Error::NonRawStringOrIdentExpected {
                        error_span: s.get_span(),
                    }),
                }
            }
        }
    };
}

macro_rules! property_enum {
    ($name:ident { $($variant:ident: $key:literal),* $(,)? }) => {
        #[derive(Debug, Clone, Copy)]
        pub enum $name {
            $($variant),*
        }

        property_enum_impl! {$name { $($variant: $key),* }}
    };
}

property_enum! {
    LineType {
        Line: "line",
        Ray: "ray",
        Segment: "segment"
    }
}

property_enum_impl! {
    Style {
        Solid: "solid",
        Dashed: "dashed",
        Dotted: "dotted",
        Bold: "bold"
    }
}

#[derive(Debug)]
pub struct LineNode {
    pub display: MaybeUnset<bool>,
    pub label: MaybeUnset<MathString>,
    pub display_label: MaybeUnset<bool>,
    pub line_type: MaybeUnset<LineType>,
    pub style: MaybeUnset<Style>,
    pub expr: Expr<Line>,
}

impl Node for LineNode {
    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn build(self: Box<Self>, compiler: &mut Compiler, figure: &mut Figure) {
        if self.display.unwrap() {
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

            match self.line_type.unwrap() {
                LineType::Line => figure
                    .lines
                    .push((compiler.compile(&self.expr), Style::Solid)),
                LineType::Ray => match &self.expr.data.as_ref() {
                    Line::LineFromPoints(a, b) => {
                        figure
                            .rays
                            .push((compiler.compile(a), compiler.compile(b), Style::Solid));
                    }
                    Line::AngleBisector(a, b, c) => {
                        let x = Expr::new_spanless(Point::LineLineIntersection(
                            Expr::new_spanless(Line::LineFromPoints(
                                a.clone_without_node(),
                                c.clone_without_node(),
                            )),
                            self.expr.clone_without_node(),
                        ));

                        figure
                            .rays
                            .push((compiler.compile(b), compiler.compile(&x), Style::Solid));
                    }
                    _ => unreachable!(),
                },
                LineType::Segment => match &self.expr.data.as_ref() {
                    Line::LineFromPoints(a, b) => figure.segments.push((
                        compiler.compile(a),
                        compiler.compile(b),
                        Style::Solid,
                    )),
                    _ => unreachable!(),
                },
            }
        }
    }
}

impl FromExpr<Line> for LineNode {
    fn from_expr(expr: &Expr<Line>, mut props: Properties, context: &CompileContext) -> Self {
        let _ = props.get::<MathString>("default-label");

        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props
                .get("label")
                .maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(false),
            line_type: MaybeUnset::new(LineType::Line),
            style: props.get("style").maybe_unset(Style::default()),
            expr: expr.clone_without_node(),
        };

        props.finish(context);

        node
    }
}

#[derive(Debug)]
pub struct ScalarNode {
    pub display: MaybeUnset<bool>,
    pub expr: Expr<Scalar>
}

impl Node for ScalarNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, _compiler: &mut Compiler, _figure: &mut Figure) {}
}

impl FromExpr<Scalar> for ScalarNode {
    fn from_expr(expr: &Expr<Scalar>, mut props: Properties, context: &CompileContext) -> Self {
        let _ = props.get::<MathString>("label");
        let _ = props.get::<bool>("display_label");
        let _ = props.get::<MathString>("default-label");

        let node = Self {
            display: props.get("display").maybe_unset(true),
            expr: expr.clone_without_node()
        };

        props.finish(context);

        node
    }
}
