//! Everything related to unrolled figures.

use crate::figure::{CircleItem, LineItem, PointItem, RayItem, SegmentItem};
use crate::math::Build;
use crate::{
    figure::SpannedMathString as MathString,
    parser::{FromProperty, Parse, PropertyValue},
    span, Error,
};
use geo_aid_figure::Style;
use std::{collections::HashMap, fmt::Debug, ops::Deref};

use super::{
    Circle, CloneWithNode, CompileContext, Derived, Displayed, Dummy, Expr, Line, Point,
    PointCollection, Properties, Scalar, Unknown,
};

/// A node is a trait characterising objects meant to be parts of the figure's display tree.
pub trait Node: Debug {
    /// Set the general display flag that decides whether this node and its children should be displayed.
    fn set_display(&mut self, display: bool);

    /// Get whether this node should be displayed.
    fn get_display(&self) -> bool;

    /// Build this node.
    fn build(self: Box<Self>, build: &mut Build);

    /// Build this node, with an unboxed `self` type.
    fn build_unboxed(self, compiler: &mut Build)
    where
        Self: Sized,
    {
        Box::new(self).build(compiler);
    }
}

impl<T: Node + ?Sized> Node for Box<T> {
    fn set_display(&mut self, display: bool) {
        self.as_mut().set_display(display);
    }

    fn get_display(&self) -> bool {
        self.as_ref().get_display()
    }

    fn build(self: Box<Self>, build: &mut Build) {
        <T as Node>::build(*self, build);
    }
}

/// Helper trait for building nodes out of unrolled expressions.
pub trait FromExpr<T: Displayed>: Node + Sized {
    /// Build a node out of an unrolled expression.
    #[must_use]
    fn from_expr(expr: &Expr<T>, display: Properties, context: &CompileContext) -> Self;
}

/// Helper type for identifying whether a value was explicitly set or it is left default.
#[derive(Debug, Clone, Copy)]
pub struct MaybeUnset<T> {
    /// Inner value.
    value: T,
    /// Whether it was explicitly set.
    set: bool,
}

impl<T> MaybeUnset<T> {
    /// Create a new unset value.
    pub fn new(default: T) -> Self {
        Self {
            value: default,
            set: false,
        }
    }

    /// Create a new value from an `Option`. If it is `Some`, the value is considered set with
    /// the contained value. Otherwise it is considered unset with the default.
    pub fn new_or(default: T, value: Option<T>) -> Self {
        let set = value.is_some();

        Self {
            value: value.unwrap_or(default),
            set,
        }
    }

    /// Check if this value was explicitly set.
    pub fn is_set(&self) -> bool {
        self.set
    }

    /// Set this value. Explicitly.
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

    /// Get a reference to the contained value.
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

    /// Unwrap this value from the [`MaybeUnset`] wrapper. Never panics.
    pub fn unwrap(self) -> T {
        self.value
    }

    /// Map this value using a function. The `set` flag is carried over.
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
    /// Clone the underlying value.
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
    /// Copy the underlying value.
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

/// A node that represents multiple node. No hierarchy is involved.
#[derive(Debug)]
pub struct CollectionNode {
    /// Whether the node should be displayed.
    pub display: MaybeUnset<bool>,
    /// Nodes
    pub children: Vec<Box<dyn Node>>,
}

impl Default for CollectionNode {
    fn default() -> Self {
        Self::new()
    }
}

impl CollectionNode {
    /// Create a new, empty, displayed collection node.
    #[must_use]
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: Vec::new(),
        }
    }

    /// Create a new, empty collection node with given display properties
    #[must_use]
    pub fn from_display(mut display: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: display.get("display").maybe_unset(true),
            children: Vec::new(),
        };

        display.finish(context);

        node
    }

    /// Push a node.
    pub fn push<T: Node + 'static>(&mut self, node: T) {
        self.children.push(Box::new(node));
    }

    /// Push a boxed node.
    pub fn push_boxed(&mut self, node: Box<dyn Node>) {
        self.children.push(node);
    }

    /// Extend the nodes.
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

    fn build(self: Box<Self>, compiler: &mut Build) {
        if self.display.unwrap() {
            for child in self.children {
                child.build(compiler);
            }
        }
    }
}

/// Nodes can have associated nodes. They're a special kind of nodes unique to specific expressions.
/// Usually utilized by builtins.
pub trait BuildAssociated<T: Node + ?Sized>: Debug {
    /// Build the associated node.
    fn build_associated(self: Box<Self>, build: &mut Build, associated: &mut HierarchyNode<T>);
}

/// Associated data used in constructing the associated node.
#[derive(Debug)]
pub enum AssociatedData {
    /// A boolean value
    Bool(MaybeUnset<bool>),
    /// A style (brush) value
    Style(MaybeUnset<Style>),
    /// A line type - line, ray or segment.
    LineType(MaybeUnset<LineType>),
}

impl AssociatedData {
    #[must_use]
    pub fn as_bool(&self) -> Option<MaybeUnset<bool>> {
        match self {
            Self::Bool(v) => Some(v.copied()),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_style(&self) -> Option<MaybeUnset<Style>> {
        match self {
            Self::Style(v) => Some(v.copied()),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_line_type(&self) -> Option<MaybeUnset<LineType>> {
        match self {
            Self::LineType(v) => Some(v.copied()),
            _ => None,
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
pub struct HierarchyNode<T: Node + ?Sized> {
    /// The root node.
    pub root: Box<T>,
    /// The child nodes.
    pub children: Vec<Box<dyn Node>>,
    /// The associated node.
    pub associated: Option<Box<dyn BuildAssociated<T>>>,
    /// Associated data for associated node construction.
    pub associated_data: HashMap<&'static str, AssociatedData>,
}

impl<T: Node + ?Sized> Node for HierarchyNode<T> {
    fn set_display(&mut self, display: bool) {
        self.root.set_display(display);
    }

    fn get_display(&self) -> bool {
        self.root.get_display()
    }

    fn build(mut self: Box<Self>, build: &mut Build) {
        if self.root.get_display() {
            if let Some(associated) = self.associated.take() {
                associated.build_associated(build, &mut self);
            }

            self.root.build(build);

            for child in self.children {
                child.build(build);
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
    /// Create a new hierarchy node from its root. No children initially.
    #[must_use]
    pub fn new(root: T) -> Self {
        Self {
            root: Box::new(root),
            children: Vec::new(),
            associated: None,
            associated_data: HashMap::new(),
        }
    }
}

impl HierarchyNode<dyn Node> {
    /// Create a new dynamically dispatched hierarchy node.
    #[must_use]
    pub fn new_dyn(root: impl Node + 'static) -> Self {
        Self {
            root: Box::new(root),
            children: Vec::new(),
            associated: None,
            associated_data: HashMap::new(),
        }
    }
}

impl<T: Node + ?Sized> HierarchyNode<T> {
    /// Push a child.
    pub fn push_child<U: Node + 'static>(&mut self, node: U) {
        self.children.push(Box::new(node));
    }

    /// Extend boxed children.
    pub fn extend_boxed<Iter: IntoIterator<Item = Box<dyn Node>>>(&mut self, nodes: Iter) {
        self.children.extend(nodes);
    }

    /// Extend children.
    pub fn extend_children<U: Node + 'static, Iter: IntoIterator<Item = U>>(
        &mut self,
        nodes: Iter,
    ) {
        self.children
            .extend(nodes.into_iter().map(|x| Box::new(x) as Box<dyn Node>));
    }

    /// Set the associated node.
    pub fn set_associated<U: BuildAssociated<T> + 'static>(&mut self, associated: U) {
        self.associated = Some(Box::new(associated));
    }

    /// Inset associated data with a specific key.
    pub fn insert_data<U: Into<AssociatedData>>(&mut self, key: &'static str, data: U) {
        self.associated_data.insert(key, data.into());
    }

    /// Get associated data.
    #[must_use]
    pub fn get_data(&self, key: &'static str) -> Option<&AssociatedData> {
        self.associated_data.get(key)
    }
}

/// Node for point collections
#[derive(Debug)]
pub struct PCNode {
    /// Whether to display the node
    pub display: MaybeUnset<bool>,
    /// The child nodes of the collection's points
    pub children: Vec<Option<HierarchyNode<<Point as Displayed>::Node>>>,
    /// Properties are stored for later processing as they might be used for conversions.
    pub props: Option<Properties>,
}

impl Default for PCNode {
    fn default() -> Self {
        Self::new()
    }
}

impl PCNode {
    /// Create a new, empty point collection node.
    #[must_use]
    pub fn new() -> Self {
        Self {
            display: MaybeUnset::new(true),
            children: Vec::new(),
            props: None,
        }
    }

    /// Push a child node.
    pub fn push(&mut self, node: Option<HierarchyNode<<Point as Displayed>::Node>>) {
        self.children.push(node);
    }

    /// Extend child nodes.
    pub fn extend<U: IntoIterator<Item = Option<HierarchyNode<<Point as Displayed>::Node>>>>(
        &mut self,
        nodes: U,
    ) {
        self.children.extend(nodes);
    }
}

impl Dummy for PCNode {
    fn dummy() -> Self {
        Self::new()
    }

    fn is_dummy(&self) -> bool {
        self.children.is_empty()
    }
}

impl Node for PCNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, build: &mut Build) {
        if self.display.unwrap() {
            for child in self.children.into_iter().flatten() {
                child.build_unboxed(build);
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

/// Type-erased node for all unrolled expression nodes.
#[derive(Debug)]
pub enum AnyExprNode {
    Point(HierarchyNode<<Point as Displayed>::Node>),
    Line(HierarchyNode<<Line as Displayed>::Node>),
    Circle(HierarchyNode<<Circle as Displayed>::Node>),
    Scalar(HierarchyNode<<Scalar as Displayed>::Node>),
    PointCollection(HierarchyNode<<PointCollection as Displayed>::Node>),
    Derived(HierarchyNode<<Derived as Displayed>::Node>),
    Unknown(HierarchyNode<<Unknown as Displayed>::Node>),
}

impl_from_for_any! {Point}
impl_from_for_any! {Line}
impl_from_for_any! {Circle}
impl_from_for_any! {Scalar}
impl_from_for_any! {PointCollection}
impl_from_for_any! {Derived}
impl_from_for_any! {Unknown}

impl AnyExprNode {
    /// Erase `self`'s type completely.
    #[must_use]
    pub fn to_dyn(self) -> Box<dyn Node> {
        match self {
            Self::Point(v) => Box::new(v),
            Self::Line(v) => Box::new(v),
            Self::Circle(v) => Box::new(v),
            Self::Scalar(v) => Box::new(v),
            Self::PointCollection(v) => Box::new(v),
            Self::Derived(v) => Box::new(v),
            Self::Unknown(v) => Box::new(v),
        }
    }

    /// # Panics
    /// If the node is not a point node.
    #[must_use]
    pub fn to_point(self) -> HierarchyNode<<Point as Displayed>::Node> {
        if let Self::Point(v) = self {
            v
        } else {
            panic!("not a point")
        }
    }

    /// # Panics
    /// If the node is not a line node.
    #[must_use]
    pub fn to_line(self) -> HierarchyNode<<Line as Displayed>::Node> {
        if let Self::Line(v) = self {
            v
        } else {
            panic!("not a line")
        }
    }

    /// # Panics
    /// If the node is not a circle node.
    #[must_use]
    pub fn to_circle(self) -> HierarchyNode<<Circle as Displayed>::Node> {
        if let Self::Circle(v) = self {
            v
        } else {
            panic!("not a circle")
        }
    }

    /// # Panics
    /// If the node is not a scalar node.
    #[must_use]
    pub fn to_scalar(self) -> HierarchyNode<<Scalar as Displayed>::Node> {
        if let Self::Scalar(v) = self {
            v
        } else {
            panic!("not a scalar")
        }
    }

    /// # Panics
    /// If the node is not a point collection node.
    #[must_use]
    pub fn to_point_collection(self) -> HierarchyNode<<PointCollection as Displayed>::Node> {
        if let Self::PointCollection(v) = self {
            v
        } else {
            panic!("not a point collection")
        }
    }

    /// # Panics
    /// If the node is not a bundle node.
    #[must_use]
    pub fn to_bundle(self) -> HierarchyNode<<Derived as Displayed>::Node> {
        if let Self::Derived(v) = self {
            v
        } else {
            panic!("not a derived type")
        }
    }

    /// # Panics
    /// If the node is not an unknown node.
    #[must_use]
    pub fn to_unknown(self) -> HierarchyNode<<Unknown as Displayed>::Node> {
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
            Self::Derived(v) => v.set_display(display),
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
            Self::Derived(v) => v.get_display(),
            Self::Unknown(v) => v.get_display(),
        }
    }

    fn build(self: Box<Self>, build: &mut Build) {
        match *self {
            Self::Point(v) => v.build_unboxed(build),
            Self::Line(v) => v.build_unboxed(build),
            Self::Circle(v) => v.build_unboxed(build),
            Self::Scalar(v) => v.build_unboxed(build),
            Self::PointCollection(v) => v.build_unboxed(build),
            Self::Derived(v) => v.build_unboxed(build),
            Self::Unknown(v) => v.build_unboxed(build),
        }
    }
}

/// An empty node that is never displayed.
#[derive(Debug)]
pub struct EmptyNode;

impl Dummy for EmptyNode {
    fn dummy() -> Self {
        Self
    }

    fn is_dummy(&self) -> bool {
        true
    }
}

impl Node for EmptyNode {
    fn set_display(&mut self, _display: bool) {}

    fn get_display(&self) -> bool {
        false
    }

    fn build(self: Box<Self>, _build: &mut Build) {}
}

/// A node for points
#[derive(Debug)]
pub struct PointNode {
    /// Whether to display the node
    pub display: MaybeUnset<bool>,
    /// The point's label
    pub label: MaybeUnset<MathString>,
    /// Whether to display the label
    pub display_label: MaybeUnset<bool>,
    /// Whether to display the point's dot.
    pub display_dot: MaybeUnset<bool>,
    /// Default label to use if `label` is empty.
    pub default_label: MathString,
    /// Defining expression
    pub expr: Expr<Point>,
}

impl Dummy for PointNode {
    fn dummy() -> Self {
        Self {
            display: MaybeUnset::new(true),
            label: MaybeUnset::new(MathString::new(span!(0, 0, 0, 0))),
            display_label: MaybeUnset::new(true),
            display_dot: MaybeUnset::new(true),
            default_label: MathString::new(span!(0, 0, 0, 0)),
            expr: Expr::dummy(),
        }
    }

    fn is_dummy(&self) -> bool {
        self.expr.is_dummy()
    }
}

impl Node for PointNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, build: &mut Build) {
        if self.display.unwrap() && !self.is_dummy() {
            let id = build.load(&self.expr);
            build.add(PointItem {
                id,
                label: if self.display_label.unwrap() {
                    if self.label.as_ref().is_empty() {
                        // println!("{} as {}", self.expr, self.default_label);
                        self.default_label
                    } else {
                        self.label.unwrap()
                    }
                } else {
                    MathString::new(span!(0, 0, 0, 0))
                }
                .string,
                display_dot: self.display_dot.unwrap(),
            });
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
                .ok_or(MathString::new(span!(0, 0, 0, 0))),
            expr: expr.clone_without_node(),
        };

        props.finish(context);

        node
    }
}

/// Node for a circle
#[derive(Debug)]
pub struct CircleNode {
    /// Whether to display the node
    pub display: MaybeUnset<bool>,
    /// The circle's label
    pub label: MaybeUnset<MathString>,
    /// Whether to display the label
    pub display_label: MaybeUnset<bool>,
    /// Default label to use if `label` is empty
    pub default_label: MathString,
    /// How to draw the circle (brush)
    pub style: MaybeUnset<Style>,
    /// The defining expression
    pub expr: Expr<Circle>,
}

impl Dummy for CircleNode {
    fn dummy() -> Self {
        Self {
            display: MaybeUnset::new(true),
            label: MaybeUnset::new(MathString::new(span!(0, 0, 0, 0))),
            display_label: MaybeUnset::new(true),
            default_label: MathString::new(span!(0, 0, 0, 0)),
            style: MaybeUnset::new(Style::default()),
            expr: Expr::dummy(),
        }
    }

    fn is_dummy(&self) -> bool {
        self.expr.is_dummy()
    }
}

impl Node for CircleNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, build: &mut Build) {
        if self.display.unwrap() && !self.is_dummy() {
            let id = build.load(&self.expr);
            build.add(CircleItem {
                id,
                label: if self.display_label.unwrap() {
                    if self.label.as_ref().is_empty() {
                        // println!("{} as {}", self.expr, self.default_label);
                        self.default_label
                    } else {
                        self.label.unwrap()
                    }
                } else {
                    MathString::new(span!(0, 0, 0, 0))
                }
                .string,
                style: self.style.unwrap(),
            });
        }
    }
}

impl FromExpr<Circle> for CircleNode {
    fn from_expr(expr: &Expr<Circle>, mut props: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props
                .get("label")
                .maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(false),
            default_label: props
                .get("default-label")
                .ok_or(MathString::new(span!(0, 0, 0, 0))),
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

impl Default for LineType {
    fn default() -> Self {
        Self::Line
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

/// Node for a line
#[derive(Debug)]
pub struct LineNode {
    /// Whether to display the node
    pub display: MaybeUnset<bool>,
    /// The line's label
    pub label: MaybeUnset<MathString>,
    /// Whether to display the label
    pub display_label: MaybeUnset<bool>,
    /// Default label to use if `label` is empty.
    pub default_label: MathString,
    /// The type of this line
    pub line_type: MaybeUnset<LineType>,
    /// How to draw the line (brush)
    pub style: MaybeUnset<Style>,
    /// Defining expression
    pub expr: Expr<Line>,
}

impl Dummy for LineNode {
    fn dummy() -> Self {
        Self {
            display: MaybeUnset::new(true),
            label: MaybeUnset::new(MathString::new(span!(0, 0, 0, 0))),
            display_label: MaybeUnset::new(true),
            default_label: MathString::new(span!(0, 0, 0, 0)),
            line_type: MaybeUnset::new(LineType::Line),
            style: MaybeUnset::new(Style::default()),
            expr: Expr::dummy(),
        }
    }

    fn is_dummy(&self) -> bool {
        self.expr.is_dummy()
    }
}

impl Node for LineNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, build: &mut Build) {
        if self.display.unwrap() && !self.is_dummy() {
            let label = if self.display_label.unwrap() {
                let label = self.label.unwrap();

                if label.is_empty() {
                    self.default_label
                } else {
                    label
                }
            } else {
                MathString::new(span!(0, 0, 0, 0))
            };
            let style = self.style.unwrap();

            match self.line_type.unwrap() {
                LineType::Line => {
                    let id = build.load(&self.expr);
                    build.add(LineItem {
                        id,
                        label: label.string,
                        style,
                    });
                }
                LineType::Ray => match &self.expr.data.as_ref() {
                    Line::LineFromPoints(a, b) => {
                        let p_id = build.load(a);
                        let q_id = build.load(b);
                        build.add(RayItem {
                            p_id,
                            q_id,
                            label: label.string,
                            style,
                        });
                    }
                    Line::AngleBisector(a, b, c) => {
                        let x = Expr::new_spanless(Point::LineLineIntersection(
                            Expr::new_spanless(Line::LineFromPoints(
                                a.clone_without_node(),
                                c.clone_without_node(),
                            )),
                            self.expr.clone_without_node(),
                        ));

                        let p_id = build.load(b);
                        let q_id = build.load(&x);
                        build.add(RayItem {
                            p_id,
                            q_id,
                            label: label.string,
                            style,
                        });
                    }
                    _ => unreachable!(),
                },
                LineType::Segment => match &self.expr.data.as_ref() {
                    Line::LineFromPoints(a, b) => {
                        let p_id = build.load(a);
                        let q_id = build.load(b);
                        build.add(SegmentItem {
                            p_id,
                            q_id,
                            label: label.string,
                            style,
                        });
                    }
                    _ => unreachable!(),
                },
            }
        }
    }
}

impl FromExpr<Line> for LineNode {
    fn from_expr(expr: &Expr<Line>, mut props: Properties, context: &CompileContext) -> Self {
        let node = Self {
            display: props.get("display").maybe_unset(true),
            label: props
                .get("label")
                .maybe_unset(MathString::new(span!(0, 0, 0, 0))),
            display_label: props.get("display_label").maybe_unset(false),
            default_label: props
                .get("default-label")
                .ok_or(MathString::new(span!(0, 0, 0, 0))),
            line_type: MaybeUnset::new(LineType::Line),
            style: props.get("style").maybe_unset(Style::default()),
            expr: expr.clone_without_node(),
        };

        props.finish(context);

        node
    }
}

/// A node for a scalar.
#[derive(Debug)]
pub struct ScalarNode {
    /// Whether to display the node
    pub display: MaybeUnset<bool>,
    /// Defining expression
    pub expr: Expr<Scalar>,
}

impl Dummy for ScalarNode {
    fn dummy() -> Self {
        Self {
            display: MaybeUnset::new(true),
            expr: Expr::dummy(),
        }
    }

    fn is_dummy(&self) -> bool {
        self.expr.is_dummy()
    }
}

impl Node for ScalarNode {
    fn set_display(&mut self, display: bool) {
        self.display.set(display);
    }

    fn get_display(&self) -> bool {
        self.display.get_copied()
    }

    fn build(self: Box<Self>, _build: &mut Build) {}
}

impl FromExpr<Scalar> for ScalarNode {
    fn from_expr(expr: &Expr<Scalar>, mut props: Properties, context: &CompileContext) -> Self {
        props.ignore("label");
        props.ignore("display_label");
        props.ignore("default-label");

        let node = Self {
            display: props.get("display").maybe_unset(true),
            expr: expr.clone_without_node(),
        };

        props.finish(context);

        node
    }
}
