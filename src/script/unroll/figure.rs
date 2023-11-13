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

use std::{ops::Deref, fmt::Debug};

use crate::{script::{figure::{MathString, Figure, Mode}, token::{PointCollectionItem, NamedIdent}, compile::{Compiler, Compile}}, span};

use super::{Expr, Point, Circle};

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