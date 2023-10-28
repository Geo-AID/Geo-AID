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

use crate::script::{figure::MathString, Error, token::{PointCollectionItem, NamedIdent}};

use super::{Expr, Point as UnrolledPoint, Properties};

#[derive(Debug, Clone)]
pub enum IdentOrItem {
    Ident(NamedIdent),
    PointCollectionItem(PointCollectionItem)
}

/// A figure is a tree of nodes, where each node represents a drawable (or empty) object.
#[derive(Debug, Clone)]
pub struct Node {
    pub children: Vec<Node>,
    pub object: Object,
    /// Decides whether this node and its child nodes are displayed.
    pub display: bool
}

impl Default for Node {
    fn default() -> Self {
        Self {
            children: Vec::new(),
            object: Object::Empty,
            display: true
        }
    }
}

impl Node {
    pub fn new_point(expr: Expr<UnrolledPoint>, mut props: Properties, default_label: IdentOrItem) -> Result<Self, Error> {
        // Get the value of `display`
        let display = props.get_bool("display")?.get_or(true);

        let mut display_label;
        let mut label;

        // Check if there is a label.
        if let Some(prop) = props.get_string("label")?.get_prop() {
            // If so, make the label displayed, unless otherwise instructed.
            display_label = props.get_bool("display_label")?.get_or(true);
            label = MathString::parse(&prop.value, prop.span)?;
        } else {
            // If not, parse the ident as one and see if it should be displayed.
            display_label = matches!(default_label, IdentOrItem::PointCollectionItem(_));

            label = match default_label {
                IdentOrItem::Ident(ident) => MathString::parse(&ident.ident, ident.span)?,
                IdentOrItem::PointCollectionItem(item) => MathString::from(item)
            };

            if !display_label {
                if let Some(x) = label.displayed_by_default() {
                    display_label = true;
                    label = x;
                }
            };
        }

        let display_dot = props.get_bool("display_dot")?.get_or(true);

        Ok(Self {
            display,
            object: Object::Point(Point {
                display_label,
                label,
                display_dot,
                expr
            }),
            ..Default::default()
        })
    }
}

/// An object is really anything the drawers can draw.
#[derive(Debug, Clone, Default)]
pub enum Object {
    #[default]
    Empty,
    Point(Point)
}

/// A point has a displayable label.
#[derive(Debug, Clone)]
pub struct Point {
    pub display_label: bool,
    pub display_dot: bool,
    pub label: MathString,
    pub expr: Expr<UnrolledPoint>
}