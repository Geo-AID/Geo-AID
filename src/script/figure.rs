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

use std::sync::Arc;

use serde::Serialize;

use crate::generator::expression::{CircleExpr, Expression, LineExpr, PointExpr, ScalarExpr};

use super::unroll::PointMeta;

type Point = Arc<Expression<PointExpr>>;

/// The display mode of the expression.
#[derive(Debug, Clone, Serialize, Copy)]
pub enum Mode {
    Dotted,
    Dashed,
    Bolded,
    Default, // Normal solid curve
}

/// Defines the visual data of the figure.
#[derive(Debug, Default)]
pub struct Figure {
    /// Points to be displayed
    pub points: Vec<(Arc<Expression<PointExpr>>, PointMeta)>,
    /// Lines to be displayed
    pub lines: Vec<(Arc<Expression<LineExpr>>, Mode)>,
    /// Angles to be displayed
    pub angles: Vec<(Arc<Expression<ScalarExpr>>, u8, Mode)>, // This u8 refers to number of arcs in an angle!
    /// Segments to be displayed
    pub segments: Vec<(Point, Point, Mode)>,
    /// Rays to be displayed
    pub rays: Vec<(Point, Point, Mode)>,
    /// Circles to be displayed
    pub circles: Vec<(Arc<Expression<CircleExpr>>, Mode)>,
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
