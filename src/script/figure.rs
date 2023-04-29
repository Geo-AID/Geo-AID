use std::sync::Arc;

use crate::generator::expression::{Expression, LineExpr, PointExpr, ScalarExpr};

use super::unroll::PointMeta;

type Point = Arc<Expression<PointExpr>>;

/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<(Arc<Expression<PointExpr>>, PointMeta)>,
    /// The lines to be displayed
    pub lines: Vec<Arc<Expression<LineExpr>>>,
    /// Angles to be displayed
    pub angles: Vec<(Arc<Expression<ScalarExpr>>, u8)>, // This u8 refers to number of arcs in an angle!
    /// Segments to be displayed
    pub segments: Vec<(Point, Point)>,
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
