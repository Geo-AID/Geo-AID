use std::sync::Arc;

use super::{unroll::PointMeta, ExprKind, Weighed};

/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<(Arc<Weighed<ExprKind>>, PointMeta)>,
    /// The lines to be displayed
    pub lines: Vec<Arc<Weighed<ExprKind>>>,
    /// Angles to be displayed
    pub angles: Vec<(Arc<Weighed<Expression>>, u8)>, // This u8 refers to number of arcs in an angle!
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
