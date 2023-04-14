use std::sync::Arc;

use super::{unroll::PointMeta, Expression, Weighed};

/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<(Arc<Weighed<Expression>>, PointMeta)>,
    /// The lines to be displayed
    pub lines: Vec<Arc<Weighed<Expression>>>,
    /// Angles to be displayed
    pub angles: Vec<(Arc<Weighed<Expression>>, u8)>, // This u8 refers to number of arcs in an angle!
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
