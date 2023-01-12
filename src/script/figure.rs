use std::sync::Arc;

use super::{unroll::PointMeta, Expression, Weighed};

/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<(Arc<Weighed<Expression>>, PointMeta)>,
    /// The lines to be displayed
    pub lines: Vec<Arc<Weighed<Expression>>>,
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
