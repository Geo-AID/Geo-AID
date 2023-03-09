use std::sync::Arc;

use super::{unroll::PointMeta, ExprKind, Weighed};

/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<(Arc<Weighed<ExprKind>>, PointMeta)>,
    /// The lines to be displayed
    pub lines: Vec<Arc<Weighed<ExprKind>>>,
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
