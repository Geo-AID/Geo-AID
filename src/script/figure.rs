use std::sync::Arc;

use crate::generator::expression::Expression;

use super::{unroll::PointMeta};

/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<(Arc<Expression>, PointMeta)>,
    /// The lines to be displayed
    pub lines: Vec<Arc<Expression>>,
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
