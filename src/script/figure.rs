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
    /// Angles to be displayed
    pub angles: Vec<(Arc<Expression>, u8)>, // This u8 refers to number of arcs in an angle!
    /// The canvas size.
    pub canvas_size: (usize, usize),
}
