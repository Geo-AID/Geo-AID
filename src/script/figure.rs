use super::Expression;

/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<Expression>,
    /// The lines to be displayed
    pub lines: Vec<Expression>,
    /// The canvas size.
    pub canvas_size: (usize, usize),
}


