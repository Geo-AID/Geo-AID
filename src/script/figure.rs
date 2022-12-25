/// Defines the visual data of the figure.
#[derive(Debug)]
pub struct Figure {
    /// The points to be displayed
    pub points: Vec<Point>,
    /// The lines to be displayed
    pub lines: Vec<Line>,
    /// The segments to be displayed.
    pub segments: Vec<Segment>,
    /// The canvas size.
    pub canvas_size: (usize, usize),
}

/// Defines visual info about points.
#[derive(Debug)]
pub struct Point {
    /// The text to display next to the point.
    pub label: String,
    /// The definition allows the projector to find the point's position.
    pub definition: PointDefinition,
}

/// How the point is defined
#[derive(Debug)]
pub enum PointDefinition {
    /// For free points, that are adjusted by the generator.
    Indexed(usize),
    /// An intersection of two lines.
    Crossing(LineDefinition, LineDefinition),
}

/// How the line is defined.
#[derive(Debug)]
pub enum LineDefinition {
    /// Indices pointing to geometrical constructs creating that line
    TwoPoints(Box<PointDefinition>, Box<PointDefinition>),
}

/// Defines visual info about lines.
#[derive(Debug)]
pub struct Line {
    /// The text to display next to the line.
    pub label: String,
    /// The definition allows the projector to find where the line should be drawn.
    pub definition: LineDefinition,
}

/// Defines visual info about segments.
#[derive(Debug)]
pub struct Segment {
    /// The text to display next to the segment
    pub label: String,
    /// The beginning and the end of the segment.
    pub points: (usize, usize),
}
