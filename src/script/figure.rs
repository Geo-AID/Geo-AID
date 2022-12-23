#[derive(Debug)]
pub struct Figure {
    pub points: Vec<Point>,
    pub lines: Vec<Line>,
    pub segments: Vec<Segment>,
    pub canvas_size: (usize, usize),
}

pub enum Construct {
    Point(PointDefinition),
    Line(Line),
    Segment(Segment),
}

impl Construct {
    /// Returns `true` if the construct is [`Point`].
    ///
    /// [`Point`]: Construct::Point
    #[must_use]
    pub fn is_point(&self) -> bool {
        matches!(self, Self::Point(..))
    }

    #[must_use]
    pub fn as_point(&self) -> Option<&PointDefinition> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Point {
    pub label: String,
    pub definition: PointDefinition,
}

#[derive(Debug)]
pub enum PointDefinition {
    Indexed(usize),
    Crossing(LineDefinition, LineDefinition),
}

#[derive(Debug)]
pub enum LineDefinition {
    /// Indices pointing to geometrical constructs creating that line
    TwoPoints(Box<PointDefinition>, Box<PointDefinition>),
}

#[derive(Debug)]
pub struct Line {
    pub label: String,
    pub definition: LineDefinition,
}

#[derive(Debug)]
pub struct Segment {
    pub label: String,
    pub points: (usize, usize),
}
