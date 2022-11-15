pub struct Figure {
    pub points: Vec<Point>,
    pub lines: Vec<Line>,
    pub segments: Vec<Segment>,
    pub canvas_size: (usize, usize)
}

pub enum Construct {
    Point(PointDefinition),
    Line(Line),
    Segment(Segment)
}

impl Construct {
    /// Returns `true` if the construct is [`Point`].
    ///
    /// [`Point`]: Construct::Point
    #[must_use]
    pub fn is_point(&self) -> bool {
        matches!(self, Self::Point(..))
    }

    pub fn as_point(&self) -> Option<&PointDefinition> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub struct Point {
    pub label: String,
    pub definition: PointDefinition
}

pub enum PointDefinition {
    Indexed(usize),
    Crossing(LineDefinition, LineDefinition)
}

pub enum LineDefinition {
    /// Indices pointing to geometrical constructs creating that line
    TwoPoints(usize, usize)
}

pub struct Line {
    pub label: String,
    pub definition: LineDefinition
}

pub struct Segment {
    pub label: String,
    pub points: (usize, usize)
}