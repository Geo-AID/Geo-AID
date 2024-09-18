//! Geo-AID is capable of outputting its figures in plaintext, human-readable format.
//! This capability is currently very limited and largely untested, especially UX wise.

use geo_aid_figure::{
    CircleItem, Figure, Item, Label, LineItem, PointItem, Position, Style, TwoPointItem,
};

/// The raw format writer
#[derive(Debug, Default)]
pub struct Plaintext {
    /// The current file contents
    content: String,
}

impl Plaintext {
    /// Get the figure in plaintext format.
    #[must_use]
    pub fn draw(figure: &Figure) -> String {
        let mut plain = Self::default();

        for item in &figure.items {
            match item {
                Item::Point(point) => plain.draw_point(point),
                Item::Line(line) => plain.draw_line(line),
                Item::Ray(ray) => plain.draw_ray(ray),
                Item::Segment(segment) => plain.draw_segment(segment),
                Item::Circle(circle) => plain.draw_circle(circle),
            }
        }

        plain.content
    }
}

impl Plaintext {
    /// Get the human readable name of the requested [`Style`]
    fn get_style_name(style: Style) -> &'static str {
        match style {
            Style::Dotted => "dotted",
            Style::Dashed => "dashed",
            Style::Bold => "bold",
            Style::Solid => "solid",
        }
    }

    /// Draw a styled segment delimited by two points.
    fn draw_simple_segment(
        &mut self,
        (p1, p2): (Position, Position),
        style: Style,
        label: Option<&Label>,
    ) {
        let label = label.map(|l| l.content.to_string()).unwrap_or_default();

        self.content += &format!(
            "{} line \"{}\" from ({:.3}, {:.3}) to ({:.3}, {:.3})\n",
            Self::get_style_name(style),
            label,
            p1.x,
            p1.y,
            p2.x,
            p2.y
        );
    }

    fn draw_point(&mut self, point: &PointItem) {
        let label = point
            .label
            .as_ref()
            .map(|x| format!("\"{}\"", x.content))
            .unwrap_or_default();

        self.content += &format!(
            "point \"{label}\" at ({:.3}, {:.3})\n",
            point.position.x, point.position.y
        );
    }

    fn draw_line(&mut self, line: &LineItem) {
        self.draw_simple_segment(line.points, line.style, line.label.as_ref());
    }

    fn draw_ray(&mut self, ray: &TwoPointItem) {
        self.draw_simple_segment(ray.points, ray.style, ray.label.as_ref());
    }

    fn draw_segment(&mut self, segment: &TwoPointItem) {
        self.draw_simple_segment(segment.points, segment.style, segment.label.as_ref());
    }

    // fn draw_angle(&mut self, angle: &RenderedAngle, output: &Output) {
    //     let p_1 = angle.points.0;
    //     let p_origin = angle.points.1;
    //     let p_2 = angle.points.2;
    //     let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
    //     match &angle.expr.kind {
    //         ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
    //             self.content += &format!("\n3 points angle: points' coordinates point1 - {}, origin - {}, point2 - {}, number of arcs: {no_arcs}, mode: {} \n",
    //                 get_point_name(arm1, output, p_1), get_point_name(origin, output, p_origin), get_point_name(arm2, output, p_2), styling(angle.style)
    //             );
    //         }
    //         ScalarExpr::AngleLine(_) => {
    //             self.content += &format!("\n2 lines angle: points' coordinates: point1 - ({}, {}), origin - ({}, {}), point2 - ({}, {}), number of arcs: {no_arcs}, mode: {} \n",
    //                 p_1.real, p_1. imaginary, p_origin.real, p_origin.imaginary, p_2.real, p_2.imaginary, styling(angle.style)
    //             );
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    fn draw_circle(&mut self, circle: &CircleItem) {
        self.content += &format!(
            "{} circle at ({:.3}, {:.3}) with radius {:.3}\n",
            Self::get_style_name(circle.style),
            circle.center.x,
            circle.center.y,
            circle.radius,
        );
    }
}
