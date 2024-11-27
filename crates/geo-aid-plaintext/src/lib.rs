//! Geo-AID is capable of outputting its figures in plaintext, human-readable format.
//! This capability is currently very limited and largely untested, especially UX wise.

use std::io::{self, Seek, Write};

use geo_aid_figure::{
    CircleItem, Figure, Item, Label, LineItem, PointItem, Position, Style, TwoPointItem,
};

/// The raw format writer
#[derive(Debug)]
pub struct Plaintext<W: Write + Seek> {
    /// The write stream
    writer: W,
}

impl<W: Write + Seek> Plaintext<W> {
    /// Get the figure in plaintext format.
    pub fn draw(figure: &Figure, writer: W) -> io::Result<()> {
        let mut plain = Self { writer };

        for item in &figure.items {
            match item {
                Item::Point(point) => plain.draw_point(point)?,
                Item::Line(line) => plain.draw_line(line)?,
                Item::Ray(ray) => plain.draw_ray(ray)?,
                Item::Segment(segment) => plain.draw_segment(segment)?,
                Item::Circle(circle) => plain.draw_circle(circle)?,
            }
        }

        Ok(())
    }

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
    ) -> io::Result<()> {
        let label = label.map(|l| l.content.to_string()).unwrap_or_default();

        writeln!(
            &mut self.writer,
            "{} line \"{}\" from ({:.3}, {:.3}) to ({:.3}, {:.3})",
            Self::get_style_name(style),
            label,
            p1.x,
            p1.y,
            p2.x,
            p2.y
        )
    }

    fn draw_point(&mut self, point: &PointItem) -> io::Result<()> {
        let label = point
            .label
            .as_ref()
            .map(|x| format!("\"{}\"", x.content))
            .unwrap_or_default();

        writeln!(
            &mut self.writer,
            "point \"{label}\" at ({:.3}, {:.3})",
            point.position.x, point.position.y
        )
    }

    fn draw_line(&mut self, line: &LineItem) -> io::Result<()> {
        self.draw_simple_segment(line.points, line.style, line.label.as_ref())
    }

    fn draw_ray(&mut self, ray: &TwoPointItem) -> io::Result<()> {
        self.draw_simple_segment(ray.points, ray.style, ray.label.as_ref())
    }

    fn draw_segment(&mut self, segment: &TwoPointItem) -> io::Result<()> {
        self.draw_simple_segment(segment.points, segment.style, segment.label.as_ref())
    }

    fn draw_circle(&mut self, circle: &CircleItem) -> io::Result<()> {
        writeln!(
            &mut self.writer,
            "{} circle at ({:.3}, {:.3}) with radius {:.3}",
            Self::get_style_name(circle.style),
            circle.center.x,
            circle.center.y,
            circle.radius,
        )
    }
}
