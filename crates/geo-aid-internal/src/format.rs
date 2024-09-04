use geo_aid_figure::{CircleItem, Figure, Item, LineItem, PointItem, TwoPointItem};
use std::{fs::File, io::Write, path::Path};

pub mod json;
pub mod latex;
pub mod raw;
pub mod svg;

pub trait Draw {
    /// # Errors
    /// In case of io failure.
    fn draw(&mut self, target: &Path, figure: &Figure) -> Result<(), std::io::Error> {
        self.begin(figure);

        for item in &figure.items {
            match item {
                Item::Point(point) => self.draw_point(point),
                Item::Line(line) => self.draw_line(line),
                // Rendered::Angle(angle) => self.draw_angle(angle),
                Item::Segment(segment) => self.draw_segment(segment),
                Item::Ray(ray) => self.draw_ray(ray),
                Item::Circle(circle) => self.draw_circle(circle),
            }
        }

        let content = self.end();

        let file = File::create(target);

        match file {
            Ok(mut file) => file.write_all(content.as_bytes()),
            Err(error) => Err(error),
        }
    }

    fn begin(&mut self, _output: &Figure) {}

    fn draw_point(&mut self, _point: &PointItem) {
        unimplemented!("support for point rendering not implemented")
    }

    fn draw_line(&mut self, _line: &LineItem) {
        unimplemented!("support for line rendering not implemented")
    }

    fn draw_ray(&mut self, _ray: &TwoPointItem) {
        unimplemented!("support for ray rendering not implemented")
    }

    fn draw_segment(&mut self, _segment: &TwoPointItem) {
        unimplemented!("support for segment rendering not implemented")
    }

    // fn draw_angle(&mut self, _angle: &RenderedAngle) {
    //     unimplemented!("support for angle rendering not implemented")
    // }

    fn draw_circle(&mut self, _circle: &CircleItem) {
        unimplemented!("support for circle rendering not implemented")
    }

    fn end(&mut self) -> &str;
}
