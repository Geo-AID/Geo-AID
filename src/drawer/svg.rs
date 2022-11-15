use svg::{Document, node::{element::{Circle, Line, Text}}};

use crate::{projector::Rendered, generator::Complex};

pub fn draw(target: String, canvas_size: (usize, usize), rendered: Vec<Rendered>) {
    let mut  document = Document::new()
        .set("width", canvas_size.0)
        .set("height", canvas_size.1);

    for elem in &rendered {
        match elem {
            Rendered::Point(pt) => {
                let p = Complex::new(pt.position.real, canvas_size.1 as f64 - pt.position.imaginary);

                let circle = Circle::new()
                    .set("cx", p.real)
                    .set("cy", p.imaginary)
                    .set("r", 2)
                    .set("fill", "black");

                let label = Text::new()
                    .set("x", p.real - 15.0)
                    .set("y", p.imaginary - 15.0)
                    .set("content", pt.label.as_str())
                    .set("stroke", "black");

                document = document.add(circle).add(label);
            },
            Rendered::Line(ln) => {
                let p1 = Complex::new(ln.points.0.real, canvas_size.1 as f64 - ln.points.0.imaginary);
                let p2 = Complex::new(ln.points.1.real, canvas_size.1 as f64 - ln.points.1.imaginary);

                let line = Line::new()
                    .set("x1", p1.real)
                    .set("y1", p1.imaginary)
                    .set("x2", p2.real)
                    .set("y2", p2.imaginary)
                    .set("stroke", "black");

                document = document.add(line);
            },
        }
    }

    svg::save(target, &document).unwrap();
}