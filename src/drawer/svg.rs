use std::{fs::File, io::Write, path::Path};

use crate::{generator::Complex, projector::Rendered};

/// Draws the given figure as .svg format.
///
/// # Panics
/// Panics when there are issues with writing to file.
pub fn draw(target: &Path, canvas_size: (usize, usize), rendered: &Vec<Rendered>) {
    let mut content = String::new();
    content += &format!(
        "<svg height=\"{}\" width=\"{}\" xmlns=\"http://www.w3.org/2000/svg\">",
        canvas_size.0, canvas_size.1
    );

    for elem in rendered {
        match elem {
            Rendered::Point(pt) => {
                #[allow(clippy::cast_precision_loss)]
                let p = Complex::new(
                    pt.position.real,
                    canvas_size.1 as f64 - pt.position.imaginary,
                );
                content += &format!(
                    "<circle cx=\"{}\" cy=\"{}\" fill=\"black\" r=\"2\"/>
                <text stroke=\"black\" x=\"{}\" y=\"{}\">{}</text>",
                    p.real,
                    p.imaginary,
                    p.real - 20.0,
                    p.imaginary - 20.0,
                    pt.label
                );
            }

            Rendered::Line(ln) => {
                #[allow(clippy::cast_precision_loss)]
                let p1 = Complex::new(
                    ln.points.0.real,
                    canvas_size.1 as f64 - ln.points.0.imaginary,
                );
                #[allow(clippy::cast_precision_loss)]
                let p2 = Complex::new(
                    ln.points.1.real,
                    canvas_size.1 as f64 - ln.points.1.imaginary,
                );
                content += &format!(
                    "<line stroke=\"black\" x1=\"{}\" x2=\"{}\" y1=\"{}\" y2=\"{}\"/>",
                    p1.real, p2.real, p1.imaginary, p2.imaginary
                );
            }
            Rendered::Angle(..) => {
                
            }
        }
    }
    content += "</svg>";

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
