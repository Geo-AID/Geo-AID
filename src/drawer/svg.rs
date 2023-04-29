use std::{fs::File, io::Write, path::Path};

use crate::{
    generator::{geometry, Complex},
    projector::{Output, Rendered},
};

/// Draws the given figure as .svg format.
///
/// # Panics
/// Panics when there are issues with writing to file.
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    let mut content = String::new();
    content += &format!(
        "<svg height=\"{}\" width=\"{}\" xmlns=\"http://www.w3.org/2000/svg\">",
        canvas_size.0, canvas_size.1
    );

    for elem in &output.vec_rendered {
        match elem {
            Rendered::Point(pt) => {
                #[allow(clippy::cast_precision_loss)]
                let p = Complex::new(pt.position.real, pt.position.imaginary);
                let real = if p.real - 20.0 < 0.0 {
                    p.real + 20.0
                } else {
                    p.real - 20.0
                };

                let imaginary = if p.imaginary - 20.0 < 0.0 {
                    p.imaginary + 20.0
                } else {
                    p.imaginary - 20.0
                };
                content += &format!(
                    "<circle cx=\"{}\" cy=\"{}\" fill=\"black\" r=\"2\"/>
                <text stroke=\"black\" x=\"{real}\" y=\"{imaginary}\">{}</text>",
                    p.real, p.imaginary, pt.label
                );
            }

            Rendered::Line(ln) => {
                #[allow(clippy::cast_precision_loss)]
                let p1 = Complex::new(ln.points.0.real, ln.points.0.imaginary);
                #[allow(clippy::cast_precision_loss)]
                let p2 = Complex::new(ln.points.1.real, ln.points.1.imaginary);
                content += &format!(
                    "<line stroke=\"black\" x1=\"{}\" x2=\"{}\" y1=\"{}\" y2=\"{}\"/>",
                    p1.real, p2.real, p1.imaginary, p2.imaginary
                );
            }
            Rendered::Angle(angle) => {
                let x: u32 = 5;
                content += &format!(
                    r#"
                <g transform="translate({}, {}) rotate({}, 0, 0)" fill="transparent">
                    <path d="M {}, 0 A 45, 45, 0, 0, 0, {}, {}" stroke="black" stroke-width="0.4" /> 
                </g>"#,
                    angle.points.1.real,
                    angle.points.1.imaginary,
                    geometry::get_line(angle.points.1, angle.points.0)
                        .direction
                        .arg()
                        .to_degrees(),
                    9 * x,
                    angle.angle_value.cos() * 45.0,
                    -angle.angle_value.sin() * 45.0
                );
            }
        }
    }
    content += "</svg>";

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
