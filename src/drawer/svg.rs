use std::{fs::File, io::Write, path::Path};

use crate::{
    generator::Complex,
    projector::{Output, Rendered},
    script::Expression::{AngleLine, AnglePoint},
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
                let real;
                let imaginary;
                if p.real - 20.0 < 0.0 {
                    real = p.real + 10.0;
                } else {
                    real = p.real - 10.0;
                };
                if p.imaginary - 20.0 < 0.0 {
                    imaginary = p.imaginary + 10.0;
                } else {
                    imaginary = p.imaginary - 10.0;
                };
                content += &format!(
                    "<circle cx=\"{}\" cy=\"{}\" fill=\"black\" r=\"2\"/>
                <text stroke=\"black\" x=\"{}\" y=\"{}\">{}</text>",
                    p.real, p.imaginary, real, imaginary, pt.label
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
                let _no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
                match &angle.expr.object {
                    AnglePoint(_p1, _p2, _p3) => {
                        let x: u32 = 5;
                        content += &format!(
                            r#"
                        <g transform="translate({}, {}) rotate({}, 0, 0)" fill="transparent" stroke="black" stroke-width="0.1">
                            <path d="M {}, 0 A 45, 45, 0, 0, 0, {}, {}" /> 
                        </g>"#,
                            angle.points.1.real,
                            angle.points.1.imaginary,
                            (crate::generator::geometry::get_line(angle.points.0, angle.points.1)
                                .real
                                .atan())
                            .to_degrees(),
                            9 * x,
                            angle.angle_value.cos() * 45.0,
                            -angle.angle_value.sin() * 45.0
                        );
                    }
                    AngleLine(_ln1, _ln2) => {
                        let x: u32 = 5;
                        content += &format!(
                            r#"
                        <g transform="translate({}, {}) rotate({}, 0, 0)" fill="transparent" stroke="black" stroke-width="0.1">
                            <path d="M {}, 0 A 45, 45, 0, 0, 0, {}, {}" /> 
                        </g>"#,
                            angle.points.1.real,
                            angle.points.1.imaginary,
                            (crate::generator::geometry::get_line(angle.points.0, angle.points.1)
                                .real
                                .atan())
                            .to_degrees(),
                            9 * x,
                            angle.angle_value.cos() * 45.0,
                            -angle.angle_value.sin() * 45.0
                        );
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
    content += "</svg>";

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
