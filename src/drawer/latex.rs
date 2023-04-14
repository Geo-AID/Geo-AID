use std::string::String;
use std::sync::Arc;
use std::{fs::File, io::Write, path::Path};

use crate::generator::Complex;
use crate::projector::{Output, Rendered};
use crate::script::Expression::{self, AngleLine, AnglePoint};
use crate::script::{HashableArc, Weighed};

/// Function getting the point's name (if it exists, if not then it returns the point's coordinates)
fn get_point_name(
    expr: &Arc<Weighed<Expression>>,
    output: &Output,
    point: Complex,
    scale: f64,
) -> String {
    match output.map.get(&HashableArc::new(Arc::clone(expr))) {
        Some(p) => {
            format!("q{}", p.uuid)
        }
        None => {
            format!("({}, {})", (point.real * scale), point.imaginary * scale)
        }
    }
}
/// Draws the given figure to a .tex file using tikz library.
///
/// # Panics
/// Panics whenever there is a filesystem related problem.
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    // We must allow losing precision here.
    #[allow(clippy::cast_precision_loss)]
    let scale = f64::min(20.0 / canvas_size.0 as f64, 20.0 / canvas_size.1 as f64);
    let mut content = String::from(
        r#"
    \documentclass{article}
    \usepackage{tikz}
    \usepackage{tkz-euclide}
    \usetikzlibrary {angles,calc,quotes}
    \begin{document}
    \begin{tikzpicture}
    "#,
    );
    for item in &output.vec_rendered {
        match item {
            Rendered::Point(point) => {
                let position = point.position * scale;
                content+=&format!(
                    "\\coordinate [label=left:${}$] ({}) at ({}, {}); \\fill[black] ({}) circle (1pt);",
                    point.label, point.uuid, position.real,
                    position.imaginary, point.uuid
                );
            }
            Rendered::Line(line) => {
                let pos1 = line.points.0 * scale;
                let pos2 = line.points.1 * scale;
                content += &format!(
                    "\\draw ({},{}) -- ({},{});",
                    pos1.real, pos1.imaginary, pos2.real, pos2.imaginary
                );
            }
            Rendered::Angle(angle) => {
                let p1 = angle.points.0;
                let origin = angle.points.1;
                let p2 = angle.points.2;
                let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
                match &angle.expr.object {
                    AnglePoint(point1, point2, point3) => {
                        content += &format!(
                            r#"
                            \begin{{scope}}
                            \coordinate (A) at {};
                            \coordinate (B) at {};
                            \coordinate (C) at {};
                            \tkzMarkAngle[size = 0.5,mark = none,arc={no_arcs},mkcolor = black](A,B,C)
                            \end{{scope}}
                            "#,
                            get_point_name(point1, output, p1, scale),
                            get_point_name(point2, output, origin, scale),
                            get_point_name(point3, output, p2, scale),
                        );
                    }
                    AngleLine(_ln1, _ln2) => {
                        content += &format!(
                            r#"
                            \begin{{scope}}
                            \coordinate (A) at ({}, {});
                            \coordinate (B) at ({}, {});
                            \coordinate (C) at ({}, {});
                            \tkzMarkAngle[size = 2,mark = none,arc={no_arcs},mkcolor = black](A,B,C)
                            \end{{scope}}
                        "#,
                            p1.real,
                            p1.imaginary,
                            origin.real,
                            origin.imaginary,
                            p2.real,
                            p2.imaginary
                        );
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
    content += "\\end{tikzpicture} \\end{document}";

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
