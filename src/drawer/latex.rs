use std::sync::Arc;
use std::{fs::File, io::Write, path::Path};

use crate::projector::{Output, Rendered};
use crate::script::Expression::{AngleLine, AnglePoint};
use crate::script::HashableArc;

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
                    point.label, point.label, position.real,
                    position.imaginary, point.label
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
                let p1 = angle.points.0 * scale;
                let origin = angle.points.1 * scale;
                let p2 = angle.points.2 * scale;
                let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
                match &angle.expr.object {
                    AnglePoint(p1, p2, p3) => {
                        let point1 = HashableArc::new(Arc::clone(p1));
                        let point2 = HashableArc::new(Arc::clone(p2));
                        let point3 = HashableArc::new(Arc::clone(p3));
                        let p1_name = output.map.get(&point1).unwrap();
                        let p2_name = output.map.get(&point2).unwrap();
                        let p3_name = output.map.get(&point3).unwrap();

                        content += &format!(
                            r#"
                            \tkzMarkAngle[size = 0.5,mark = none,arc={},mkcolor = black]({:?},{:?},{:?})
                            "#, no_arcs, p1_name, p2_name, p3_name
                        );
                    }
                    AngleLine(_ln1, _ln2) => {
                        content += &format!(
                            r#"
                        \coordinate (A) at ({}, {})
                        \coordinate (B) at ({}, {})
                        \coordinate (C) at ({}, {})
                        \tkzMarkAngle[size = 0.5,mark = none,arc={no_arcs},mkcolor = black](A,B,C)"#,
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
