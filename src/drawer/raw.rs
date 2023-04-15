use std::{fs::File, io::Write, path::Path, sync::Arc};

use crate::{
    generator::Complex,
    projector::{Output, Rendered},
    script::{
        Expression::{self, AngleLine, AnglePoint},
        HashableArc, Weighed,
    },
};

/// Function getting the point's name (if it exists, if not it returns the point's coordinates).
fn get_point_name(expr: &Arc<Weighed<Expression>>, output: &Output, point: Complex) -> String {
    match output.map.get(&HashableArc::new(Arc::clone(expr))) {
        Some(p) => p.label.to_string(),
        None => {
            format!("({}, {})", point.real, point.imaginary)
        }
    }
}

/// # Panics
/// Panics whenever there is a filesystem problem
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    let mut file = File::create(target).unwrap();

    file.write_all(format!("canvas size: {} by {}", canvas_size.0, canvas_size.1).as_bytes())
        .unwrap();

    for item in &output.vec_rendered {
        match item {
            Rendered::Point(point) => {
                file.write_all((format!("\npoint: label of the point - \"{}\", x and y coordinates of the point - ({:.3}, {:.3})", 
                point.label, point.position.real, point.position.imaginary)).as_bytes()).unwrap();
            }
            Rendered::Line(line) => {
                let p1 = line.points.0;
                let p2 = line.points.1;
                file.write_all(
                    format!(
                        "\nline: label - \"{}\", 
                x and y coordinates of the two points - ({:.3}, {:.3}) and ({:.3}, {:.3})",
                        line.label, p1.real, p1.imaginary, p2.real, p2.imaginary
                    )
                    .as_bytes(),
                )
                .unwrap();
            }
            Rendered::Angle(angle) => {
                let p_1 = angle.points.0;
                let origin = angle.points.1;
                let p_2 = angle.points.2;
                let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
                match &angle.expr.object {
                    AnglePoint(p1, p2, p3) => {
                        file.write_all(format!("\nangle defined with 3 points: points with x and y coordinates: first point - {}, origin - {}, second point - {}. Number of arcs: {no_arcs}",
                        get_point_name(p1, output, p_1), get_point_name(p2, output, origin), get_point_name(p3, output, p_2)).as_bytes()).unwrap();
                    }
                    AngleLine(_ln1, _ln2) => {
                        file.write_all(format!("\nangle defined with 2 lines: coordinates of the points defining the angle: first point - ({}, {}), origin - ({}, {}), second point - ({}, {})", 
                            p_1.real, p_1. imaginary, origin.real, origin.imaginary, p_2.real, p_2.imaginary
                        ).as_bytes()).unwrap();
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}
