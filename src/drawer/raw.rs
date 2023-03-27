use std::{fs::OpenOptions, io::Write, path::Path, sync::Arc};

use crate::{
    projector::{Output, Rendered},
    script::{
        Expression::{AngleLine, AnglePoint},
        HashableArc,
    },
};

/// # Panics
/// Panics whenever there is a filesystem problem
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(target)
        .unwrap();

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
                file.write_all(format!("\nline: label of the line - \"{}\", 
                x and y coordinates of points defining the line - ({:.3}, {:.3}) and ({:.3}, {:.3})", 
                line.label, p1.real, p1.imaginary, p2.real, p2.imaginary).as_bytes()).unwrap();
            }
            Rendered::Angle(angle) => {
                let p_1 = angle.points.0;
                let origin = angle.points.1;
                let p_2 = angle.points.2;
                let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
                match &angle.expr.object {
                    AnglePoint(p1, p2, p3) => {
                        let point1 = HashableArc::new(Arc::clone(p1));
                        let point2 = HashableArc::new(Arc::clone(p2));
                        let point3 = HashableArc::new(Arc::clone(p3));
                        let p1_name = output.map.get(&point1).unwrap();
                        let p2_name = output.map.get(&point2).unwrap();
                        let p3_name = output.map.get(&point3).unwrap();

                        file.write_all(format!("\nangle defined with 3 points: points defining the angle with their x and y coordinates: first point - ({})({}, {}), origin - ({})({}, {}), second point - ({})({}, {}). Number of arcs: {}",
                        p1_name.label, p1_name.position.real, p1_name.position.imaginary, p2_name.label, p2_name.position.real, p2_name.position.imaginary, p3_name.label, p3_name.position.real, p3_name.position.imaginary, no_arcs).as_bytes()).unwrap();
                    }
                    AngleLine(_ln1, _ln2) => {
                        file.write_all(format!("\nangle defined with 2 lines: coordinates of the points defining the lines: first point - ({}, {}), origin - ({}, {}), second point - ({}, {})", p_1.real, p_1. imaginary, origin.real, origin.imaginary, p_2.real, p_2.imaginary).as_bytes()).unwrap();
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}
