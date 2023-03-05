use std::{fs::OpenOptions, io::Write, path::Path, sync::Arc};

use crate::{
    projector::{Output, Rendered},
    script::{Expression, HashableArc},
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
                if let Expression::Line(point1, point2) = &line.expr.object {
                    file.write_all(format!("\nline: label of the line - \"{}\", goes through point {:?}({:.3}, {:.3}) and point {:?}({:.3}, {:.3})", 
                    line.label, output.map.get(&HashableArc::new(Arc::clone(point1))).unwrap(), p1.real, p1.imaginary,output.map.get(&HashableArc::new(Arc::clone(point2))).unwrap(), p2.real, p2.imaginary).as_bytes()).unwrap();
                } else {
                    file.write_all(format!("\nline: label of the line - \"{}\", goes through point ({:.3}, {:.3}) and point ({:.3}, {:.3})", 
                    line.label, p1.real, p1.imaginary, p2.real, p2.imaginary).as_bytes()).unwrap();
                }
            }
            Rendered::Angle(angle) => {
                let p_1 = angle.points.0;
                let p_2 = angle.points.1;
                let p_3 = angle.points.2;
                match &angle.expr.object {
                    Expression::AnglePoint(p1, p2, p3) => {
                        let no_arcs = String::from("l"); // Requires a change later!
                        file.write_all((format!("\nangle: number of arcs - \"{}\", defined by - point {:?}({:.3}, {:.3}), point {:?}({:.3}, {:.3}) and point {:?}({:.3},{:.3}))", 
                        no_arcs, output.map.get(&HashableArc::new(Arc::clone(p1))).unwrap(), p_1.real, p_1.imaginary, output.map.get(&HashableArc::new(Arc::clone(p2))).unwrap(), p_2.real, p_2.imaginary, output.map.get(&HashableArc::new(Arc::clone(p3))).unwrap(), p_3.real, p_3.imaginary)).as_bytes()).unwrap();
                    }
                    Expression::AngleLine(ln1, ln2) => {
                        
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}
