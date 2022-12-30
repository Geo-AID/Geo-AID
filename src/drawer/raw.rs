use std::{fs::OpenOptions, io::Write, path::Path};

use crate::projector::Rendered;

/// # Panics
/// Panics whenever there is a filesystem problem
pub fn draw(target: &Path, canvas_size: (usize, usize), rendered: &Vec<Rendered>) {
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(target)
        .unwrap();

    file.write_all(format!("canvas size: {} by {}", canvas_size.0, canvas_size.1).as_bytes())
        .unwrap();

    for item in rendered {
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
        }
    }
}
