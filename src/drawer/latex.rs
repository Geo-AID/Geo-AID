use std::{fs::File, io::Write};

use crate::projector::Rendered;

pub fn draw(target: String, canvas_size: (usize, usize), rendered: Vec<Rendered>) {
    let scale = f64::min(20.0/canvas_size.0 as f64, 20.0/canvas_size.1 as f64);
    let mut content = String::from(r#"
    \documentclass{article}
    \usepackage{tikz}
    \usetikzlibrary {angles,calc,quotes}
    \begin{document}
    \begin{tikzpicture}
    "#);
    for item in rendered {
        match item {
            Rendered::Point(point) => {
                let position = point.position * scale;
                content+=&format!(
                    "\\coordinate [label=left:${}$] ({}) at ({}, {}); \\fill[black] ({}) circle (1pt);",
                    point.label, point.label, position.real,
                    position.imaginary, point.label
                );
            },
            Rendered::Line(line) => {
                let pos1 = line.points.0 * scale;
                let pos2 = line.points.1 * scale;
                content+=&format!("\\draw ({},{}) -- ({},{});",pos1.real, pos1.imaginary, pos2.real, pos2.imaginary);
            },
        }
    }
    content+="\\end{tikzpicture} \\end{document}";

    let mut file = File::create(&target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
    
}