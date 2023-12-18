/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use std::rc::Rc;
use std::string::String;
use std::sync::Arc;
use std::{fs::File, io::Write, path::Path};

use crate::generator::expression::expr::AnglePoint;
use crate::generator::expression::{Expression, PointExpr, ScalarExpr};
use crate::generator::Complex;
use crate::projector::{
    Output, Rendered, RenderedAngle, RenderedCircle, RenderedLine, RenderedPoint, RenderedRay,
    RenderedSegment,
};
use crate::script::HashableArc;

use crate::script::figure::Style::{self, Bolded, Dashed, Solid, Dotted};

/// Function getting the point's name (if it exists, if not then it returns the point's coordinates).
fn get_point_name(
    expr: &Arc<Expression<PointExpr>>,
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

/// Function that assigns modes to the rendered variants.
fn assign_mode(rendered: &Rendered, mode: Style) -> String {
    match rendered {
        Rendered::Point(_) => unreachable!(),
        _ => match mode {
            Dotted => "dotted".to_string(),
            Dashed => "dashed".to_string(),
            Bolded => "ultra thick".to_string(),
            Solid => "thin".to_string(),
        },
    }
}

/// Function that handles the points.
fn points(point: &Rc<RenderedPoint>, scale: f64) -> String {
    let position = point.position * scale;
    let label_position = point.label_position * scale;
    format!(
        r#"
            \coordinate ({}) at ({}, {}); \fill[black] ({}) circle (1pt);
            \node at ({}, {}) {{{}}};
        "#,
        point.uuid, position.real, position.imaginary, point.uuid, label_position.real, label_position.imaginary, point.label
    )
}

/// Function that handles the lines.
fn lines(line: &RenderedLine, scale: f64, rendered: &Rendered) -> String {
    let pos1 = line.points.0 * scale;
    let pos2 = line.points.1 * scale;
    format!(
        r#"
            \begin{{scope}}
            \coordinate (A) at ({},{});
            \coordinate (B) at ({},{});
            \tkzDrawSegment[{}](A,B)
            \end{{scope}}
        "#,
        pos1.real,
        pos1.imaginary,
        pos2.real,
        pos2.imaginary,
        assign_mode(rendered, line.mode)
    )
}

/// Function that handles the angles.
fn angles(angle: &RenderedAngle, scale: f64, output: &Output, rendered: &Rendered) -> String {
    let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
    match &angle.expr.kind {
        ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
            format!(
                r#"
                \begin{{scope}}
                \coordinate (A) at {};
                \coordinate (B) at {};
                \coordinate (C) at {};
                \tkzMarkAngle[size = 0.5,mark = none,arc={no_arcs},mkcolor = black, {}](A,B,C)
                \end{{scope}}
                "#,
                get_point_name(arm1, output, angle.points.0, scale),
                get_point_name(origin, output, angle.points.1, scale),
                get_point_name(arm2, output, angle.points.2, scale),
                assign_mode(rendered, angle.mode)
            )
        }
        // There are hard coded values in \coordinate, it is intentional, every point has it label marked by Rendered::Point sequence above
        ScalarExpr::AngleLine(_) => {
            format!(
                r#"
                \begin{{scope}}
                \coordinate (A) at ({}, {});
                \coordinate (B) at ({}, {});
                \coordinate (C) at ({}, {});
                \tkzMarkAngle[size = 2,mark = none,arc={no_arcs},mkcolor = black, {}](A,B,C)
                \end{{scope}}
                "#,
                angle.points.0.real,
                angle.points.0.imaginary,
                angle.points.1.real,
                angle.points.1.imaginary,
                angle.points.2.real,
                angle.points.2.imaginary,
                assign_mode(rendered, angle.mode)
            )
        }
        _ => unreachable!(),
    }
}

/// Function that handles the segments.
fn segments(segment: &RenderedSegment, scale: f64, rendered: &Rendered) -> String {
    let pos1 = segment.points.0 * scale;
    let pos2 = segment.points.1 * scale;
    format!(
        r#"
        \begin{{scope}}
        \coordinate (A) at ({}, {});
        \coordinate (B) at ({}, {});
        \tkzDrawSegment[{}](A,B)
        \end{{scope}}
        "#,
        pos1.real,
        pos1.imaginary,
        pos2.real,
        pos2.imaginary,
        assign_mode(rendered, segment.mode)
    )
}

/// Function that handles the rays.
fn rays(ray: &RenderedRay, scale: f64, rendered: &Rendered) -> String {
    let pos1 = ray.points.0 * scale;
    let pos2 = ray.points.1 * scale;
    format!(
        r#"
        \begin{{scope}}
        \coordinate (A) at ({}, {});
        \coordinate (B) at ({}, {});
        \tkzDrawSegment[{}](A,B)
        \end{{scope}}
        "#,
        pos1.real,
        pos1.imaginary,
        pos2.real,
        pos2.imaginary,
        assign_mode(rendered, ray.mode)
    )
}

/// Function that handles the circles.
fn circles(circle: &RenderedCircle, scale: f64, rendered: &Rendered) -> String {
    let pos1 = circle.center * scale;
    let pos2 = circle.draw_point * scale;
    format!(
        r#"
        \begin{{scope}}
        \coordinate (A) at ({}, {});
        \coordinate (B) at ({}, {});
        \tkzDrawCircle[{}](A,B)
        \end{{scope}}
        "#,
        pos1.real,
        pos1.imaginary,
        pos2.real,
        pos2.imaginary,
        assign_mode(rendered, circle.mode)
    )
}
/// Draws the given figure to a .tex file using tikz library.
///
/// # Panics
/// Panics whenever there is a filesystem related problem.
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    // We must allow losing precision here.
    #[allow(clippy::cast_precision_loss)]
    let scale = f64::min(10.0 / canvas_size.0 as f64, 10.0 / canvas_size.1 as f64);
    let mut content = String::from(
        r"
    \documentclass{article}
    \usepackage{tikz}
    \usepackage{tkz-euclide}
    \usetikzlibrary {angles,calc,quotes}
    \begin{document}
    \begin{tikzpicture}
    ",
    );
    for item in &output.vec_rendered {
        content += &match item {
            Rendered::Point(point) => points(point, scale),
            Rendered::Line(line) => lines(line, scale, item),
            Rendered::Angle(angle) => angles(angle, scale, output, item),
            Rendered::Segment(segment) => segments(segment, scale, item),
            Rendered::Ray(ray) => rays(ray, scale, item),
            Rendered::Circle(circle) => circles(circle, scale, item),
        }
    }
    content += "\\end{tikzpicture} \\end{document}";

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
