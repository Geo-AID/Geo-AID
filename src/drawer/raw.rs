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

use std::{fs::File, io::Write, path::Path, sync::Arc};

use crate::{
    generator::{
        expression::{expr::AnglePoint, Expression, PointExpr, ScalarExpr},
        Complex,
    },
    projector::{Output, Rendered},
    script::HashableArc,
};

/// Function getting the point's name (if it exists, if not it returns the point's coordinates).
fn get_point_name(expr: &Arc<Expression<PointExpr>>, output: &Output, point: Complex) -> String {
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
                let p_origin = angle.points.1;
                let p_2 = angle.points.2;
                let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
                match &angle.expr.kind {
                    ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
                        file.write_all(format!("\nangle defined with 3 points: points with x and y coordinates: point1 - {}, origin - {}, point2 - {}. Number of arcs: {no_arcs}",
                        get_point_name(arm1, output, p_1), get_point_name(origin, output, p_origin), get_point_name(arm2, output, p_2)).as_bytes()).unwrap();
                    }
                    ScalarExpr::AngleLine(_) => {
                        file.write_all(format!("\nangle defined with 2 lines: coordinates of the points defining the angle: point1 - ({}, {}), origin - ({}, {}), point2 - ({}, {})", 
                            p_1.real, p_1. imaginary, p_origin.real, p_origin.imaginary, p_2.real, p_2.imaginary
                        ).as_bytes()).unwrap();
                    }
                    _ => unreachable!(),
                }
            }
            Rendered::Segment(segment) => {
                file.write_all(
                    format!(
                        "\nsegment defined by two points: point1 - ({:.3}, {:.3}), point2 - ({:.3}, {:.3})",
                        segment.points.0.real,
                        segment.points.1.real,
                        segment.points.0.imaginary,
                        segment.points.1.imaginary
                    )
                    .as_bytes(),
                )
                .unwrap();
            }
            Rendered::Ray(ray) => {
                file.write_all(format!("\nray defined with two points: first point - ({:.3}, {:.3}) second point - ({:.3}. {:.3})", ray.points.0.real, ray.points.1.imaginary, ray.draw_point.real, ray.draw_point.imaginary).as_bytes()).unwrap();
            }
            Rendered::Circle(circle) => {
                file.write_all(
                    format!(
                        "\ncircle: center - ({:.3}, {:.3}) radius - {:.3}",
                        circle.center.real, circle.center.imaginary, circle.radius
                    )
                    .as_bytes(),
                )
                .unwrap();
            }
        }
    }
}
