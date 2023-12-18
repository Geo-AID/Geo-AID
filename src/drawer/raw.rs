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

use std::{fs::File, io::Write, path::Path, rc::Rc, sync::Arc};

use crate::{
    generator::{
        expression::{expr::AnglePoint, Expression, PointExpr, ScalarExpr},
        Complex,
    },
    projector::{
        Output, Rendered, RenderedAngle, RenderedCircle, RenderedLine, RenderedPoint, RenderedRay,
        RenderedSegment,
    },
    script::{
        figure::Style::{self, Bolded, Dashed, Solid, Dotted},
        HashableArc,
    },
};

/// Function getting the point's name (if it exists, otherwise it returns the point's coordinates).
fn get_point_name(expr: &Arc<Expression<PointExpr>>, output: &Output, point: Complex) -> String {
    match output.map.get(&HashableArc::new(Arc::clone(expr))) {
        Some(p) => p.label.to_string(),
        None => {
            format!("({}, {})", point.real, point.imaginary)
        }
    }
}

/// Function that assigns drawing modes to the rendered variants.
fn assign_mode(rendered: &Rendered, mode: Style) -> String {
    match rendered {
        Rendered::Point(_) => unreachable!(),
        _ => match mode {
            Dotted => "dotted".to_string(),
            Dashed => "dashed".to_string(),
            Bolded => "bolded".to_string(),
            Solid => "default".to_string(),
        },
    }
}

/// Function that handles the points.
fn points(mut file: &File, point: &Rc<RenderedPoint>) {
    file.write_all((format!("\npoint: label of the point - \"{}\", coordinates: point - ({:.3}, {:.3}), label - ({:.3}, {:.3})", 
        point.label,
        point.position.real,
        point.position.imaginary,
        point.label_position.real,
        point.label_position.imaginary))
            .as_bytes())
            .unwrap();
}

/// Function that handles the lines.
fn lines(mut file: &File, line: &RenderedLine, rendered: &Rendered) {
    let p1 = line.points.0;
    let p2 = line.points.1;
    file.write_all(
        format!(
            "\nline: label - \"{}\", 
            x and y coordinates of the two points - ({:.3}, {:.3}) and ({:.3}, {:.3}), mode: {}",
            line.label,
            p1.real,
            p1.imaginary,
            p2.real,
            p2.imaginary,
            assign_mode(rendered, line.mode)
        )
        .as_bytes(),
    )
    .unwrap();
}

/// Function that handles the angles.
fn angles(mut file: &File, angle: &RenderedAngle, output: &Output, rendered: &Rendered) {
    let p_1 = angle.points.0;
    let p_origin = angle.points.1;
    let p_2 = angle.points.2;
    let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
    match &angle.expr.kind {
        ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
            file.write_all(format!("\nangle defined with 3 points: points with x and y coordinates: point1 - {}, origin - {}, point2 - {}. Number of arcs: {no_arcs}. Mode: {}",
                        get_point_name(arm1, output, p_1), get_point_name(origin, output, p_origin), get_point_name(arm2, output, p_2), assign_mode(rendered, angle.mode)).as_bytes()).unwrap();
        }
        ScalarExpr::AngleLine(_) => {
            file.write_all(format!("\nangle defined with 2 lines: coordinates of the points defining the angle: point1 - ({}, {}), origin - ({}, {}), point2 - ({}, {}), mode: {}", 
                            p_1.real, p_1. imaginary, p_origin.real, p_origin.imaginary, p_2.real, p_2.imaginary, assign_mode(rendered, angle.mode)
                        ).as_bytes()).unwrap();
        }
        _ => unreachable!(),
    }
}

/// Function that handles the segments.
fn segments(mut file: &File, segment: &RenderedSegment, rendered: &Rendered) {
    file.write_all(
        format!(
            "\nsegment defined by two points: point1 - ({:.3}, {:.3}), point2 - ({:.3}, {:.3}), mode: {}",
            segment.points.0.real,
            segment.points.1.real,
            segment.points.0.imaginary,
            segment.points.1.imaginary,
            assign_mode(rendered, segment.mode)
        )
        .as_bytes(),
    )
    .unwrap();
}

/// Function that handles the rays.
fn rays(mut file: &File, ray: &RenderedRay, rendered: &Rendered) {
    file.write_all(format!("\nray defined with two points: first point - ({:.3}, {:.3}) second point - ({:.3}. {:.3}). Mode: {}", 
        ray.points.0.real,
        ray.points.1.imaginary,
        ray.draw_point.real,
        ray.draw_point.imaginary,
        assign_mode(rendered, ray.mode)).
            as_bytes())
            .unwrap();
}

/// Function that handles the circles.
fn circles(mut file: &File, circle: &RenderedCircle, rendered: &Rendered) {
    file.write_all(
        format!(
            "\ncircle: center - ({:.3}, {:.3}) radius - {:.3}, mode: {}",
            circle.center.real,
            circle.center.imaginary,
            circle.radius,
            assign_mode(rendered, circle.mode)
        )
        .as_bytes(),
    )
    .unwrap();
}

/// Outputs a file which can be read
///
/// # Panics
/// Panics whenever there is a filesystem problem
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    let mut file = File::create(target).unwrap();

    file.write_all(format!("canvas size: {} by {}", canvas_size.0, canvas_size.1).as_bytes())
        .unwrap();

    for item in &output.vec_rendered {
        match item {
            Rendered::Point(point) => {
                points(&file, point);
            }
            Rendered::Line(line) => {
                lines(&file, line, item);
            }
            Rendered::Angle(angle) => {
                angles(&file, angle, output, item);
            }
            Rendered::Segment(segment) => {
                segments(&file, segment, item);
            }
            Rendered::Ray(ray) => {
                rays(&file, ray, item);
            }
            Rendered::Circle(circle) => {
                circles(&file, circle, item);
            }
        }
    }
}
