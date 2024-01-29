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

use std::{rc::Rc, sync::Arc};

use crate::{
    generator::{
        expression::{expr::AnglePoint, Expression, PointExpr, ScalarExpr},
        Complex,
    },
    labels::get_special_char_raw,
    projector::{
        Output, RenderedAngle, RenderedCircle, RenderedLine, RenderedPoint, RenderedRay,
        RenderedSegment,
    },
    script::{
        figure::{
            MathChar, MathIndex,
            Style::{self, Bold, Dashed, Dotted, Solid},
        },
        HashableArc,
    },
};

use crate::drawer::{Draw, Raw};

/// Function evaluating point's `MathString` to a string which then can be displayed.
fn math_string(point: &Rc<RenderedPoint>) -> String {
    let mut label = String::default();
    let mut seen = false;
    let mut lower_last = false;

    for char in &point.math_string.chars {
        match char {
            MathChar::Ascii(c) => {
                label += &c.to_string();
            }
            MathChar::Special(c) => {
                label += get_special_char_raw(c);
            }
            MathChar::SetIndex(i) => match i {
                MathIndex::Normal => {
                    if seen {
                        label += "}";
                    }
                    lower_last = false;
                }
                MathIndex::Lower => {
                    seen = true;
                    label += "_{";

                    lower_last = true;
                }
            },
            MathChar::Prime => {
                label += "'";
            }
        }
    }

    if lower_last {
        label += "}";
    }

    label
}

/// Function getting the point's name (if it exists, otherwise it returns the point's coordinates).
fn get_point_name(expr: &Arc<Expression<PointExpr>>, output: &Output, point: Complex) -> String {
    match output.map.get(&HashableArc::new(Arc::clone(expr))) {
        Some(p) => math_string(p),
        None => {
            format!("({}, {})", point.real, point.imaginary)
        }
    }
}

/// Function that assigns the styling.
fn styling(mode: Style) -> String {
    match mode {
        Dotted => "dotted".to_string(),
        Dashed => "dashed".to_string(),
        Bold => "bolded".to_string(),
        Solid => "default".to_string(),
    }
}

impl Draw for Raw {
    fn begin(&mut self) {
        
    }

    fn draw_point(&mut self, point: &Rc<RenderedPoint>) {
        self.content += &format!("\npoint: label - \"{}\", coordinates: point - ({:.3}, {:.3}), label - ({:.3}, {:.3}) \n", 
            math_string(point), 
            point.position.real,
            point.position.imaginary,
            point.label_position.real,
            point.label_position.imaginary
        );
    }

    fn draw_line(&mut self, line: &RenderedLine) {
        let p1 = line.points.0;
        let p2 = line.points.1;
        self.content += &format!(
            "\nline: points' coordinates: ({:.3}, {:.3}) and ({:.3}, {:.3}), mode: {} \n",
            p1.real,
            p1.imaginary,
            p2.real,
            p2.imaginary,
            styling(line.style)
        );
    }

    fn draw_ray(&mut self, ray: &RenderedRay) {
        self.content += &format!("\nray: points' coordinates first point - ({:.3}, {:.3}) second point - ({:.3}. {:.3}), mode: {} \n", 
            ray.points.0.real,
            ray.points.1.imaginary,
            ray.draw_point.real,
            ray.draw_point.imaginary,
            styling(ray.style)
        );
    }

    fn draw_segment(&mut self, segment: &RenderedSegment) {
        self.content += &format!(
            "\nsegment: points' coordinates point1 - ({:.3}, {:.3}), point2 - ({:.3}, {:.3}), mode: {} \n",
            segment.points.0.real,
            segment.points.1.real,
            segment.points.0.imaginary,
            segment.points.1.imaginary,
            styling(segment.style)
        );
    }

    fn draw_angle(&mut self, angle: &RenderedAngle, output: &Output) {
        let p_1 = angle.points.0;
        let p_origin = angle.points.1;
        let p_2 = angle.points.2;
        let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
        match &angle.expr.kind {
            ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
                self.content += &format!("\n3 points angle: points' coordinates point1 - {}, origin - {}, point2 - {}, number of arcs: {no_arcs}, mode: {} \n",
                    get_point_name(arm1, output, p_1), get_point_name(origin, output, p_origin), get_point_name(arm2, output, p_2), styling(angle.style)
                );
            }
            ScalarExpr::AngleLine(_) => {
                self.content += &format!("\n2 lines angle: points' coordinates: point1 - ({}, {}), origin - ({}, {}), point2 - ({}, {}), number of arcs: {no_arcs}, mode: {} \n", 
                    p_1.real, p_1. imaginary, p_origin.real, p_origin.imaginary, p_2.real, p_2.imaginary, styling(angle.style)
                );
            }
            _ => unreachable!(),
        }
    }

    fn draw_circle(&mut self, circle: &RenderedCircle) {
        self.content += &format!(
            "\ncircle: center - ({:.3}, {:.3}) radius - {:.3}, mode: {} \n",
            circle.center.real,
            circle.center.imaginary,
            circle.radius,
            styling(circle.style)
        );
    }

    fn close_draw(&mut self) {
        todo!()
    }

    fn end(&self) -> &String {
        todo!()
    }
}

/*

// Function that handles the points.
fn points(mut file: &File, point: &Rc<RenderedPoint>) {
    file.write_all((format!("\npoint: label - \"{}\", coordinates: point - ({:.3}, {:.3}), label - ({:.3}, {:.3}) \n", 
        math_string(point),
        point.position.real,
        point.position.imaginary,
        point.label_position.real,
        point.label_position.imaginary))
            .as_bytes())
            .unwrap();
}

// Function that handles the lines.
fn lines(mut file: &File, line: &RenderedLine, rendered: &Rendered) {
    let p1 = line.points.0;
    let p2 = line.points.1;
    file.write_all(
        format!(
            "\nline: points' coordinates: ({:.3}, {:.3}) and ({:.3}, {:.3}), mode: {} \n",
            p1.real,
            p1.imaginary,
            p2.real,
            p2.imaginary,
            styling(line.style)
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
            file.write_all(format!("\n3 points angle: points' coordinates point1 - {}, origin - {}, point2 - {}, number of arcs: {no_arcs}, mode: {} \n",
                        get_point_name(arm1, output, p_1), get_point_name(origin, output, p_origin), get_point_name(arm2, output, p_2), styling(angle.style)).as_bytes()).unwrap();
        }
        ScalarExpr::AngleLine(_) => {
            file.write_all(format!("\n2 lines angle: points' coordinates: point1 - ({}, {}), origin - ({}, {}), point2 - ({}, {}), number of arcs: {no_arcs}, mode: {} \n", 
                            p_1.real, p_1. imaginary, p_origin.real, p_origin.imaginary, p_2.real, p_2.imaginary, styling(angle.style)
                        ).as_bytes()).unwrap();
        }
        _ => unreachable!(),
    }
}

/// Function that handles the segments.
fn segments(mut file: &File, segment: &RenderedSegment, rendered: &Rendered) {
    file.write_all(
        format!(
            "\nsegment: points' coordinates point1 - ({:.3}, {:.3}), point2 - ({:.3}, {:.3}), mode: {} \n",
            segment.points.0.real,
            segment.points.1.real,
            segment.points.0.imaginary,
            segment.points.1.imaginary,
            styling(segment.style)
        )
        .as_bytes(),
    )
    .unwrap();
}

/// Function that handles the rays.
fn rays(mut file: &File, ray: &RenderedRay, rendered: &Rendered) {
    file.write_all(format!("\nray: points' coordinates first point - ({:.3}, {:.3}) second point - ({:.3}. {:.3}), mode: {} \n", 
        ray.points.0.real,
        ray.points.1.imaginary,
        ray.draw_point.real,
        ray.draw_point.imaginary,
        styling(ray.style)).
            as_bytes())
            .unwrap();
}

/// Function that handles the circles.
fn circles(mut file: &File, circle: &RenderedCircle, rendered: &Rendered) {
    file.write_all(
        format!(
            "\ncircle: center - ({:.3}, {:.3}) radius - {:.3}, mode: {} \n",
            circle.center.real,
            circle.center.imaginary,
            circle.radius,
            styling(circle.style)
        )
        .as_bytes(),
    )
    .unwrap();
}

/// Outputs a file which can be read.
///
/// # Panics
/// Panics whenever there is a filesystem problem.
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    let mut file = File::create(target).unwrap();

    file.write_all(format!("canvas size: {} by {}", canvas_size.0, canvas_size.1).as_bytes())
        .unwrap();

    for item in &output.vec_rendered {
        match item {
            Rendered::Point(point) => points(&file, point),
            Rendered::Line(line) => lines(&file, line, item),
            Rendered::Angle(angle) => angles(&file, angle, output, item),
            Rendered::Segment(segment) => segments(&file, segment, item),
            Rendered::Ray(ray) => rays(&file, ray, item),
            Rendered::Circle(circle) => circles(&file, circle, item),
        }
    }
}
*/
