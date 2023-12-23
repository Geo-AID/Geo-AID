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

use std::{fs::File, io::Write, path::Path, rc::Rc};

use crate::{
    generator::{geometry, Complex},
    projector::{
        Output, Rendered, RenderedAngle, RenderedCircle, RenderedLine, RenderedPoint, RenderedRay,
        RenderedSegment,
    },
    script::figure::Style::{self, Bold, Dashed, Solid, Dotted},
};

/// Function that assigns modes to the rendered variants.
fn assign_mode(rendered: &Rendered, mode: Style) -> (String, String) {
    let default_width = "0.5".to_string();
    let default_strarray = "1,0".to_string();
    match rendered {
        Rendered::Point(_) => unreachable!(),
        _ => match mode {
            Dotted => (default_width, "0.8,1".to_string()),
            Dashed => (default_width, "2,2".to_string()),
            Bold => ("2".to_string(), default_strarray),
            Solid => ("1".to_string(), default_strarray),
        },
    }
}

fn points(point: &Rc<RenderedPoint>) -> String {
    format!(
        r#"
            <circle cx="{}" cy="{}" fill="black" r="1"/>
            <text transform="scale(-1,1), rotate(180, {}, {})" 
            text-anchor="middle" dominant-baseline="middle" 
            style="font-family: 'Computer Modern'" font-size="10px"
            stroke="black" stroke-width="0" x="{}" y="{}">{}</text>
        "#,
        point.position.real, point.position.imaginary, -point.label_position.real, point.label_position.imaginary, -point.label_position.real, point.label_position.imaginary, point.label
    )
}

fn lines(ln: &RenderedLine, rendered: &Rendered) -> String {
    let p1 = Complex::new(ln.points.0.real, ln.points.0.imaginary);
    let p2 = Complex::new(ln.points.1.real, ln.points.1.imaginary);
    format!(
        r#"
        <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
        "#,
        assign_mode(rendered, ln.mode).0,
        assign_mode(rendered, ln.mode).1,
        p1.real,
        p2.real,
        p1.imaginary,
        p2.imaginary
    )
}

fn angles(angle: &RenderedAngle, rendered: &Rendered) -> String {
    let x: u32 = 45;
    format!(
        r#"
            <g transform="translate({}, {}) rotate({}, 0, 0)" fill="transparent">
            <path stroke-dasharray="{}" d="M {}, 0 A 45, 45, 0, 0, 0, {}, {}" stroke="black" stroke-width="{}" /> 
            </g>
        "#,
        angle.points.1.real,
        angle.points.1.imaginary,
        geometry::get_line(angle.points.1, angle.points.0)
            .direction
            .arg()
            .to_degrees(),
        assign_mode(rendered, angle.mode).1,
        x, // It should probably be a constant. For now we will leave it like this.
        angle.angle_value.cos() * 45.0,
        -angle.angle_value.sin() * 45.0,
        assign_mode(rendered, angle.mode).0
    )
}

fn segments(segment: &RenderedSegment, rendered: &Rendered) -> String {
    format!(
        r#"
            <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
        "#,
        assign_mode(rendered, segment.mode).0,
        assign_mode(rendered, segment.mode).1,
        segment.points.0.real,
        segment.points.1.real,
        segment.points.0.imaginary,
        segment.points.1.imaginary
    )
}

fn rays(ray: &RenderedRay, rendered: &Rendered) -> String {
    format!(
        r#"
            <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
        "#,
        assign_mode(rendered, ray.mode).0,
        assign_mode(rendered, ray.mode).1,
        ray.points.0.real,
        ray.points.1.real,
        ray.points.0.imaginary,
        ray.points.1.imaginary,
    )
}

fn circles(circle: &RenderedCircle, rendered: &Rendered) -> String {
    format!(
        r#"
            <circle cx="{}" cy="{}" r="{}" stroke="black" stroke-width="{}" stroke-dasharray="{}" fill="transparent"/>
        "#,
        circle.center.real,
        circle.center.imaginary,
        circle.radius,
        assign_mode(rendered, circle.mode).0,
        assign_mode(rendered, circle.mode).1
    )
}

/// Draws the given figure as .svg format.
///
/// # Panics
/// Panics when there are issues with writing to the file.
pub fn draw(target: &Path, canvas_size: (usize, usize), output: &Output) {
    let mut content = String::new();
    content += &format!(
        r#"
            <svg height="{}" width="{}" xmlns="http://www.w3.org/2000/svg">
            <font>
            <font-face font-family="New Computer Modern">
            </font-face>
            </font>
            <g transform="translate(0,{})">
            <g transform="scale(1,-1)">
        "#,
        canvas_size.0, canvas_size.1, canvas_size.1
    );

    for elem in &output.vec_rendered {
        content += &match elem {
            Rendered::Point(point) => points(point),
            Rendered::Line(line) => lines(line, elem),
            Rendered::Angle(angle) => angles(angle, elem),
            Rendered::Segment(segment) => segments(segment, elem),
            Rendered::Ray(ray) => rays(ray, elem),
            Rendered::Circle(circle) => circles(circle, elem),
        };
    }
    content += " </g> </g> </svg>";

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}