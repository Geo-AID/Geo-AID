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

use crate::{
    generator::{geometry, Complex},
    projector::{
        Output, RenderedAngle, RenderedCircle, RenderedLine, RenderedPoint, RenderedRay,
        RenderedSegment,
    },
    script::figure::Style,
};

use crate::drawer::{Draw, Svg};

/// Function getting the width styling for svg.
fn style_width(mode: Style) -> String {
    let default_width = "0.5".to_string();

    match mode {
        Style::Dashed | Style::Dotted => default_width,
        Style::Bold => "2".to_string(),
        Style::Solid => "1".to_string(), 
    }
}

/// Function getting the dashing styling for svg.
fn style_dashing(mode: Style) -> String {
    let default_strarray = "1,0".to_string();

    match mode {
        Style::Dotted => "0.8,1".to_string(),
        Style::Dashed => "2,2".to_string(),
        Style::Bold | Style::Solid => default_strarray,
    }
}

impl Draw for Svg {
    fn begin(&mut self) {
        self.content += &format!(
            r#"
                <svg height="{}" width="{}" xmlns="http://www.w3.org/2000/svg">
                    <font>
                        <font-face font-family="New Computer Modern">
                        </font-face>
                    </font>
                <g transform="translate(0,{})">
                <g transform="scale(1,-1)">
            "#,
            self.canvas_size.0, self.canvas_size.1, self.canvas_size.0,
        );
    }

    fn draw_point(&mut self, point: &Rc<RenderedPoint>) {
        self.content += &format!(
            r#"
                <circle cx="{}" cy="{}" fill="black" r="1"/>
                    <text transform="scale(1,-1)" 
                        text-anchor="middle" dominant-baseline="middle" 
                        style="font-family: 'Computer Modern'" font-size="10px"
                        stroke="black" stroke-width="0" x="{}" y="-{}">{}
                    </text>
            "#,
            point.position.real, point.position.imaginary, point.label_position.real, point.label_position.imaginary, point.math_string.chars.first().unwrap()
        );
    }

    fn draw_line(&mut self, line: &RenderedLine) {
        let p1 = Complex::new(line.points.0.real, line.points.0.imaginary);
        let p2 = Complex::new(line.points.1.real, line.points.1.imaginary);
        self.content += &format!(
            r#"
                <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
            "#,
            style_width(line.style),
            style_dashing(line.style),
            p1.real,
            p2.real,
            p1.imaginary,
            p2.imaginary
        );
    }

    fn draw_ray(&mut self, ray: &RenderedRay) {
        self.content += &format!(
            r#"
                <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
            "#,
            style_width(ray.style),
            style_dashing(ray.style),
            ray.points.0.real,
            ray.points.1.real,
            ray.points.0.imaginary,
            ray.points.1.imaginary,
        );
    }

    fn draw_segment(&mut self, segment: &RenderedSegment) {
        self.content += &format!(
            r#"
                <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
            "#,
            style_width(segment.style),
            style_dashing(segment.style),
            segment.points.0.real,
            segment.points.1.real,
            segment.points.0.imaginary,
            segment.points.1.imaginary
        );
    }

    fn draw_angle(&mut self, angle: &RenderedAngle, _output: &Output) {
        let x: u32 = 45;
        self.content += &format!(
            r#"
                <g transform="translate({}, {}) rotate({}, 0, 0)" fill="transparent">
                    <path stroke-dasharray="{}" d="M {}, 0 A 45, 45, 0, 0, 0, {}, {}" stroke="black" stroke-width="{}"/> 
                </g>
            "#,
            angle.points.1.real,
            angle.points.1.imaginary,
            geometry::get_line(angle.points.1, angle.points.0)
                .direction
                .arg()
                .to_degrees(),
            style_dashing(angle.style),
            x, // It should probably be a constant. For now we will leave it like this.
            angle.angle_value.cos() * 45.0,
            -angle.angle_value.sin() * 45.0,
            style_width(angle.style),
        );
    }

    fn draw_circle(&mut self, circle: &RenderedCircle) {
        self.content += &format!(
            r#"
                <circle cx="{}" cy="{}" r="{}" stroke="black" stroke-width="{}" stroke-dasharray="{}" fill="transparent"/>
            "#,
            circle.center.real,
            circle.center.imaginary,
            circle.radius,
            style_width(circle.style),
            style_dashing(circle.style),
        );
    }

    fn close_draw(&mut self) {
        self.content += "</g> </g> </svg>";
    }

    fn end(&self) -> &String {
        &self.content
    }
}

/*fn points(point: &Rc<RenderedPoint>) -> String {
    format!(
        r#"
            <circle cx="{}" cy="{}" fill="black" r="1"/>
                <text transform="scale(1,-1)" 
                    text-anchor="middle" dominant-baseline="middle" 
                    style="font-family: 'Computer Modern'" font-size="10px"
                    stroke="black" stroke-width="0" x="{}" y="-{}">{}
                </text>
        "#,
        point.position.real, point.position.imaginary, point.label_position.real, point.label_position.imaginary, point.math_string.chars.first().unwrap()
    )
}

fn lines(ln: &RenderedLine) -> String {
    let p1 = Complex::new(ln.points.0.real, ln.points.0.imaginary);
    let p2 = Complex::new(ln.points.1.real, ln.points.1.imaginary);
    format!(
        r#"
            <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
        "#,
        style_width(ln.style),
        style_dashing(ln.style),
        p1.real,
        p2.real,
        p1.imaginary,
        p2.imaginary
    )
}

fn angles(angle: &RenderedAngle) -> String {
    let x: u32 = 45;
    format!(
        r#"
            <g transform="translate({}, {}) rotate({}, 0, 0)" fill="transparent">
                <path stroke-dasharray="{}" d="M {}, 0 A 45, 45, 0, 0, 0, {}, {}" stroke="black" stroke-width="{}"/> 
            </g>
        "#,
        angle.points.1.real,
        angle.points.1.imaginary,
        geometry::get_line(angle.points.1, angle.points.0)
            .direction
            .arg()
            .to_degrees(),
        style_dashing(angle.style),
        x, // It should probably be a constant. For now we will leave it like this.
        angle.angle_value.cos() * 45.0,
        -angle.angle_value.sin() * 45.0,
        style_width(angle.style),
    )
}

fn segments(segment: &RenderedSegment) -> String {
    format!(
        r#"
            <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
        "#,
        style_width(segment.style),
        style_dashing(segment.style),
        segment.points.0.real,
        segment.points.1.real,
        segment.points.0.imaginary,
        segment.points.1.imaginary
    )
}

fn rays(ray: &RenderedRay) -> String {
    format!(
        r#"
            <line stroke-width="{}" stroke-dasharray="{}" stroke="black" x1="{}" x2="{}" y1="{}" y2="{}"/>
        "#,
        style_width(ray.style),
        style_dashing(ray.style),
        ray.points.0.real,
        ray.points.1.real,
        ray.points.0.imaginary,
        ray.points.1.imaginary,
    )
}

fn circles(circle: &RenderedCircle) -> String {
    format!(
        r#"
            <circle cx="{}" cy="{}" r="{}" stroke="black" stroke-width="{}" stroke-dasharray="{}" fill="transparent"/>
        "#,
        circle.center.real,
        circle.center.imaginary,
        circle.radius,
        style_width(circle.style),
        style_dashing(circle.style),
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
        canvas_size.0, canvas_size.1, canvas_size.0,
    );

    for elem in &output.vec_rendered {
        content += &match elem {
            Rendered::Point(point) => points(point),
            Rendered::Line(line) => lines(line),
            Rendered::Angle(angle) => angles(angle),
            Rendered::Segment(segment) => segments(segment),
            Rendered::Ray(ray) => rays(ray),
            Rendered::Circle(circle) => circles(circle),
        };
    }
    content += "</g> </g> </svg>";

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}*/
