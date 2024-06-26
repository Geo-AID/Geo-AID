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

use crate::{
    projector::{
        Output, RenderedCircle, RenderedLine, RenderedPoint, RenderedRay,
        RenderedSegment,
    },
    script::figure::Style,
};

use crate::drawer::Draw;
use crate::geometry::Complex;
use crate::script::figure::MathString;

#[derive(Debug, Default)]
pub struct Raw {
    content: String
}

impl Raw {
    fn get_style_name(style: Style) -> &'static str {
        match style {
            Style::Dotted => "dotted",
            Style::Dashed => "dashed",
            Style::Bold => "bold",
            Style::Solid => "solid",
        }
    }

    fn draw_simple_segment(&mut self, (p1, p2): (Complex, Complex), style: Style, label: &MathString) {
        self.content += &format!(
            "{} line \"{}\" from ({:.3}, {:.3}) to ({:.3}, {:.3})\n",
            Self::get_style_name(style),
            label,
            p1.real,
            p1.imaginary,
            p2.real,
            p2.imaginary
        );
    }
}

impl Draw for Raw {
    fn begin(&mut self, _output: &Output) {}

    fn draw_point(&mut self, point: &RenderedPoint) {
        self.content += &format!("point \"{}\" at ({:.3}, {:.3})\n",
            point.item.label,
            point.position.real,
            point.position.imaginary
        );
    }

    fn draw_line(&mut self, line: &RenderedLine) {
        self.draw_simple_segment(line.points, line.item.style, &line.item.label);
    }

    fn draw_ray(&mut self, ray: &RenderedRay) {
        self.draw_simple_segment(ray.points, ray.item.style, &ray.item.label);
    }

    fn draw_segment(&mut self, segment: &RenderedSegment) {
        self.draw_simple_segment(segment.points, segment.item.style, &segment.item.label);
    }

    // fn draw_angle(&mut self, angle: &RenderedAngle, output: &Output) {
    //     let p_1 = angle.points.0;
    //     let p_origin = angle.points.1;
    //     let p_2 = angle.points.2;
    //     let no_arcs = String::from("l"); // Requires a change later! It has to be based on info from the script
    //     match &angle.expr.kind {
    //         ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
    //             self.content += &format!("\n3 points angle: points' coordinates point1 - {}, origin - {}, point2 - {}, number of arcs: {no_arcs}, mode: {} \n",
    //                 get_point_name(arm1, output, p_1), get_point_name(origin, output, p_origin), get_point_name(arm2, output, p_2), styling(angle.style)
    //             );
    //         }
    //         ScalarExpr::AngleLine(_) => {
    //             self.content += &format!("\n2 lines angle: points' coordinates: point1 - ({}, {}), origin - ({}, {}), point2 - ({}, {}), number of arcs: {no_arcs}, mode: {} \n",
    //                 p_1.real, p_1. imaginary, p_origin.real, p_origin.imaginary, p_2.real, p_2.imaginary, styling(angle.style)
    //             );
    //         }
    //         _ => unreachable!(),
    //     }
    // }

    fn draw_circle(&mut self, circle: &RenderedCircle) {
        self.content += &format!(
            "{} circle at ({:.3}, {:.3}) with radius {:.3}\n",
            Self::get_style_name(circle.item.style),
            circle.center.real,
            circle.center.imaginary,
            circle.radius,
        );
    }

    fn end(&mut self) -> &str {
        &self.content
    }
}