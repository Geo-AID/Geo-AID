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

use std::{path::Path, rc::Rc, fs::File, io::Write};

use crate::projector::{Output, RenderedPoint, RenderedLine, RenderedAngle, RenderedSegment, RenderedRay, RenderedCircle, Rendered};

pub mod json;
pub mod latex;
pub mod raw;
pub mod svg;

pub trait Draw {
    /// # Errors
    /// Returns an error when there was a problem with creating the output file.
    fn draw(&mut self, target: &Path, output: &Output) -> Result<(), std::io::Error> {
        self.begin();

        for item in &output.vec_rendered {
            match item {
                Rendered::Point(point) => self.draw_point(point),
                Rendered::Line(line) => self.draw_line(line),
                Rendered::Angle(angle) => self.draw_angle(angle, output),
                Rendered::Segment(segment) => self.draw_segment(segment),
                Rendered::Ray(ray) => self.draw_ray(ray),
                Rendered::Circle(circle) => self.draw_circle(circle),
            }
        }

        self.close_draw();
        let content = self.end();

        let file = File::create(target);
        
        match file {
            Ok(mut file) => file.write_all(content.as_bytes()),
            Err(error) => Err(error),
        }
    }

    fn begin(&mut self);
    fn draw_point(&mut self, point: &Rc<RenderedPoint>);
    fn draw_line(&mut self, line: &RenderedLine);
    fn draw_ray(&mut self, ray: &RenderedRay);
    fn draw_segment(&mut self, segment: &RenderedSegment);
    fn draw_angle(&mut self, angle: &RenderedAngle, output: &Output);
    fn draw_circle(&mut self, circle: &RenderedCircle);
    fn close_draw(&mut self);
    fn end(&self) -> &String;
}

pub struct Json {
    pub content: String,
    pub canvas_size: (usize, usize),
}

pub struct Latex {
    pub content: String,
    pub canvas_size: (usize, usize),
    pub scale: f64,
}

pub struct Raw {
    pub content: String,
    pub canvas_size: (usize, usize),
} 

pub struct Svg {
    pub content: String,
    pub canvas_size: (usize, usize),
}