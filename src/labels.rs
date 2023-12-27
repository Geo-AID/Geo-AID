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

use std::f64::consts::PI;

use crate::{
    generator::{
        geometry::{distance_pt_ln, distance_pt_pt, get_line, rotate_around},
        Complex,
    },
    projector::{RenderedAngle, RenderedCircle, RenderedLine, RenderedRay, RenderedSegment},
};

/// Function that outputs the position of the label. The position is located on the bisector of the given angle.
/// ///
/// # Panics
/// Panics when the angle is equal to 0, which shouldn't happen.  
fn label_position(vec: Complex, vec_next: Complex, angle: f64, point: Complex) -> Complex {
    // We get the the bisector angle.
    let bisector_angle = ((vec_next.arg() - vec.arg()).rem_euclid(2.0 * PI)) / 2.0 + vec.arg();

    // This is just the standard complex number formula.
    let bisector_vec = Complex::new(bisector_angle.cos(), bisector_angle.sin()).normalize();

    // to do -> better scaling
    let scale = 540.0 / angle.to_degrees().abs();

    Complex::new(
        point.real + (bisector_vec.real * 3.0 * scale),
        point.imaginary + (bisector_vec.imaginary * 3.0 * scale),
    )
}

/// Function that gets the label position.
fn get_label_pos(vec_associated: &Vec<Complex>, point: Complex) -> Complex {
    let mut vec_iter = vec_associated.iter();
    vec_iter.next();

    let mut biggest_angle = 0.0;
    let mut label_cords = (Complex::default(), Complex::default());

    let label_pos: Complex;

    if vec_associated.is_empty() {
        label_pos = Complex::new(point.real + 2.0, point.imaginary + 2.0);
    } else if vec_associated.len() == 1 {
        label_pos = point - 2.0 * *vec_associated.first().unwrap();
    } else {
        for vec in vec_associated {
            if let Some(vec_next) = vec_iter.next() {
                let angle = vec_next.arg() - vec.arg();
                if angle > biggest_angle {
                    biggest_angle = angle;
                    label_cords = (*vec, *vec_next);
                }
            } else {
                let first = vec_associated.first().unwrap();
                let last = vec_associated.last().unwrap();
                let angle = 2.0 * PI - (first.arg().abs() + last.arg().abs());
                if angle > biggest_angle {
                    biggest_angle = angle;
                    label_cords = (*last, *first);
                }
                break;
            }
        }
        label_pos = label_position(label_cords.0, label_cords.1, biggest_angle, point);
    }

    label_pos
}

/// Function that output the position of the label.
/// ///
/// # Panics
///
#[must_use]
pub fn point_label_position(
    lines: &Vec<RenderedLine>,
    angles: &Vec<RenderedAngle>,
    segments: &Vec<RenderedSegment>,
    rays: &Vec<RenderedRay>,
    circles: &Vec<RenderedCircle>,
    mut vec_associated: Vec<Complex>,
    point: Complex,
) -> Complex {
    // Checking the lines for associated vectors.
    for ln in lines {
        let ln = get_line(ln.points.0, ln.points.1);

        if distance_pt_ln(point, ln) < 1e-2 {
            vec_associated.push(ln.direction);
            vec_associated.push(-ln.direction);
        }
    }

    for _angle in angles {
        // to do
    }

    for segment in segments {
        let seg1;
        let seg2;

        if segment.points.0.real < segment.points.1.real {
            seg1 = segment.points.0;
            seg2 = segment.points.1;
        } else {
            seg1 = segment.points.1;
            seg2 = segment.points.0;
        }

        let ln = get_line(seg1, seg2);
        let distance = distance_pt_ln(point, ln);

        let seg1_mov = seg1 - ln.direction * distance;

        let seg2_mov = seg2 + ln.direction * distance;

        let a = ln.direction.real * ln.direction.imaginary;

        let unit = ln.direction * 1e-2;
        let u1 = unit.real;
        let u2 = unit.imaginary;

        if distance < 1e-2 {
            if a > 0.0 {
                if seg1_mov.real - u1 < point.real
                    && point.real < seg2_mov.real + u1
                    && seg1_mov.imaginary - u2 < point.imaginary
                    && point.imaginary < seg2_mov.imaginary + u2
                {
                    if distance_pt_pt(point, seg1) < 1.0 {
                        vec_associated.push(ln.direction);
                    } else if distance_pt_pt(point, seg2) < 1.0 {
                        vec_associated.push(-ln.direction);
                    } else {
                        vec_associated.push(ln.direction);
                        vec_associated.push(-ln.direction);
                    }
                }
            } else if a < 0.0
                && seg1_mov.real - u1 < point.real
                && point.real < seg2_mov.real + u1
                && seg1_mov.imaginary - u2 > point.imaginary
                && point.imaginary > seg2_mov.imaginary + u2
            {
                if distance_pt_pt(point, seg1) < 1.0 {
                    vec_associated.push(ln.direction);
                } else if distance_pt_pt(point, seg2) < 1.0 {
                    vec_associated.push(-ln.direction);
                } else {
                    vec_associated.push(ln.direction);
                    vec_associated.push(-ln.direction);
                }
            }
        }
    }

    for ray in rays {
        let frame_point = ray.points.1;

        let ray_point = if distance_pt_pt(ray.points.0, frame_point)
            > distance_pt_pt(ray.draw_point, frame_point)
        {
            ray.points.0
        } else {
            ray.draw_point
        };

        let ln = get_line(ray_point, frame_point);
        let a = ln.direction.real * ln.direction.imaginary;

        let unit = ln.direction * 1e-2;
        let u1 = unit.real;
        let u2 = unit.imaginary;

        let b1 = ray_point.real > frame_point.real
            && frame_point.real < point.real
            && point.real < ray_point.real - u1
            && frame_point.imaginary < point.imaginary
            && point.imaginary < ray_point.imaginary - u2;
        let b2 = frame_point.real > ray_point.real
            && ray_point.real - u1 < point.real
            && point.real < frame_point.real
            && ray_point.imaginary - u2 < point.imaginary
            && point.imaginary < frame_point.imaginary;

        let b3 = frame_point.real > ray_point.real
            && ray_point.real - u1 < point.real
            && point.real < frame_point.real
            && point.imaginary < ray_point.imaginary - u2
            && frame_point.imaginary < point.imaginary;
        let b4 = ray_point.real > frame_point.real
            && frame_point.real < point.real
            && point.real < ray_point.real - u1
            && point.imaginary < frame_point.imaginary
            && ray_point.imaginary - u2 < point.imaginary;

        if distance_pt_ln(point, ln) < 1e-2 {
            if a > 0.0 {
                if b1 || b2 {
                    if distance_pt_pt(point, ray_point) < 1.0 {
                        vec_associated.push(ln.direction);
                    } else {
                        vec_associated.push(ln.direction);
                        vec_associated.push(-ln.direction);
                    }
                }
            } else if a < 0.0 && (b3 || b4) {
                if distance_pt_pt(ray_point, point) < 1.0 {
                    vec_associated.push(ln.direction);
                } else {
                    vec_associated.push(ln.direction);
                    vec_associated.push(-ln.direction);
                }
            }
        }
    }

    for circle in circles {
        if (distance_pt_pt(circle.center, point) - circle.radius).abs() < 1e-4 {
            let line = get_line(point, rotate_around(circle.center, point, 90.0));

            vec_associated.push(line.direction);
            vec_associated.push(-line.direction);
        }
    }

    vec_associated.sort_by(|a, b| (a.arg()).partial_cmp(&b.arg()).unwrap());

    get_label_pos(&vec_associated, point)
}
