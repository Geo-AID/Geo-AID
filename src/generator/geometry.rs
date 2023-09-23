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

use crate::generator::Complex;

use super::expression::Line;

#[must_use]
pub fn get_line(p1: Complex, p2: Complex) -> Line {
    Line {
        origin: p1,
        direction: (p2 - p1).normalize(),
    }
}

/// Gets the intersection point of two lines.
///
/// # Errors
/// If the two lines are parallel, return an evaluation error. (Currently ignored)
pub fn get_intersection(l_ln: Line, k_ln: Line) -> Complex {
    let Line {
        origin: a,
        direction: b,
    } = l_ln;
    let Line {
        origin: c,
        direction: d,
    } = k_ln;

    a - b * ((a - c) / d).imaginary / (b / d).imaginary
}

/// Gets the angle between two arms and the origin
#[must_use]
pub fn get_angle(arm1: Complex, origin: Complex, arm2: Complex) -> f64 {
    // Get the vectors to calculate the angle between them.
    let arm1_vec = arm1 - origin;
    let arm2_vec = arm2 - origin;

    // Get the dot product
    let dot_product = arm1_vec.real * arm2_vec.real + arm1_vec.imaginary * arm2_vec.imaginary;

    // Get the argument
    f64::acos(dot_product / (arm1_vec.mangitude() * arm2_vec.mangitude()))
}

/// Gets the directed angle between two arms and the origin
#[must_use]
pub fn get_angle_directed(arm1: Complex, origin: Complex, arm2: Complex) -> f64 {
    // Get the vectors to calculate the angle between them.
    let arm1_vec = arm1 - origin;
    let arm2_vec = arm2 - origin;

    // decrease p2's angle by p1's angle:
    let p2_rotated = arm2_vec / arm1_vec;

    // Get the argument
    p2_rotated.arg()
}

// Rotates p around origin by angle.
#[must_use]
pub fn rotate_around(p: Complex, origin: Complex, angle: f64) -> Complex {
    (p - origin) * Complex::new(angle.cos(), angle.sin()) + origin
}
