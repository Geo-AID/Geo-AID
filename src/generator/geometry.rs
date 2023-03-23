use crate::generator::Complex;

use super::{EvaluationError, expression::Line};

#[must_use]
pub fn get_line(p1: Complex, p2: Complex) -> Line {
    Line {
        origin: p1,
        direction: (p2 - p1).normalize()
    }
}

/// Gets the intersection point of two lines.
///
/// # Errors
/// If the two lines are parallel, return an evaluation error. (Currently ignored)
pub fn get_intersection(l_ln: Line, k_ln: Line) -> Result<Complex, EvaluationError> {
    // For now: ignore parallelity.

    /* OLD, INVALID VERSION
    // First, make l's origin relative to k's origin.
    let l_origin = l.origin - k.origin;

    // Next, rotate the system so that k becomes the x-axis. k's origin is (0, 0) is our system, we don't have to rotate it.
    // The exact rotation can be performed by dividing by the line's direction vector. We have to divide both l's origin
    // and l's direction vector.
    let l_origin = l_origin / k.direction;
    let l_dir = l.direction / k.direction;

    // Now, we simply get the intersection of rotated l with rotated k (the x-axis).
    // Let L be the origin of l, P the intersection point and L' a point on x-axis so that LL'P is right.
    // Let v be the direction vector of l, m the magnitude of that vector and d the (directed) distance LP.
    // Let h also be the directed distance LL'.
    // Then we have: P = L + v * d / m. And based on similarity d / m = h / y.
    // Thus P = L + v * h / y.
    let intersection = l_origin + l_dir * l_origin.imaginary / l_dir.imaginary;

    // Now we have to rotate the intersection back and offset it by k.origin.
    println!("{intersection}, {}", k.direction);
    let intersection = intersection * k.direction + k.origin;
    println!("{intersection}");
    */

    let Line { origin: a, direction: b } = l_ln;
    let Line { origin: c, direction: d } = k_ln;

    let intersection = a - b * ((a - c) / d).imaginary / (b / d).imaginary;
    
    Ok(intersection)
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
    f64::acos(dot_product / (arm1_vec.mangitude()  * arm2_vec.mangitude()))
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
