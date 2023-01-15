use crate::generator::Complex;

use super::EvaluationError;

#[cfg(test)]
mod tests {
    use crate::generator::Complex;

    use super::{get_angle, get_line, rotate_around};

    #[test]
    fn bisector() {
        let p3 = Complex::new(1.0, 1.0);
        let p2 = Complex::new(0.0, 0.0);
        let p1 = Complex::new(-1.0, 1.0);

        let angle = get_angle(p1, p2, p3) / 2.0;

        let line = get_line(p2, rotate_around(p1, p2, angle));

        // println!("a{} + {}, {angle}, {}", line.real, line.imaginary, rotate_around(p3, p2, angle));
        assert!(line.real.is_infinite());
        assert!(f64::abs(line.imaginary - 0.0) < 0.01);
    }
}

#[must_use]
pub fn get_line(p1: Complex, p2: Complex) -> Complex {
    let slope = if f64::abs(p1.real - p2.real) < 1e-6 {
        // Assumed to be vertical, intercept is offset
        return Complex::new(f64::INFINITY, p1.real);
    } else {
        (p1.imaginary - p2.imaginary) / (p1.real - p2.real)
    };

    let intercept = p1.imaginary - p1.real * slope;
    // logger.push(format!("The line is {slope}x + {intercept}."));
    Complex::new(slope, intercept)
}

/// Gets the intersection point of two lines.
///
/// # Errors
/// If the two lines are parallel, return an evaluation error.
pub fn get_crossing(l1: Complex, l2: Complex) -> Result<Complex, EvaluationError> {
    // Further on we assume either only l1 is vertical or both are.
    let (l1, l2) = if l2.real.is_infinite() {
        (l2, l1)
    } else {
        (l1, l2)
    };

    let x = if l1.real.is_infinite() {
        // l1 is vertical
        if l2.real.is_infinite() {
            // l2 is also vertical, the two lines are parallel
            return Err(EvaluationError::ParallelLines);
        }

        // If l2 is not vertical, l1's offset is the crossing point.
        l1.imaginary
    } else {
        // None are vertical
        (l2.imaginary - l1.imaginary) / (l1.real - l2.real)
    };

    // Calculate the y based on the second line, as it's now guaranteed not to be vertical
    let y = l2.real * x + l2.imaginary;

    Ok(Complex::new(x, y))
}

/// Gets the angle between two arms and the origin
#[must_use]
pub fn get_angle(arm1: Complex, origin: Complex, arm2: Complex) -> f64 {
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
