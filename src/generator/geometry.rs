use crate::generator::Complex;

use super::EvaluationError;

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
