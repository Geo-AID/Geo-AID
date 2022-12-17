use std::f64::consts::PI;

use super::Complex;

/// Performs an adjustment in a random direction.
///
/// # Arguments
/// * `points` - a vector of complex numbers, all representing a point on a plane
/// * `quality` - a vector of qualities assigned to each point. Epsilon in the formula.
/// * `adjustment_magnitude` - the magnitude to apply to the adjustment (how much of a jump to allow). Eta in the formula.
pub fn adjust(points: Vec<(Complex, f64)>, adjustment_magnitude: f64) -> Vec<(Complex, f64)> {
    points
        .into_iter()
        .map(|point| {
            let direction = 2.0 * rand::random::<f64>() * PI;

            let unit = Complex::new(direction.cos(), direction.sin());
            let offset = unit * adjustment_magnitude * (1.0 - point.1);

            (point.0 + offset, point.1)
        })
        .collect()
}
