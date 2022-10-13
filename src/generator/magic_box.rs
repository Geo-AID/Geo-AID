use std::f64::consts::PI;

use super::Complex;

/// Performs an adjustment in a random direction.
/// 
/// # Arguments
/// * `points` - a vector of complex numbers, all representing a point on a plane
/// * `quality` - a vector of qualities assigned to each point. Epsilon in the formula.
/// * `adjustment_magnitude` - the magnitude to apply to the adjustment (how much of a jump to allow). Eta in the formula.
pub fn adjust(points: &mut Vec<Complex>, quality: &Vec<f64>, adjustment_magnitude: f64) {
    for (i, point) in points.iter_mut().enumerate() {
        let direction = 2.0 * rand::random::<f64>() * PI;
        
        let unit = Complex::new(direction.cos(), direction.sin());
        let offset = unit * adjustment_magnitude * (1.0 - quality[i]);

        *point += offset;
    }
}