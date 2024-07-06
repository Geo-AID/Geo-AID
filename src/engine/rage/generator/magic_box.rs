use std::f64::consts::PI;

use super::{Adjustable, Complex, State, program::Value};

/// Performs an adjustment in a random direction.
///
/// # Arguments
/// * `current_state` - current values and qualities of all adjustables
/// * `matrix` - adjustment results are written to this thing.
/// * `adjustment_magnitude` - the magnitude to apply to the adjustment (how much of a jump to allow). Eta in the formula.
pub fn adjust(
    current_state: &State,
    matrix: &mut [Value],
    adjustment_magnitude: f64,
) {
    let it = current_state.adjustables.iter().copied()
        .zip(current_state.qualities.iter().copied())
        .zip(matrix);

    for ((origin, quality), matrix) in it {
        match origin {
            Adjustable::Point(point) => {
                let direction = 2.0 * rand::random::<f64>() * PI;

                let unit = Complex::new(direction.cos(), direction.sin());
                let offset = unit * adjustment_magnitude * (1.0 - quality);

                matrix.complex = point + offset;
            }
            Adjustable::Real(val) => {
                let direction = if rand::random::<u8>() & 1 == 0 {
                    1.0
                } else {
                    -1.0
                };

                // Adjust by a relative value based on quality and randomly chosen direction (+/-)
                matrix.complex = Complex::real(val + direction * adjustment_magnitude * val * (1.0 - quality));
            }
            Adjustable::Clip1D(val) => {
                let direction = if rand::random::<u8>() & 1 == 0 {
                    1.0
                } else {
                    -1.0
                };

                // Adjust by an absolute value based on quality and randomly chosen direction (+/-)
                matrix.complex = Complex::real(val + direction * adjustment_magnitude * (1.0 - quality));
            }
        }
    }
}
