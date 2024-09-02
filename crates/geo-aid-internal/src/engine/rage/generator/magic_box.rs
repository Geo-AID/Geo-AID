use std::f64::consts::PI;

use super::{AdjustableTemplate, Complex, State};

/// Performs an adjustment in a random direction.
///
/// # Arguments
/// * `current_state` - current values and errors of all inputs
/// * `matrix` - adjustment results are written to this thing.
/// * `adjustment_magnitude` - the magnitude to apply to the adjustment (how much of a jump to allow). Eta in the formula.
pub fn adjust(
    current_state: &mut State,
    adjustment_magnitude: f64,
    template: &[AdjustableTemplate],
) {
    let it = template
        .iter()
        .zip(current_state.qualities.iter().copied().map(|x| 1.0 - x));

    let mut index = 0;

    for (template, error) in it {
        match template {
            AdjustableTemplate::Point => {
                let direction = 2.0 * rand::random::<f64>() * PI;

                let unit = Complex::new(direction.cos(), direction.sin());
                let offset = unit * adjustment_magnitude * error;

                current_state.inputs[index] += offset.real;
                current_state.inputs[index + 1] += offset.imaginary;
                index += 2;
            }
            AdjustableTemplate::Real => {
                let direction = if rand::random::<u8>() & 1 == 0 {
                    1.0
                } else {
                    -1.0
                };

                // Adjust by a RELATIVE value based on quality and randomly chosen direction (+/-)
                let val = current_state.inputs[index];
                current_state.inputs[index] += direction * adjustment_magnitude * val * error;
                index += 1;
            }
            AdjustableTemplate::Clip1d => {
                let direction = if rand::random::<u8>() & 1 == 0 {
                    1.0
                } else {
                    -1.0
                };

                // Adjust by an ABSOLUTE value based on quality and randomly chosen direction (+/-)
                let val = current_state.inputs[index];
                current_state.inputs[index] = val + direction * adjustment_magnitude * error;
                index += 1;
            }
        }
    }
}
