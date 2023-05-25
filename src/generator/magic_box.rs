use std::f64::consts::PI;

use super::{Adjustables, Complex, WithQuality};

/// Performs an adjustment in a random direction.
///
/// # Arguments
/// * `points` - a vector of complex numbers, all representing a point on a plane
/// * `quality` - a vector of qualities assigned to each point. Epsilon in the formula.
/// * `adjustment_magnitude` - the magnitude to apply to the adjustment (how much of a jump to allow). Eta in the formula.
pub fn adjust(
    adjustables: Adjustables,
    adjustment_magnitude: f64,
) -> Adjustables {
    Adjustables {
        points: adjustables.points
            .into_iter()
            .map(|WithQuality {quality, value}| {
                let direction = 2.0 * rand::random::<f64>() * PI;

                    let unit = Complex::new(direction.cos(), direction.sin());
                    let offset = unit * adjustment_magnitude * (1.0 - quality);

                    WithQuality {
                        quality,
                        value: value + offset
                    }
            })
            .collect(),
        scalars: adjustables.scalars
            .into_iter()
            .map(|WithQuality {quality, value}| {
                let direction = if rand::random::<u8>() & 1 == 0 {
                    1.0
                } else {
                    -1.0
                };

                WithQuality {
                    quality,
                    // Adjust by a relative value based on quality and randomly chosen direction (+/-)
                    value: value + direction * adjustment_magnitude * value * (1.0 - quality)
                }
            })
            .collect(),
        circle_clips: adjustables.circle_clips
            .into_iter()
            .map(|WithQuality {quality, value}| {
                let direction = if rand::random::<u8>() & 1 == 0 {
                    1.0
                } else {
                    -1.0
                };

                WithQuality {
                    quality,
                    // Adjust by a relative value based on quality and randomly chosen direction (+/-)
                    value: value + direction * adjustment_magnitude * value * (1.0 - quality)
                }
            })
            .collect()
    }
}
