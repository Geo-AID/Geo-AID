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

use super::{Adjustable, Complex};

/// Performs an adjustment in a random direction.
///
/// # Arguments
/// * `points` - a vector of complex numbers, all representing a point on a plane
/// * `quality` - a vector of qualities assigned to each point. Epsilon in the formula.
/// * `adjustment_magnitude` - the magnitude to apply to the adjustment (how much of a jump to allow). Eta in the formula.
pub fn adjust(
    adjustables: Vec<(Adjustable, f64)>,
    adjustment_magnitude: f64,
) -> Vec<(Adjustable, f64)> {
    adjustables
        .into_iter()
        .map(|(adj, quality)| {
            match adj {
                Adjustable::Point(point) => {
                    let direction = 2.0 * rand::random::<f64>() * PI;

                    let unit = Complex::new(direction.cos(), direction.sin());
                    let offset = unit * adjustment_magnitude * (1.0 - quality);

                    (Adjustable::Point(point + offset), quality)
                }
                Adjustable::Real(val) => {
                    let direction = if rand::random::<u8>() & 1 == 0 {
                        1.0
                    } else {
                        -1.0
                    };

                    (
                        // Adjust by a relative value based on quality and randomly chosen direction (+/-)
                        Adjustable::Real(
                            val + direction * adjustment_magnitude * val * (1.0 - quality),
                        ),
                        quality,
                    )
                }
                Adjustable::Clip1D(val) => {
                    let direction = if rand::random::<u8>() & 1 == 0 {
                        1.0
                    } else {
                        -1.0
                    };

                    (
                        // Adjust by a relative value based on quality and randomly chosen direction (+/-)
                        Adjustable::Clip1D(
                            val + direction * adjustment_magnitude * (1.0 - quality),
                        ),
                        quality,
                    )
                }
            }
        })
        .collect()
}
