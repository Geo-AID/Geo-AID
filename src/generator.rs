use std::ops::{Mul, Add, AddAssign};

mod magic_box;
mod critic;

/// The simplest geometrical construct used in Geo-AID.
/// X and Y coordinates are assumed to be on a "unit" plane.
#[derive(Debug, Clone, Copy)]
pub struct Complex {
    /// X coordinate located in range [0, 1).
    real: f64,
    /// Y coordinate located in range [0, 1).
    imaginary: f64
}

impl Complex {
    pub fn new(real: f64, imaginary: f64) -> Self { Self { real, imaginary } }
}

impl Mul<f64> for Complex {
    type Output = Complex;

    fn mul(self, rhs: f64) -> Self::Output {
        Complex::new(self.real * rhs, self.imaginary * rhs)
    }
}

impl Add<f64> for Complex {
    type Output = Complex;

    fn add(self, rhs: f64) -> Self::Output {
        Complex::new(self.real + rhs, self.imaginary)
    }
}

impl Add for Complex {
    type Output = Complex;

    fn add(self, rhs: Self) -> Self::Output {
        Complex::new(self.real + rhs.real, self.imaginary + rhs.imaginary)
    }
}

impl AddAssign for Complex {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}