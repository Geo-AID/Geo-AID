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

use serde::Serialize;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Div, Mul, MulAssign};

use crate::script::Error;
use crate::script::parser::{FromProperty, PropertyValue, Parse};

/// A floating point value for optimized operations with often use of values 1 and 0.
#[derive(Debug, Clone, Copy, Serialize, Default)]
pub enum FastFloat {
    #[default]
    Zero,
    One,
    Other(f64),
}

impl FastFloat {
    /// Returns `true` if is [`FastFloat::Zero`]. `false` otherwise.
    #[must_use]
    pub const fn is_zero(&self) -> bool {
        matches!(self, Self::Zero)
    }

    /// Converts the value to a standard `f64`.
    #[must_use]
    pub const fn to_f64(self) -> f64 {
        match self {
            Self::One => 1.0,
            Self::Zero => 0.0,
            Self::Other(v) => v,
        }
    }
}

impl Add for FastFloat {
    type Output = FastFloat;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Zero => rhs,
            Self::One => match rhs {
                Self::Zero => Self::One,
                Self::One => Self::Other(2.0),
                Self::Other(v) => Self::Other(1.0 + v),
            },
            Self::Other(v) => match rhs {
                Self::Zero => self,
                Self::One => Self::Other(1.0 + v),
                Self::Other(u) => Self::Other(u + v),
            },
        }
    }
}

impl AddAssign for FastFloat {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Mul for FastFloat {
    type Output = FastFloat;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Self::Zero => Self::Zero,
            Self::One => rhs,
            Self::Other(v) => match rhs {
                Self::Zero => Self::Zero,
                Self::One => self,
                Self::Other(u) => Self::Other(v * u),
            },
        }
    }
}

impl MulAssign for FastFloat {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl Div for FastFloat {
    type Output = FastFloat;

    fn div(self, rhs: Self) -> Self::Output {
        match rhs {
            Self::Zero => Self::Other(f64::INFINITY),
            Self::One => self,
            Self::Other(v) => match self {
                Self::Zero => Self::Zero,
                Self::One => Self::Other(1.0 / v),
                Self::Other(u) => Self::Other(u / v),
            },
        }
    }
}

impl Sum<Self> for FastFloat {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut s = FastFloat::Zero;

        for v in iter {
            s += v;
        }

        s
    }
}

impl FromProperty for FastFloat {
    fn from_property(property: PropertyValue) -> Result<Self, Error> {
        match property {
            PropertyValue::Number(num) => {
                if num.is_zero() {
                    Ok(FastFloat::Zero)
                } else if num.is_one() {
                    Ok(FastFloat::One)
                } else {
                    Ok(FastFloat::Other(num.to_float()))
                }
            }
            PropertyValue::Ident(_)
            | PropertyValue::RawString(_)
            | PropertyValue::String(_) => Err(Error::NumberExpected { error_span: property.get_span() }),
        }
    }
}
