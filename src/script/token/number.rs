use std::{mem, fmt::Display};

use num_traits::{Zero, CheckedMul, CheckedAdd, FromPrimitive, ToPrimitive};

#[derive(Debug)]
pub struct ParseIntError;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// Unsigned integer for parsing
pub struct ParsedInt {
    digits: Vec<u8>
}

impl ParsedInt {
    /// # Errors
    /// Returns an error if the value is too big to fit in the given type.
    pub fn parse<T: Zero + CheckedMul + CheckedAdd + FromPrimitive>(&self) -> Result<T, ParseIntError> {
        let ten = T::from_u8(10).ok_or(ParseIntError)?;

        self.digits
            .iter()
            .copied()
            .try_fold(T::zero(), |v, item| {
                let item = T::from_u8(item)?;
                v.checked_mul(&ten).and_then(|x| x.checked_add(&item))
            })
            .ok_or(ParseIntError)
    }

    /// # Panics
    /// Should not.
    #[must_use]
    pub fn to_float(&self) -> f64 {
        self.digits
            .iter()
            .copied()
            .fold(0.0, |v, item| {
                v * 10.0 + item.to_f64().unwrap()
            })
    }

    #[must_use]
    pub fn is_zero(&self) -> bool {
        self.digits[0] == 0
    }

    #[must_use]
    pub fn is_one(&self) -> bool {
        self.digits[0] == 1 && self.digits.len() == 1
    }
}

impl Display for ParsedInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.digits.iter().map(ToString::to_string).collect::<String>())
    }
}

#[derive(Debug, Default)]
pub struct ParsedIntBuilder {
    digits: Vec<u8>
}

impl ParsedIntBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self {
            digits: Vec::new()
        }
    }

    pub fn push_digit(&mut self, digit: u8) {
        self.digits.push(digit);
    }

    #[must_use]
    pub fn build(self) -> ParsedInt {
        ParsedInt {
            digits: self.digits
        }
    }

    #[must_use]
    pub fn dot(self) -> ParsedFloatBuilder {
        ParsedFloatBuilder {
            integral: self.build(),
            digits: Vec::new()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// Signed integer for parsing.
pub struct ParsedFloat {
    integral: ParsedInt,
    decimal: Vec<u8>
}

impl ParsedFloat {
    /// # Panics
    /// Should not.
    #[must_use]
    pub fn to_float(&self) -> f64 {
        self.integral.to_float() + self.decimal
            .iter()
            .copied()
            .enumerate()
            .fold(0.0, |v, (i, item)| {
                v + item.to_f64().unwrap() * f64::powi(10.0, -(i.to_i32().unwrap() + 1))
            })
    }

    #[must_use]
    pub fn is_zero(&self) -> bool {
        self.integral.is_zero() && self.decimal.is_empty()
    }

    #[must_use]
    pub fn is_one(&self) -> bool {
        self.integral.is_one() && self.decimal.is_empty()
    }
}

impl Display for ParsedFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.integral, self.decimal.iter().map(ToString::to_string).collect::<String>())
    }
}

#[derive(Debug)]
pub struct ParsedFloatBuilder {
    integral: ParsedInt,
    digits: Vec<u8>
}

impl ParsedFloatBuilder {
    pub fn push_digit(&mut self, digit: u8) {
        self.digits.push(digit);
    }

    #[must_use]
    pub fn build(self) -> ParsedFloat {
        let mut digits = Vec::new();
        let mut segment = Vec::new();

        for d in self.digits {
            if d == 0 {
                segment.push(d);
            } else {
                let taken = mem::take(&mut segment);
                digits.extend(taken);
                digits.push(d);
            }
        }

        ParsedFloat {
            integral: self.integral,
            decimal: digits
        }
    }
}

/// Number for computing
pub type CompFloat = f64;