use std::{mem, fmt::Display};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// Unsigned integer for parsing
pub struct ParsedInt {
    digits: Vec<u8>
}

impl ParsedInt {
    pub fn add_digit(&mut self, digit: u8) {
        if !self.digits.is_empty() || digit != 0 {
            self.digits.push(digit);
        }
    }
}

impl Display for ParsedInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.digits.into_iter().map(|d| d.to_string()).collect::<String>())
    }
}

#[derive(Debug, Default)]
pub struct ParsedIntBuilder {
    digits: Vec<u8>
}

impl ParsedIntBuilder {
    pub fn new() -> Self {
        Self {
            digits: Vec::new()
        }
    }

    pub fn add_digit(&mut self, digit: u8) {
        if !self.digits.is_empty() || digit != 0 {
            self.digits.push(digit);
        }
    }

    pub fn build(self) -> ParsedInt {
        ParsedInt {
            digits: self.digits
        }
    }

    pub fn dot(self) -> ParsedFloatBuilder {
        ParsedFloatBuilder {
            integral: self.build(),
            digits: Vec::new()
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// Signed integer for parsing.
pub struct ParsedFloat {
    pub integral: ParsedInt,
    pub decimal: Vec<u8>
}

impl Display for ParsedFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.integral, self.decimal.into_iter().map(|d| d.to_string()).collect::<String>())
    }
}

#[derive(Debug)]
pub struct ParsedFloatBuilder {
    integral: ParsedInt,
    digits: Vec<u8>
}

impl ParsedFloatBuilder {
    pub fn add_digit(&mut self, digit: u8) {
        self.digits.push(digit);
    }

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

/// Integer for computing
pub type CompUint = i64;

/// Integer for computing
pub type CompInteger = i64;

/// Number for computing
pub type CompNumber = f64;