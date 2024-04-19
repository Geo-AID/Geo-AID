use std::{fmt::Display, mem};
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::ops::{Add, AddAssign, SubAssign};
use num_bigint::BigInt;
use num_complex::Complex;

use num_rational::{BigRational, Rational64};
use num_traits::{CheckedAdd, CheckedMul, FromPrimitive, One, ToPrimitive, Zero};
use crate::script::token::Number;

#[derive(Debug)]
pub struct ParseIntError;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// Unsigned integer for parsing
pub struct ParsedInt {
    digits: Vec<u8>,
}

impl ParsedInt {
    /// # Errors
    /// Returns an error if the value is too big to fit in the given type.
    pub fn parse<T: Zero + CheckedMul + CheckedAdd + FromPrimitive>(
        &self,
    ) -> Result<T, ParseIntError> {
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
            .fold(0.0, |v, item| v * 10.0 + item.to_f64().unwrap())
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
        write!(
            f,
            "{}",
            self.digits
                .iter()
                .map(ToString::to_string)
                .collect::<String>()
        )
    }
}

#[derive(Debug, Default)]
pub struct ParsedIntBuilder {
    digits: Vec<u8>,
}

impl ParsedIntBuilder {
    #[must_use]
    pub fn new() -> Self {
        Self { digits: Vec::new() }
    }

    pub fn push_digit(&mut self, digit: u8) {
        self.digits.push(digit);
    }

    #[must_use]
    pub fn build(self) -> ParsedInt {
        ParsedInt {
            digits: self.digits,
        }
    }

    #[must_use]
    pub fn dot(self) -> ParsedFloatBuilder {
        ParsedFloatBuilder {
            integral: self.build(),
            digits: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// Signed integer for parsing.
pub struct ParsedFloat {
    integral: ParsedInt,
    decimal: Vec<u8>,
}

impl ParsedFloat {
    /// # Panics
    /// Should not.
    #[must_use]
    pub fn to_float(&self) -> f64 {
        self.integral.to_float()
            + self
                .decimal
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
        write!(
            f,
            "{}.{}",
            self.integral,
            self.decimal
                .iter()
                .map(ToString::to_string)
                .collect::<String>()
        )
    }
}

#[derive(Debug)]
pub struct ParsedFloatBuilder {
    integral: ParsedInt,
    digits: Vec<u8>,
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
            decimal: digits,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedNumber {
    Int(ParsedInt),
    Float(ParsedFloat)
}

impl Display for ParsedNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsedNumber::Int(v) => write!(f, "{v}"),
            ParsedNumber::Float(v) => write!(f, "{v}")
        }
    }
}

/// Number for processing
#[derive(Debug, Clone)]
pub struct ProcNum(Complex<BigRational>);

impl SubAssign for ProcNum {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl AddAssign for ProcNum {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl FromPrimitive for ProcNum {
    fn from_i64(n: i64) -> Option<Self> {
        Some(Self(Complex::from_i64(n)?))
    }

    fn from_u64(n: u64) -> Option<Self> {
        Some(Self(Complex::from_u64(n)?))
    }

    fn from_f32(n: f32) -> Option<Self> {
        Some(Self(Complex::from_f32(n)?))
    }
}

impl Add<Self> for ProcNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Zero for ProcNum {
    fn zero() -> Self {
        Self(Complex::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl Display for ProcNum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq for ProcNum {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl Eq for ProcNum {}

impl PartialOrd for ProcNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ProcNum {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.0.re.cmp(&other.0.re) {
            Ordering::Equal => self.0.im.cmp(&other.0.im),
            ord => ord
        }
    }
}

impl From<&Number> for ProcNum {
    fn from(value: &Number) -> Self {
        let ten = Complex::from_u8(10).unwrap();

        match value {
            Number::Integer(i) => {
                let mut x: Complex<BigRational> = Complex::zero();

                for digit in &i.parsed.digits {
                    x *= ten;
                    x += Complex::from_u8(*digit).unwrap();
                }

                Self(x)
            }
            Number::Float(f) => {
                let mut integral: Complex<BigRational> = Complex::zero();
                let mut decimal: Complex<BigRational> = Complex::zero();
                let mut denominator = BigInt::one();

                for digit in &f.parsed.integral.digits {
                    integral *= ten;
                    integral += Complex::from_u8(*digit).unwrap();
                }

                for digit in &f.parsed.decimal {
                    denominator *= BigInt::from_u8(10).unwrap();
                    decimal *= ten;
                    decimal += Complex::from_u8(*digit).unwrap();
                }

                Self(integral + decimal)
            }
        }
    }
}

/// Number for computing
pub type CompFloat = f64;

/// Exponent type
pub type CompExponent = Rational64;
