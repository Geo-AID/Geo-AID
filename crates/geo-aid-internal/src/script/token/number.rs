use num_bigint::BigInt;
use num_complex::Complex;
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, SubAssign};
use std::{fmt::Display, mem};

use crate::geometry;
use crate::script::token::Number;
use num_rational::{BigRational, Rational64};
use num_traits::{CheckedAdd, CheckedMul, FromPrimitive, One, ToPrimitive, Zero};
use serde::Serialize;

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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
pub enum Parsed {
    Int(ParsedInt),
    Float(ParsedFloat),
}

impl Display for Parsed {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Parsed::Int(v) => write!(f, "{v}"),
            Parsed::Float(v) => write!(f, "{v}"),
        }
    }
}

/// Number for processing
#[derive(Debug, Clone, Hash, Serialize, PartialEq, Eq)]
pub struct ProcNum(Complex<BigRational>);

impl ProcNum {
    /// # Panics
    /// A panic is a bug
    #[must_use]
    pub fn to_complex(&self) -> geometry::Complex {
        geometry::Complex::new(self.0.re.to_f64().unwrap(), self.0.im.to_f64().unwrap())
    }

    /// # Panics
    /// A panic is a bug.
    #[must_use]
    pub fn pi() -> Self {
        Self(Complex::new(
            BigRational::from_f64(std::f64::consts::PI).unwrap(),
            BigRational::zero(),
        ))
    }
}

impl FromPrimitive for ProcNum {
    fn from_i64(n: i64) -> Option<Self> {
        Complex::from_i64(n).map(Self)
    }

    fn from_u64(n: u64) -> Option<Self> {
        Complex::from_u64(n).map(Self)
    }

    fn from_f32(n: f32) -> Option<Self> {
        Complex::from_f32(n).map(Self)
    }

    fn from_f64(n: f64) -> Option<Self> {
        Complex::from_f64(n).map(Self)
    }
}

impl SubAssign<&Self> for ProcNum {
    fn sub_assign(&mut self, rhs: &Self) {
        self.0 -= &rhs.0;
    }
}

impl AddAssign<&Self> for ProcNum {
    fn add_assign(&mut self, rhs: &Self) {
        self.0 += &rhs.0;
    }
}

impl Add for ProcNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Add<&Self> for ProcNum {
    type Output = Self;

    fn add(self, rhs: &Self) -> Self::Output {
        Self(self.0 + &rhs.0)
    }
}

impl Mul for ProcNum {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl Mul<&Self> for ProcNum {
    type Output = Self;

    fn mul(self, rhs: &Self) -> Self::Output {
        Self(self.0 * &rhs.0)
    }
}

impl MulAssign<&Self> for ProcNum {
    fn mul_assign(&mut self, rhs: &Self) {
        self.0 *= &rhs.0;
    }
}

impl Div<&Self> for ProcNum {
    type Output = Self;

    fn div(self, rhs: &Self) -> Self::Output {
        Self(self.0 / &rhs.0)
    }
}

impl DivAssign<&Self> for ProcNum {
    fn div_assign(&mut self, rhs: &Self) {
        self.0 /= &rhs.0;
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

impl One for ProcNum {
    fn one() -> Self {
        Self(Complex::one())
    }

    fn is_one(&self) -> bool
    where
        Self: PartialEq,
    {
        self.0.is_one()
    }
}

impl Display for ProcNum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialOrd for ProcNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ProcNum {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.0.re.cmp(&other.0.re) {
            Ordering::Equal => self.0.im.cmp(&other.0.im),
            ord => ord,
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
                    x *= &ten;
                    x += Complex::from_u8(*digit).unwrap();
                }

                Self(x)
            }
            Number::Float(f) => {
                let mut integral: Complex<BigRational> = Complex::zero();
                let mut decimal: Complex<BigRational> = Complex::zero();
                let mut denominator = BigInt::one();

                for digit in &f.parsed.integral.digits {
                    integral *= &ten;
                    integral += Complex::from_u8(*digit).unwrap();
                }

                for digit in &f.parsed.decimal {
                    denominator *= BigInt::from_u8(10).unwrap();
                    decimal *= &ten;
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
