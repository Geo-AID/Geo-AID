//! A higher-level API for `geo-aid-math`

use std::{
    cell::RefCell,
    fmt::Display,
    ops::{Add, AddAssign, Div, Mul, Neg, Sub},
    rc::Rc,
};

use crate::{Condition, Context, Expr, Float};

#[cfg(test)]
mod tests {
    use crate::shared::Complex;

    use super::Shared;

    #[test]
    fn test_exp() {
        let ctx = Shared::new(1);

        let expr = Complex::real(ctx.input(0)).exp();
        let f = ctx.exec(|ctx| ctx.compute([expr.real.expr, expr.imaginary.expr]));
        let mut buff = [0.0, 0.0];
        f.call(&[3.14], &mut buff);

        assert!((buff[0] - 3.14f64.exp()).abs() < 0.001);
        assert!((buff[1].abs()) < 0.001);
    }

    #[test]
    fn test_log() {
        let ctx = Shared::new(1);

        let expr = Complex::real(ctx.input(0)).log();
        let f = ctx.exec(|ctx| ctx.compute([expr.real.expr, expr.imaginary.expr]));
        let mut buff = [0.0, 0.0];
        f.call(&[3.14], &mut buff);

        assert!((buff[0] - 3.14f64.ln()).abs() < 0.001);
        assert!((buff[1].abs()) < 0.001);
    }

    #[test]
    fn test_sin() {
        let ctx = Shared::new(1);

        let expr = Complex::real(ctx.input(0).sin());
        let f = ctx.exec(|ctx| ctx.compute([expr.real.expr, expr.imaginary.expr]));
        let mut buff = [0.0, 0.0];
        f.call(&[4.12], &mut buff);

        assert!((buff[0] - 4.12f64.sin()).abs() < 0.001);
        assert!((buff[1].abs()) < 0.001);
    }
}

/// Access to a shared context.
#[derive(Clone, Debug)]
pub struct Shared(Rc<RefCell<Context>>);

impl Shared {
    /// Create a new shared context.
    #[must_use]
    pub fn new(inputs: usize) -> Self {
        Self(Rc::new(RefCell::new(Context::new(inputs))))
    }

    /// The i-th input.
    ///
    /// # Panics
    /// If the input is out of bounds
    pub fn input(&self, input: usize) -> Real {
        Real {
            ctx: self.clone(),
            expr: self.exec(|ctx| ctx.input(input)),
        }
    }

    /// Perform an action with the shared context.
    ///
    /// # Panics
    /// If nested inside another `do`.
    #[must_use]
    pub fn exec<T>(&self, f: impl FnOnce(&mut Context) -> T) -> T {
        f(&mut self.0.borrow_mut())
    }

    /// Create a new real with value 0.
    #[must_use]
    pub fn real_zero(&self) -> Real {
        Real {
            ctx: self.clone(),
            expr: Context::zero(),
        }
    }

    /// Create a new complex with value 0.
    #[must_use]
    pub fn complex_zero(&self) -> Complex {
        Complex {
            real: self.real_zero(),
            imaginary: self.real_zero(),
        }
    }

    /// Create a new real with value 1.
    #[must_use]
    pub fn real_one(&self) -> Real {
        Real {
            ctx: self.clone(),
            expr: Context::one(),
        }
    }

    /// Create a new complex with value 1.
    #[must_use]
    pub fn complex_one(&self) -> Complex {
        Complex {
            real: self.real_one(),
            imaginary: self.real_zero(),
        }
    }

    /// Create a new complex with value i.
    #[must_use]
    pub fn i(&self) -> Complex {
        Complex {
            real: self.real_zero(),
            imaginary: self.real_one(),
        }
    }

    /// Creates a constant value.
    #[must_use]
    pub fn constant(&self, value: Float) -> Real {
        Real {
            ctx: self.clone(),
            expr: self.exec(|ctx| ctx.constant(value)),
        }
    }

    /// A ternary expression
    #[must_use]
    pub fn ternary(&self, condition: Condition, then: Real, else_: Real) -> Real {
        Real {
            ctx: self.clone(),
            expr: self.exec(|ctx| ctx.ternary(condition, then.expr, else_.expr)),
        }
    }

    /// A ternary expression
    #[must_use]
    pub fn complex_ternary(&self, condition: Condition, then: Complex, else_: Complex) -> Complex {
        Complex {
            real: self.ternary(condition, then.real, else_.real),
            imaginary: self.ternary(condition, then.imaginary, else_.imaginary),
        }
    }
}

/// A real value.
#[derive(Clone, Debug)]
pub struct Real {
    /// Shared context
    pub ctx: Shared,
    /// The expression representing this value
    pub expr: Expr,
}

impl Real {
    /// Calculate an expression with the context
    #[must_use]
    pub fn exec(&self, f: impl FnOnce(&mut Context) -> Expr) -> Self {
        Self {
            ctx: self.ctx.clone(),
            expr: self.ctx.exec(f),
        }
    }

    /// Get the absolute value.
    #[must_use]
    pub fn abs(&self) -> Self {
        self.exec(|ctx| ctx.abs(self.expr))
    }

    /// Get the minimum
    #[must_use]
    pub fn min(&self, other: &Self) -> Self {
        self.exec(|ctx| ctx.min(self.expr, other.expr))
    }

    /// Raise to a power
    #[must_use]
    pub fn pow(&self, exponent: &Self) -> Self {
        (self.log() * exponent).exp()
    }

    /// Get the sine
    #[must_use]
    pub fn sin(&self) -> Self {
        self.exec(|ctx| ctx.sin(self.expr))
    }

    /// Get the cosine
    #[must_use]
    pub fn cos(&self) -> Self {
        self.exec(|ctx| ctx.cos(self.expr))
    }

    /// Arccosine
    #[must_use]
    pub fn acos(&self) -> Self {
        Complex::real(self.clone()).acos().real
    }

    /// Atan2 function
    #[must_use]
    pub fn atan2(y: &Self, x: &Self) -> Self {
        y.exec(|ctx| ctx.atan2(y.expr, x.expr))
    }

    /// Square root of this number
    #[must_use]
    pub fn sqrt(&self) -> Self {
        self.exec(|ctx| ctx.sqrt(self.expr))
    }

    /// Natural logarithm of this number
    #[must_use]
    pub fn log(&self) -> Self {
        self.exec(|ctx| ctx.log(self.expr))
    }

    /// e^this
    #[must_use]
    pub fn exp(&self) -> Self {
        self.exec(|ctx| ctx.exp(self.expr))
    }
}

impl Add<&Self> for Real {
    type Output = Self;

    fn add(self, rhs: &Self) -> Self::Output {
        self.exec(|ctx| ctx.add(self.expr, rhs.expr))
    }
}

impl AddAssign<&Self> for Real {
    fn add_assign(&mut self, rhs: &Self) {
        *self = self.clone() + rhs;
    }
}

impl Sub<&Self> for Real {
    type Output = Self;

    fn sub(self, rhs: &Self) -> Self::Output {
        self.exec(|ctx| ctx.sub(self.expr, rhs.expr))
    }
}

impl Mul<&Self> for Real {
    type Output = Self;

    fn mul(self, rhs: &Self) -> Self::Output {
        self.exec(|ctx| ctx.mul(self.expr, rhs.expr))
    }
}

impl Mul<&Complex> for Real {
    type Output = Complex;

    fn mul(self, rhs: &Complex) -> Self::Output {
        Complex {
            real: self.clone() * &rhs.real,
            imaginary: self.clone() * &rhs.imaginary,
        }
    }
}

impl Div<&Self> for Real {
    type Output = Self;

    fn div(self, rhs: &Self) -> Self::Output {
        self.exec(|ctx| ctx.div(self.expr, rhs.expr))
    }
}

impl Neg for Real {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.exec(|ctx| ctx.neg(self.expr))
    }
}

impl Display for Real {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ctx.0.borrow().stringify(self.expr))
    }
}

/// A complex value with a real and an imaginary part.
#[derive(Clone, Debug)]
pub struct Complex {
    /// The real part
    pub real: Real,
    /// The imaginary part
    pub imaginary: Real,
}

impl Complex {
    /// Get the norm (real^2 + imagainry^2)
    #[must_use]
    pub fn norm(&self) -> Real {
        self.real.clone() * &self.real + &(self.imaginary.clone() * &self.imaginary)
    }

    /// Create a real number
    #[must_use]
    pub fn real(re: Real) -> Self {
        let zero = re.ctx.real_zero();
        Self {
            real: re,
            imaginary: zero,
        }
    }

    /// Create a complex number from polar form.
    #[must_use]
    pub fn polar(theta: &Real, radius: &Real) -> Self {
        Self {
            real: theta.cos(),
            imaginary: theta.sin(),
        } * radius
    }

    /// Multiply this number by complex unit
    #[must_use]
    pub fn mul_i(&self) -> Self {
        Self {
            real: -self.imaginary.clone(),
            imaginary: self.real.clone(),
        }
    }

    /// Raise this number to a power
    #[must_use]
    pub fn pow(&self, exponent: &Self) -> Self {
        (self.log() * exponent).exp()
    }

    /// Natural logarithm of this number
    #[must_use]
    pub fn log(&self) -> Self {
        Self {
            real: self.abs().log(),
            imaginary: self.arg(),
        }
    }

    /// Modulus, square root of norm
    #[must_use]
    pub fn abs(&self) -> Real {
        self.norm().sqrt()
    }

    /// Argument of this complex
    #[must_use]
    pub fn arg(&self) -> Real {
        Real::atan2(&self.imaginary, &self.real)
    }

    /// e^this
    #[must_use]
    pub fn exp(&self) -> Self {
        Self::polar(&self.imaginary, &self.real.exp())
    }

    /// Square root
    #[must_use]
    pub fn sqrt(&self) -> Self {
        self.pow(&Self::real(self.real.ctx.constant(0.5)))
    }

    /// Sine
    #[must_use]
    pub fn sin(&self) -> Self {
        let i_theta = self.mul_i();
        (i_theta.exp() - &(-i_theta).exp()) / &(self.real.ctx.constant(2.0) * &self.real.ctx.i())
    }

    /// Cosine
    #[must_use]
    pub fn cos(&self) -> Self {
        let i_theta = self.mul_i();
        (i_theta.exp() + &(-i_theta).exp()) / &self.real.ctx.constant(2.0)
    }

    /// Arccosine
    #[must_use]
    pub fn acos(&self) -> Self {
        // -i ln(i sqrt(1-z^2) + z)
        ((self.real.ctx.complex_one() - &(self.clone() * self)).sqrt() * &self.real.ctx.i() + self)
            .log()
            * &self.real.ctx.i().neg()
    }
}

impl Add<&Self> for Complex {
    type Output = Self;

    fn add(self, rhs: &Self) -> Self::Output {
        Self {
            real: self.real + &rhs.real,
            imaginary: self.imaginary + &rhs.imaginary,
        }
    }
}

impl Add<&Real> for Complex {
    type Output = Self;

    fn add(self, rhs: &Real) -> Self::Output {
        Self {
            real: self.real + rhs,
            ..self
        }
    }
}

impl Sub<&Self> for Complex {
    type Output = Self;

    fn sub(self, rhs: &Self) -> Self::Output {
        Self {
            real: self.real - &rhs.real,
            imaginary: self.imaginary - &rhs.imaginary,
        }
    }
}

impl Mul<&Self> for Complex {
    type Output = Self;

    fn mul(self, rhs: &Self) -> Self::Output {
        Self {
            real: self.real.clone() * &rhs.real - &(self.imaginary.clone() * &rhs.imaginary),
            imaginary: self.real * &rhs.imaginary + &(self.imaginary * &rhs.real),
        }
    }
}

impl Mul<&Real> for Complex {
    type Output = Self;

    fn mul(self, rhs: &Real) -> Self::Output {
        Self {
            real: self.real * rhs,
            imaginary: self.imaginary * rhs,
        }
    }
}

impl Div<&Self> for Complex {
    type Output = Self;

    fn div(self, rhs: &Self) -> Self::Output {
        let denom = rhs.norm();

        Self {
            real: self.real.clone() * &rhs.real + &(self.imaginary.clone() * &rhs.imaginary),
            imaginary: self.imaginary * &rhs.real - &(self.real * &rhs.imaginary),
        } / &denom
    }
}

impl Div<&Real> for Complex {
    type Output = Self;

    fn div(self, rhs: &Real) -> Self::Output {
        Self {
            real: self.real / rhs,
            imaginary: self.imaginary / rhs,
        }
    }
}

impl Neg for Complex {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            real: -self.real,
            imaginary: -self.imaginary,
        }
    }
}
