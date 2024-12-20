mod compiler;
pub mod shared;

/// A feature-specific floating point representation.
#[cfg(feature = "f64")]
pub type Float = f64;
#[cfg(not(feature = "f64"))]
pub type Float = f32;

/// A compiled expression.
#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Expr(usize);

/// A comparison between two expressions.
#[derive(Debug, Clone, Copy)]
pub struct Comparison {
    pub a: Expr,
    pub b: Expr,
    pub kind: ComparisonKind,
}

/// The primitive kind of a comparison.
#[derive(Debug, Clone, Copy)]
pub enum ComparisonKind {
    Eq,
    Neq,
    Gt,
    Gteq,
}

/// A condition for ternary operators
#[derive(Debug, Clone, Copy)]
pub enum Condition {
    Comparison(Comparison),
}

/// The kind of an expression. Internal use only.
#[derive(Debug, Clone, Copy)]
enum ExprKind {
    /// A constant number
    Constant(Float),
    /// a + b
    Add(Expr, Expr),
    /// a - b
    Sub(Expr, Expr),
    /// a * b
    Mul(Expr, Expr),
    /// a / b
    Div(Expr, Expr),
    /// An input at an index
    Input(usize),
    /// sine of a value
    Sin(Expr),
    /// cosine of a value
    Cos(Expr),
    /// atan2 function
    Atan2(Expr, Expr),
    /// -expr
    Neg(Expr),
    /// If condition is true, returns first expression. Otherwise returns the second one.
    Ternary(Condition, Expr, Expr),
    /// Square root
    Sqrt(Expr),
    /// e^expr
    Exp(Expr),
    /// Natural logarithm
    Log(Expr),
}

/// An entry in the expression record.
#[derive(Debug, Clone, Copy)]
struct Entry {
    /// Kind of this expression
    kind: ExprKind,
    /// The index of derivatives of this expression to the `derivatives` vector.
    derivatives: Option<usize>,
}

/// Compilation context. Necessary for any expression manipulation and compilation.
#[derive(Debug, Clone)]
pub struct Context {
    /// Input count.
    inputs: usize,
    /// Expression vector.
    exprs: Vec<Entry>,
    /// Derivatives w.r.t. respective inputs. Every `inputs` next entries are a set
    /// of derivatives.
    derivatives: Vec<Expr>,
}

impl Context {
    /// Create a new context prepared to handle a given amount of inputs.
    #[must_use]
    pub fn new(inputs: usize) -> Self {
        let mut exprs = Vec::new();
        let mut derivatives = Vec::new();

        // Due to it being potentially useful and popular, zero is added automatically.
        exprs.push(Entry {
            kind: ExprKind::Constant(0.0),
            derivatives: Some(0),
        });
        derivatives.extend([Expr(0)].repeat(inputs));

        // For inputs, we also include a one.
        exprs.push(Entry {
            kind: ExprKind::Constant(1.0),
            derivatives: Some(0),
        });

        // For the same reason. Input exprs is also included.
        for i in 0..inputs {
            exprs.push(Entry {
                kind: ExprKind::Input(i),
                derivatives: Some(derivatives.len()),
            });
            derivatives.extend((0..inputs).map(|j| if i == j { Expr(1) } else { Expr(0) }));
        }

        Self {
            inputs,
            exprs,
            derivatives,
        }
    }

    /// Print the expression as a string. Use only for debugging, brace for long output.
    pub fn stringify(&self, expr: Expr) -> String {
        self.stringify_kind(self.exprs[expr.0].kind)
    }

    /// Helper for the [`stringify`] function
    fn stringify_kind(&self, expr_kind: ExprKind) -> String {
        match expr_kind {
            ExprKind::Constant(v) => format!("{v:.2}"),
            ExprKind::Add(a, b) => {
                format!("({} + {})", self.stringify(a), self.stringify(b))
            }
            ExprKind::Sub(a, b) => {
                format!("({} - {})", self.stringify(a), self.stringify(b))
            }
            ExprKind::Mul(a, b) => {
                format!("({} * {})", self.stringify(a), self.stringify(b))
            }
            ExprKind::Div(a, b) => {
                format!("({} / {})", self.stringify(a), self.stringify(b))
            }
            ExprKind::Input(i) => format!("#{i}"),
            ExprKind::Sin(v) => format!("sin({})", self.stringify(v)),
            ExprKind::Cos(v) => format!("cos({})", self.stringify(v)),
            ExprKind::Atan2(y, x) => format!("atan2({}, {})", self.stringify(y), self.stringify(x)),
            ExprKind::Neg(v) => format!("-{}", self.stringify(v)),
            ExprKind::Ternary(cond, then, else_) => format!(
                "({} ? {} : {})",
                self.stringify_condition(cond),
                self.stringify(then),
                self.stringify(else_)
            ),
            ExprKind::Sqrt(v) => format!("sqrt({})", self.stringify(v)),
            ExprKind::Exp(v) => format!("e^{}", self.stringify(v)),
            ExprKind::Log(v) => format!("ln({})", self.stringify(v)),
        }
    }

    /// Helper for the [`stringify`] function.
    fn stringify_condition(&self, condition: Condition) -> String {
        match condition {
            Condition::Comparison(cmp) => {
                let a = self.stringify(cmp.a);
                let b = self.stringify(cmp.b);
                let sign = match cmp.kind {
                    ComparisonKind::Eq => "=",
                    ComparisonKind::Neq => "!=",
                    ComparisonKind::Gt => ">",
                    ComparisonKind::Gteq => "≥",
                };

                format!("{a} {sign} {b}")
            }
        }
    }

    /// Push an expression without derivatives.
    fn push_expr_nodiff(&mut self, kind: ExprKind) -> Expr {
        let id = self.exprs.len();
        self.exprs.push(Entry {
            kind,
            derivatives: None,
        });
        Expr(id)
    }

    /// Push an expression with its derivatives.
    fn push_expr(&mut self, kind: ExprKind, derivatives: Vec<Expr>) -> Expr {
        assert_eq!(self.inputs, derivatives.len());
        let id = self.exprs.len();
        self.exprs.push(Entry {
            kind,
            derivatives: Some(self.derivatives.len()),
        });
        self.derivatives.extend(derivatives);
        Expr(id)
    }

    /// Get the derivative of `expr` w.r.t. `input`
    ///
    /// # Panics
    /// If the expression is not a user-defined one (does not have a derivative).
    fn get_derivative(&self, expr: Expr, input: usize) -> Expr {
        self.derivatives[self.exprs[expr.0].derivatives.unwrap() + input]
    }

    /// Creates a zero.
    pub fn zero() -> Expr {
        Expr(0)
    }

    /// Creates a one.
    pub fn one() -> Expr {
        Expr(1)
    }

    /// Creates a constant value.
    pub fn constant(&mut self, value: Float) -> Expr {
        let kind = ExprKind::Constant(value);
        let id = self.exprs.len();
        self.exprs.push(Entry {
            kind,
            derivatives: Some(0),
        });
        Expr(id)
    }

    /// Adds two values.
    pub fn add(&mut self, a: Expr, b: Expr) -> Expr {
        // The derivative of `a + b` is the derivative of `a` plus the one of `b`.
        let derivatives = (0..self.inputs)
            .map(|i| {
                let a_d = self.get_derivative(a, i);
                let b_d = self.get_derivative(b, i);
                self.push_expr_nodiff(ExprKind::Add(a_d, b_d))
            })
            .collect();
        self.push_expr(ExprKind::Add(a, b), derivatives)
    }

    /// Subtracts two values.
    pub fn sub(&mut self, a: Expr, b: Expr) -> Expr {
        // The derivative of `a - b` is the derivative of `a` minus the one of `b`.
        let derivatives = (0..self.inputs)
            .map(|i| {
                let a_d = self.get_derivative(a, i);
                let b_d = self.get_derivative(b, i);
                self.push_expr_nodiff(ExprKind::Sub(a_d, b_d))
            })
            .collect();
        self.push_expr(ExprKind::Sub(a, b), derivatives)
    }

    /// Multiplies two values.
    pub fn mul(&mut self, a: Expr, b: Expr) -> Expr {
        // `d(a*b) = da*b + a*db`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let a_d = self.get_derivative(a, i);
                let b_d = self.get_derivative(b, i);
                let first = self.push_expr_nodiff(ExprKind::Mul(a_d, b));
                let second = self.push_expr_nodiff(ExprKind::Mul(a, b_d));
                self.push_expr_nodiff(ExprKind::Add(first, second))
            })
            .collect();
        self.push_expr(ExprKind::Mul(a, b), derivatives)
    }

    /// Divides two values.
    pub fn div(&mut self, a: Expr, b: Expr) -> Expr {
        // `d(a/b) = (da*b - a*db)/(b*b)`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let a_d = self.get_derivative(a, i);
                let b_d = self.get_derivative(b, i);
                let first = self.push_expr_nodiff(ExprKind::Mul(a_d, b));
                let second = self.push_expr_nodiff(ExprKind::Mul(a, b_d));
                let diff = self.push_expr_nodiff(ExprKind::Sub(first, second));
                let b_squared = self.push_expr_nodiff(ExprKind::Mul(b, b));
                self.push_expr_nodiff(ExprKind::Div(diff, b_squared))
            })
            .collect();
        self.push_expr(ExprKind::Div(a, b), derivatives)
    }

    /// The i-th input.
    ///
    /// # Panics
    /// If the input is out of bounds
    pub fn input(&self, input: usize) -> Expr {
        assert!(input < self.inputs);
        Expr(2 + input)
    }

    /// Calculates the sine of a value.
    pub fn sin(&mut self, v: Expr) -> Expr {
        // `dsin(v) = cos(v) * dv`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let cos = self.push_expr_nodiff(ExprKind::Cos(v));
                self.push_expr_nodiff(ExprKind::Mul(cos, dv))
            })
            .collect();
        self.push_expr(ExprKind::Sin(v), derivatives)
    }

    /// Calculates the cosine of a value.
    pub fn cos(&mut self, v: Expr) -> Expr {
        // `dcos(v) = -sin(v) * dv`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let sin = self.push_expr_nodiff(ExprKind::Sin(v));
                let minus_sin = self.push_expr_nodiff(ExprKind::Neg(sin));
                self.push_expr_nodiff(ExprKind::Mul(minus_sin, dv))
            })
            .collect();
        self.push_expr(ExprKind::Cos(v), derivatives)
    }

    /// Square root of a number
    pub fn sqrt(&mut self, v: Expr) -> Expr {
        // dsqrt(v) = dv / 2sqrt(v)
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let sqrt = self.push_expr_nodiff(ExprKind::Sqrt(v));
                let two = self.push_expr_nodiff(ExprKind::Constant(2.0));
                let two_sqrt = self.push_expr_nodiff(ExprKind::Mul(two, sqrt));
                self.push_expr_nodiff(ExprKind::Div(dv, two_sqrt))
            })
            .collect();
        self.push_expr(ExprKind::Sqrt(v), derivatives)
    }

    /// Calculates e^expr
    pub fn exp(&mut self, v: Expr) -> Expr {
        // dexp(v) = dv*exp(v)
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let expv = self.push_expr_nodiff(ExprKind::Exp(v));
                self.push_expr_nodiff(ExprKind::Mul(expv, dv))
            })
            .collect();
        self.push_expr(ExprKind::Exp(v), derivatives)
    }

    /// Natural logarithm of expression
    pub fn log(&mut self, v: Expr) -> Expr {
        // dlog(v) = dv/v
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                self.push_expr_nodiff(ExprKind::Div(dv, v))
            })
            .collect();
        self.push_expr(ExprKind::Log(v), derivatives)
    }

    /// Calculates the atan2 of two value.
    pub fn atan2(&mut self, y: Expr, x: Expr) -> Expr {
        // `datan2(v) = (x*dy - y*dx)/(y^2 + x^2)` In theory, at least. We'll see if that works.
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dy = self.get_derivative(y, i);
                let dx = self.get_derivative(x, i);
                let x_dy = self.push_expr_nodiff(ExprKind::Mul(x, dy));
                let y_dx = self.push_expr_nodiff(ExprKind::Mul(y, dx));
                let x2 = self.push_expr_nodiff(ExprKind::Mul(x, x));
                let y2 = self.push_expr_nodiff(ExprKind::Mul(y, y));
                let x2_plus_y2 = self.push_expr_nodiff(ExprKind::Add(x2, y2));
                let xdy_minus_ydx = self.push_expr_nodiff(ExprKind::Sub(x_dy, y_dx));
                self.push_expr_nodiff(ExprKind::Div(xdy_minus_ydx, x2_plus_y2))
            })
            .collect();
        self.push_expr(ExprKind::Atan2(y, x), derivatives)
    }

    /// Negates the value.
    pub fn neg(&mut self, v: Expr) -> Expr {
        // `d(-v) = -dv`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                self.push_expr_nodiff(ExprKind::Neg(dv))
            })
            .collect();
        self.push_expr(ExprKind::Neg(v), derivatives)
    }

    /// Gets the minimum value.
    pub fn min(&mut self, a: Expr, b: Expr) -> Expr {
        self.ternary(
            Condition::Comparison(Comparison {
                a: b,
                b: a,
                kind: ComparisonKind::Gt,
            }),
            a,
            b,
        )
    }

    /// Takes the absolute value.
    pub fn abs(&mut self, v: Expr) -> Expr {
        let cond = Condition::Comparison(Comparison {
            a: v,
            b: Self::zero(),
            kind: ComparisonKind::Gt,
        });

        // `dabs(a) = if a > 0 da else -da`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let minus_dv = self.push_expr_nodiff(ExprKind::Neg(dv));
                self.push_expr_nodiff(ExprKind::Ternary(cond, dv, minus_dv))
            })
            .collect();
        let minus_v = self.push_expr_nodiff(ExprKind::Neg(v));
        self.push_expr(ExprKind::Ternary(cond, v, minus_v), derivatives)
    }

    /// A ternary expression
    pub fn ternary(&mut self, condition: Condition, then: Expr, else_: Expr) -> Expr {
        // dternary(then, else_) = if condition { dthen } else { delse_ }
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dthen = self.get_derivative(then, i);
                let delse = self.get_derivative(else_, i);
                self.push_expr_nodiff(ExprKind::Ternary(condition, dthen, delse))
            })
            .collect();
        self.push_expr(ExprKind::Ternary(condition, then, else_), derivatives)
    }
}

impl Context {
    /// Returns a function computing the given expressions.
    pub fn compute(&self, exprs: impl IntoIterator<Item = Expr>) -> Func {
        Func {
            func: compiler::compile(self, exprs),
        }
    }

    /// Returns a function computing the gradient for the given expression.
    pub fn compute_gradient(&self, expr: Expr) -> Func {
        let func = compiler::compile(self, (0..self.inputs).map(|i| self.get_derivative(expr, i)));

        Func { func }
    }

    /// Returns the gradient expressions, one for each input.
    pub fn gradient(&self, expr: Expr) -> Vec<Expr> {
        (0..self.inputs)
            .map(|i| self.get_derivative(expr, i))
            .collect()
    }
}

/// A callable function accepting inputs and outputs (as mutable reference)
#[derive(Clone, Copy)]
pub struct Func {
    func: fn(*const Float, *mut Float),
}

impl Func {
    /// Call this function with `inputs` and collect the outputs into `dst`
    pub fn call(&self, inputs: &[Float], dst: &mut [Float]) {
        (self.func)(inputs.as_ptr(), dst.as_mut_ptr());
    }
}

unsafe impl Send for Func {}
unsafe impl Sync for Func {}
