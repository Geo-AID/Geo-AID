mod compiler;

#[cfg(feature = "f64")]
pub type Float = f64;
#[cfg(not(feature = "f64"))]
pub type Float = f32;

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Expr(usize);

#[derive(Debug, Clone, Copy)]
pub struct Comparison {
    pub a: Expr,
    pub b: Expr,
    pub kind: ComparisonKind,
}

#[derive(Debug, Clone, Copy)]
pub enum ComparisonKind {
    Eq,
    Neq,
    Gt,
    Lt,
    Gteq,
    Lteq,
}

#[derive(Debug, Clone, Copy)]
pub enum Condition {
    Comparison(Comparison),
}

#[derive(Debug, Clone, Copy)]
enum ExprKind {
    Constant(Float),
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Input(usize),
    Sin(Expr),
    Cos(Expr),
    Acos(Expr),
    Atan2(Expr, Expr),
    Neg(Expr),
    Ternary(Condition, Expr, Expr),
    Pow(Expr, Float),
}

#[derive(Debug, Clone, Copy)]
struct Entry {
    kind: ExprKind,
    derivatives: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct Context {
    inputs: usize,
    exprs: Vec<Entry>,
    /// Indices of expressions holding derivatives w.r.t. respective inputs
    derivatives: Vec<Expr>,
}

impl Context {
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

    pub fn stringify(&self, expr: Expr) -> String {
        self.stringify_kind(self.exprs[expr.0].kind)
    }

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
            ExprKind::Acos(v) => format!("acos({})", self.stringify(v)),
            ExprKind::Atan2(y, x) => format!("atan2({}, {})", self.stringify(y), self.stringify(x)),
            ExprKind::Neg(v) => format!("-{}", self.stringify(v)),
            ExprKind::Ternary(cond, then, else_) => format!(
                "({} ? {} : {})",
                self.stringify_condition(cond),
                self.stringify(then),
                self.stringify(else_)
            ),
            ExprKind::Pow(v, p) => format!("{}^{p}", self.stringify(v)),
        }
    }

    fn stringify_condition(&self, condition: Condition) -> String {
        match condition {
            Condition::Comparison(cmp) => {
                let a = self.stringify(cmp.a);
                let b = self.stringify(cmp.b);
                let sign = match cmp.kind {
                    ComparisonKind::Eq => "=",
                    ComparisonKind::Neq => "!=",
                    ComparisonKind::Gt => ">",
                    ComparisonKind::Lt => "<",
                    ComparisonKind::Gteq => "≥",
                    ComparisonKind::Lteq => "≤",
                };

                format!("{a} {sign} {b}")
            }
        }
    }

    fn push_expr_nodiff(&mut self, kind: ExprKind) -> Expr {
        let id = self.exprs.len();
        self.exprs.push(Entry {
            kind,
            derivatives: None,
        });
        Expr(id)
    }

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
        assert!(input <= self.inputs);
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

    /// Calculates the arc-cosine of a value.
    pub fn acos(&mut self, v: Expr) -> Expr {
        // `dacos(v) = -1/sqrt(1 - v2) * dv`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let one = Self::one();
                let v2 = self.push_expr_nodiff(ExprKind::Mul(v, v));
                let one_minus_v2 = self.push_expr_nodiff(ExprKind::Sub(one, v2));
                let inverse_square_root = self.push_expr_nodiff(ExprKind::Pow(one_minus_v2, -2.0));
                self.push_expr_nodiff(ExprKind::Mul(inverse_square_root, dv))
            })
            .collect();
        self.push_expr(ExprKind::Acos(v), derivatives)
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
                a,
                b,
                kind: ComparisonKind::Lt,
            }),
            a,
            b,
        )
    }

    /// Raises the value to an exponent.
    pub fn pow(&mut self, v: Expr, e: Float) -> Expr {
        // `d(v^e) = ev^(e-1) * dv`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let raised = self.push_expr_nodiff(ExprKind::Pow(v, e - 1.0));
                let e_const = self.push_expr_nodiff(ExprKind::Constant(e));
                let multiplied = self.push_expr_nodiff(ExprKind::Mul(e_const, raised));
                self.push_expr_nodiff(ExprKind::Mul(multiplied, dv))
            })
            .collect();
        self.push_expr(ExprKind::Pow(v, e), derivatives)
    }

    /// Takes the absolute value.
    pub fn abs(&mut self, v: Expr) -> Expr {
        let cond = Condition::Comparison(Comparison {
            a: Self::zero(),
            b: v,
            kind: ComparisonKind::Lt,
        });

        // `dabs(a) = if a > 0 da else -da`
        let derivatives = (0..self.inputs)
            .map(|i| {
                let dv = self.get_derivative(v, i);
                let minus_dv = self.push_expr_nodiff(ExprKind::Neg(dv));
                self.push_expr_nodiff(ExprKind::Ternary(cond, dv, minus_dv))
            })
            .collect();
        let minus_v = self.neg(v);
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

#[derive(Clone, Copy)]
pub struct Func {
    func: fn(*const Float, *mut Float),
}

impl Func {
    pub fn call(&self, inputs: &[Float], dst: &mut [Float]) {
        (self.func)(inputs.as_ptr(), dst.as_mut_ptr());
    }
}

unsafe impl Send for Func {}
unsafe impl Sync for Func {}
