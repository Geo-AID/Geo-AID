use std::{
    fmt::Display,
    hash::Hash,
    ops::{Deref, DerefMut, Div, Mul},
    rc::Rc,
    sync::Arc,
};

use crate::cli::{AnnotationKind, DiagnosticData};

use self::parser::Type;
use self::token::{NamedIdent, Span, Token};

mod builtins;
pub mod compile;
pub mod figure;
pub mod parser;
pub mod token;
pub mod unroll;

#[derive(Debug)]
pub enum Error {
    InvalidToken {
        token: Token,
    },
    InvalidCharacter {
        character: char,
        error_span: Span,
    },
    EndOfInput,
    UndefinedRuleOperator {
        operator: NamedIdent,
    },
    InconsistentIterators {
        first_span: Span,
        first_length: usize,
        occured_span: Span,
        occured_length: usize,
        error_span: Span,
    },
    InconsistentTypes {
        // boxes here to reduce size
        expected: (Type, Box<Span>),
        got: (Type, Box<Span>),
        error_span: Box<Span>,
    },
    InvalidType {
        expected: Type,
        got: (Type, Span),
    },
    RedefinedVariable {
        defined_at: Span,
        error_span: Span,
        variable_name: String,
    },
    UndefinedTypeVariable {
        definition: Span,
    },
    UndefinedVariable {
        error_span: Span,
        variable_name: String,
    },
    UndefinedFunction {
        error_span: Span,
        function_name: String,
    },
    FetureNotSupported {
        error_span: Span,
        feature_name: &'static str,
    },
    InvalidArgumentCount {
        error_span: Span,
        expected: &'static [u8],
        got: u8,
    },
    OverloadNotFound {
        error_span: Span,
        params: Vec<Type>,
        function_name: String,
    },
    CannotUnpack {
        error_span: Span,
        ty: Type,
    },
    ImplicitConversionDoesNotExist {
        error_span: Span,
        from: Type,
        to: Type,
    },
    InvalidOperandType {
        error_span: Span,
        got: (Type, Span),
        op: String,
    },
    LetStatUnexpectedIterator {
        var_span: Span,
        error_span: Span,
    },
}

impl Error {
    #[must_use]
    pub fn inconsistent_iterators_get_span(&self) -> Option<Span> {
        match self {
            Self::InconsistentIterators {
                first_span: _,
                first_length: _,
                occured_span,
                occured_length: _,
                error_span: _,
            } => Some(*occured_span),
            _ => None,
        }
    }

    #[must_use]
    pub fn invalid_token(token: Token) -> Self {
        Self::InvalidToken { token }
    }

    #[must_use]
    pub fn invalid_character(character: char, error_span: Span) -> Self {
        Self::InvalidCharacter {
            character,
            error_span,
        }
    }

    #[must_use]
    pub fn end_of_input() -> Self {
        Self::EndOfInput
    }

    #[must_use]
    pub fn undefined_rule_operator(name: NamedIdent) -> Self {
        Self::UndefinedRuleOperator { operator: name }
    }

    #[must_use]
    pub fn redefined_variable(defined_at: Span, error_span: Span, variable_name: String) -> Self {
        Self::RedefinedVariable {
            defined_at,
            error_span,
            variable_name,
        }
    }

    #[must_use]
    pub fn undefined_type_variable(definition: Span) -> Self {
        Self::UndefinedTypeVariable { definition }
    }

    #[must_use]
    pub fn undefined_function(error_span: Span, function_name: String) -> Self {
        Self::UndefinedFunction {
            error_span,
            function_name,
        }
    }

    #[must_use]
    pub fn feature_not_supported(error_span: Span, feature_name: &'static str) -> Self {
        Self::FetureNotSupported {
            error_span,
            feature_name,
        }
    }

    #[must_use]
    pub fn invalid_argument_count(error_span: Span, expected: &'static [u8], got: u8) -> Self {
        Self::InvalidArgumentCount {
            error_span,
            expected,
            got,
        }
    }

    #[must_use]
    pub fn overload_not_found(error_span: Span, function_name: String, params: Vec<Type>) -> Self {
        Self::OverloadNotFound {
            error_span,
            params,
            function_name,
        }
    }

    #[must_use]
    pub fn cannot_unpack(error_span: Span, ty: Type) -> Self {
        Self::CannotUnpack { error_span, ty }
    }

    #[must_use]
    pub fn implicit_conversion_does_not_exist(error_span: Span, from: Type, to: Type) -> Self {
        Self::ImplicitConversionDoesNotExist {
            error_span,
            from,
            to,
        }
    }

    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn diagnostic(self) -> DiagnosticData {
        match self {
            Self::InvalidToken { token } => {
                DiagnosticData::new(&format!("invalid token: `{token}`")).add_span(token.get_span())
            }
            Self::InvalidCharacter {
                character,
                error_span,
            } => DiagnosticData::new(&format!("invalid character: `{character}`"))
                .add_span(error_span),
            Self::EndOfInput => DiagnosticData::new("unexpected end of input"),
            Self::UndefinedRuleOperator { operator } => {
                DiagnosticData::new(&format!("undefined rule operator: `{}`", operator.ident))
                    .add_span(operator.span)
            }
            Self::InconsistentIterators {
                first_span,
                first_length,
                occured_span,
                occured_length,
                error_span,
            } => DiagnosticData::new(&"inconsitent iterator lengths")
                .add_span(error_span)
                .add_annotation(
                    first_span,
                    AnnotationKind::Note,
                    &format!("First iterator with length {first_length} here."),
                )
                .add_annotation(
                    occured_span,
                    AnnotationKind::Note,
                    &format!("Inconsistensy (iterator with length {occured_length}) here."),
                ),
            Self::InconsistentTypes {
                expected,
                got,
                error_span,
            } => DiagnosticData::new("inconsistent types")
                .add_span(*error_span)
                .add_annotation(
                    *expected.1,
                    AnnotationKind::Note,
                    &format!("This expression is of type {}", expected.0),
                )
                .add_annotation(
                    *got.1,
                    AnnotationKind::Note,
                    &format!("This expression is of type {}", got.0),
                ),
            Self::InvalidType { expected, got } => DiagnosticData::new(&format!(
                "invalid type: `{expected}` expected, got `{}`",
                got.0
            ))
            .add_span(got.1),
            Self::RedefinedVariable {
                defined_at,
                error_span,
                variable_name,
            } => DiagnosticData::new(&format!("redefined variable: `{variable_name}`"))
                .add_span(error_span)
                .add_annotation(defined_at, AnnotationKind::Note, "First defined here.")
                .add_annotation(error_span, AnnotationKind::Note, "Then redefined here."),
            Self::UndefinedTypeVariable { definition } => {
                DiagnosticData::new("variable of undefined type")
                    .add_span(definition)
                    .add_annotation(definition, AnnotationKind::Note, "Defined here.")
            }
            Self::UndefinedVariable {
                error_span,
                variable_name,
            } => DiagnosticData::new(&format!("undefined variable: `{variable_name}`"))
                .add_span(error_span),
            Self::UndefinedFunction {
                error_span,
                function_name,
            } => DiagnosticData::new(&format!("undefined function: `{function_name}`"))
                .add_span(error_span),
            Self::FetureNotSupported {
                error_span,
                feature_name,
            } => {
                DiagnosticData::new(&format!("feature `{feature_name}` of undefined type"))
                    .add_span(error_span)
            }
            Self::InvalidArgumentCount {
                error_span,
                expected,
                got,
            } => {
                DiagnosticData::new(&format!("invalid argument count. Expected one of `{expected:?}`, got {got}"))
                    .add_span(error_span)
            }
            Self::OverloadNotFound {
                error_span,
                params,
                function_name,
            } => {
                DiagnosticData::new(&format!("overload for function `{function_name}` with params `({})` not found", params.into_iter().map(|x| format!("{x}")).collect::<Vec<String>>().join(", ")))
                    .add_span(error_span)
            },
            Self::CannotUnpack { error_span, ty } => {
                DiagnosticData::new(&format!("could not unpack `{ty}` onto a point collection"))
                    .add_span(error_span)
            }
            Self::ImplicitConversionDoesNotExist {
                error_span,
                from,
                to,
            } => {
                DiagnosticData::new(&format!("implicit conversion from `{from}` to `{to}` does not exist."))
                    .add_span(error_span)
            }
            Self::InvalidOperandType {
                error_span,
                got,
                op,
            } => {
                DiagnosticData::new(&format!("invalid operand type `{}` for operator `{op}`", got.0))
                    .add_span(error_span)
                    .add_annotation(got.1, AnnotationKind::Note, "This is of invalid type.")
            }
            Self::LetStatUnexpectedIterator {
                var_span,
                error_span,
            } => {
                DiagnosticData::new(&"unexpected iterator in right-hand side of `let` statement")
                    .add_span(error_span)
                    .add_annotation(var_span, AnnotationKind::Note, "There was no iterator of left-hand side, so the same is expected for the right.")
            }
        }
    }
}

/// Defines an object with assigned weight
#[derive(Debug)]
pub struct Weighed<T> {
    pub object: T,
    pub weight: f64,
}

impl<T> Weighed<T> {
    pub fn one(object: T) -> Self {
        Self {
            object,
            weight: 1.0,
        }
    }
}

/// Defines a simple unit.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SimpleUnit {
    Distance,
    Point,
    Angle,
    Line,
    Scalar,
}

impl Mul for SimpleUnit {
    type Output = ComplexUnit;

    fn mul(self, rhs: Self) -> Self::Output {
        let complex = ComplexUnit::new(self);

        complex * rhs
    }
}

/// Defines a complex unit: a product of simple units.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ComplexUnit([i8; SimpleUnit::Scalar as usize]);

pub mod unit {
    use super::{ComplexUnit, SimpleUnit};

    pub const DISTANCE: ComplexUnit = ComplexUnit::new(SimpleUnit::Distance);
    pub const POINT: ComplexUnit = ComplexUnit::new(SimpleUnit::Point);
    pub const ANGLE: ComplexUnit = ComplexUnit::new(SimpleUnit::Angle);
    pub const LINE: ComplexUnit = ComplexUnit::new(SimpleUnit::Line);
    pub const SCALAR: ComplexUnit = ComplexUnit::new(SimpleUnit::Scalar);
}

pub mod ty {
    use super::{parser::{Type, PredefinedType}, ComplexUnit, SimpleUnit};

    pub const DISTANCE: Type = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Distance))));
    pub const POINT: Type = Type::Predefined(PredefinedType::Point);
    pub const ANGLE: Type = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Angle))));
    pub const LINE: Type = Type::Predefined(PredefinedType::Line);
    pub const SCALAR: Type = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Scalar))));

    #[must_use]
    pub const fn collection(length: usize) -> Type {
        Type::Predefined(PredefinedType::PointCollection(length))
    }
}

impl ComplexUnit {
    #[must_use]
    pub const fn new(simple: SimpleUnit) -> Self {
        let mut arr = [0; SimpleUnit::Scalar as usize];

        match simple {
            SimpleUnit::Scalar => (),
            _ => arr[simple as usize] = 1,
        }

        Self(arr)
    }
}

impl Display for ComplexUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for i in 0..(SimpleUnit::Scalar as usize) {
            if self.0[i] > 0 {
                let name = match i {
                    0 => "Distance",
                    1 => "Point",
                    2 => "Angle",
                    3 => "Line",
                    _ => unreachable!(),
                };

                if self.0[i] == 1 {
                    s += name;
                } else {
                    s += &format!("{name}^{}", self.0[i]);
                };

                s += " * ";
            }
        }

        if s.is_empty() {
            write!(f, "no unit")
        } else {
            write!(
                f,
                "{}",
                String::from_utf8(s.as_bytes()[0..(s.len() - 3)].to_vec()).unwrap()
            )
        }
    }
}

impl Mul<SimpleUnit> for ComplexUnit {
    type Output = ComplexUnit;

    fn mul(mut self, rhs: SimpleUnit) -> Self::Output {
        match rhs {
            SimpleUnit::Scalar => (),
            // Clippy doesn't like exponentiation. Thanks, Clippy
            #[allow(clippy::suspicious_arithmetic_impl)]
            _ => self[rhs as usize] += 1,
        }
        self
    }
}

impl Mul for ComplexUnit {
    type Output = ComplexUnit;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self.iter_mut()
            .enumerate()
            .map(|(i, x)| *x += rhs[i])
            .for_each(drop);
        self
    }
}

impl Div<SimpleUnit> for ComplexUnit {
    type Output = ComplexUnit;

    fn div(mut self, rhs: SimpleUnit) -> Self::Output {
        match rhs {
            SimpleUnit::Scalar => (),
            // Oh, c'mon, Clippy
            #[allow(clippy::suspicious_arithmetic_impl)]
            _ => self[rhs as usize] -= 1,
        }
        self
    }
}

impl Div for ComplexUnit {
    type Output = ComplexUnit;

    fn div(mut self, rhs: Self) -> Self::Output {
        self.iter_mut()
            .enumerate()
            .map(|(i, x)| *x -= rhs[i])
            .for_each(drop);
        self
    }
}

impl Deref for ComplexUnit {
    type Target = [i8; SimpleUnit::Scalar as usize];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ComplexUnit {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Defines an expression.
#[derive(Debug)]
pub enum Expression {
    /// Euclidean distance between two points.
    PointPointDistance(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Euclidean distance between a point and its rectangular projection onto a line.
    PointLineDistance(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// An angle defined with 3 points.
    AnglePoint(
        Arc<Weighed<Expression>>,
        Arc<Weighed<Expression>>,
        Arc<Weighed<Expression>>,
    ),
    /// An angle defined with 2 lines.
    AngleLine(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// A real literal.
    Literal(f64, ComplexUnit),
    /// An adjustable indexed point in euclidean space
    FreePoint(usize),
    /// A line in euclidean space. defined by two points.
    Line(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// The point where two lines cross.
    LineLineIntersection(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Changes the unit
    SetUnit(Arc<Weighed<Expression>>, ComplexUnit),
    /// Adds two values
    Sum(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Subtracts two values
    Difference(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Multiplies two values
    Product(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Divides two values
    Quotient(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Changes the sign
    Negation(Arc<Weighed<Expression>>),
    /// An angle bisector.
    AngleBisector(
        Arc<Weighed<Expression>>,
        Arc<Weighed<Expression>>,
        Arc<Weighed<Expression>>,
    ),
    /// Takes the average value (arithmetic mean)
    Average(Vec<Arc<Weighed<Expression>>>),
    /// Generates a line perpendicular to $1 going through $2
    PerpendicularThrough(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Generates a line parallel to $1 going through $2
    ParallelThrough(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
}

/// Defines the kind and information about criteria the figure must obey.
#[derive(Debug)]
pub enum CriteriaKind {
    /// Equality. Quality rises quickly as two values approach each other, drops quickly as their difference grows.
    Equal(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Less. Quality starts rising on equality.
    Less(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Greater. Quality starts rising on equality.
    Greater(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// Inverts the criteria. The quality is calculated as 1 - the quality of the inverted criteria.
    Inverse(Box<CriteriaKind>),
}

/// Defines a weighed piece of criteria the figure must obey.
pub type Criteria = Weighed<CriteriaKind>;

pub struct HashableArc<T>(Arc<T>);

impl<T> HashableArc<T> {
    #[must_use]
    pub fn new(content: Arc<T>) -> Self {
        Self(content)
    }
}

impl<T> Hash for HashableArc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state);
    }
}

impl<T> PartialEq for HashableArc<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.0) == Arc::as_ptr(&other.0)
    }
}

impl<T> Eq for HashableArc<T> {
    fn assert_receiver_is_total_eq(&self) {}
}

pub struct HashableRc<T: ?Sized>(Rc<T>);

impl<T: ?Sized> HashableRc<T> {
    #[must_use]
    pub fn new(content: Rc<T>) -> Self {
        Self(content)
    }
}

impl<T: ?Sized> Hash for HashableRc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}

impl<T: ?Sized> PartialEq for HashableRc<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0)
    }
}

impl<T: ?Sized> Eq for HashableRc<T> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<T: ?Sized> Deref for HashableRc<T> {
    type Target = Rc<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
