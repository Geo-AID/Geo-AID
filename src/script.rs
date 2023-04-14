use std::{
    hash::Hash,
    ops::{Deref, DerefMut, Div, Mul},
    sync::Arc, rc::Rc,
};

use self::token::{NamedIdent, Span, Token};
use self::{parser::Type, token::PointCollection};

pub mod figure;
pub mod parser;
pub mod token;
pub mod unroll;
mod builtins;
pub mod compile;

#[derive(Debug)]
pub enum ScriptError {
    InvalidToken {
        token: Token,
    },
    InvalidCharacter {
        character: char,
    },
    EndOfInput,
    UndefinedOperator {
        operator: NamedIdent,
    },
    InconsistentIterators {
        span: Span,
    },
    InconsistentTypes {
        expected: (Type, Span),
        got: (Type, Span),
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
    CollectionNotInfered {
        error_span: Span,
        collection: PointCollection,
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
        feature_name: &'static str
    },
    InvalidArgumentCount {
        error_span: Span,
        expected: &'static [u8],
        got: u8
    },
    OverloadNotFound {
        error_span: Span,
        params: Vec<Type>,
        function_name: String
    },
    CannotUnpack {
        error_span: Span,
        ty: Type
    },
    ImplicitConversionDoesNotExist {
        error_span: Span,
        from: Type,
        to: Type
    },
    InvalidOperandType {
        error_span: Span,
        got: (Type, Span),
        op: String
    }
}

impl ScriptError {
    pub fn invalid_token(token: Token) -> Self {
        Self::InvalidToken { token }
    }

    pub fn invalid_character(character: char) -> Self {
        Self::InvalidCharacter { character }
    }

    pub fn end_of_input() -> Self {
        Self::EndOfInput
    }

    pub fn undefined_operator(name: NamedIdent) -> Self {
        Self::UndefinedOperator { operator: name }
    }

    pub fn inconsistent_iterators(span: Span) -> Self {
        Self::InconsistentIterators { span }
    }

    pub fn inconsistent_types(expected: Type, exp_span: Span, got: Type, got_span: Span) -> Self {
        Self::InconsistentTypes {
            expected: (expected, exp_span),
            got: (got, got_span),
        }
    }

    pub fn invalid_type(expected: Type, got: Type, got_span: Span) -> Self {
        Self::InvalidType {
            expected,
            got: (got, got_span),
        }
    }

    pub fn redefined_variable(defined_at: Span, error_span: Span, variable_name: String) -> Self {
        Self::RedefinedVariable {
            defined_at,
            error_span,
            variable_name,
        }
    }

    pub fn collection_not_infered(error_span: Span, collection: PointCollection) -> Self {
        Self::CollectionNotInfered {
            error_span,
            collection,
        }
    }

    pub fn undefined_type_variable(definition: Span) -> Self {
        Self::UndefinedTypeVariable { definition }
    }

    pub fn undefined_variable(error_span: Span, variable_name: String) -> Self {
        Self::UndefinedVariable {
            error_span,
            variable_name,
        }
    }

    pub fn undefined_function(error_span: Span, function_name: String) -> Self {
        Self::UndefinedFunction {
            error_span,
            function_name,
        }
    }

    pub fn feature_not_supported(error_span: Span, feature_name: &'static str) -> Self {
        Self::FetureNotSupported { error_span, feature_name }
    }

    pub fn invalid_argument_count(error_span: Span, expected: &'static [u8], got: u8) -> Self {
        Self::InvalidArgumentCount { error_span, expected, got }
    }

    pub fn overload_not_found(error_span: Span, function_name: String, params: Vec<Type>) -> Self {
        Self::OverloadNotFound { error_span, params, function_name }
    }

    pub fn cannot_unpack(error_span: Span, ty: Type) -> Self {
        Self::CannotUnpack { error_span, ty }
    }

    pub fn implicit_conversion_does_not_exist(error_span: Span, from: Type, to: Type) -> Self {
        Self::ImplicitConversionDoesNotExist { error_span, from, to }
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
            weight: 1.0
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

impl ComplexUnit {
    pub fn new(simple: SimpleUnit) -> Self {
        let mut arr = [0; SimpleUnit::Scalar as usize];

        match simple {
            SimpleUnit::Scalar => (),
            _ => arr[simple as usize] = 1,
        }

        Self(arr)
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
        Box<Weighed<Expression>>,
        Box<Weighed<Expression>>,
        Box<Weighed<Expression>>,
    ),
    /// A real literal.
    Literal(f64, ComplexUnit),
    /// An adjustable indexed point in euclidean space
    FreePoint(usize),
    /// A line in euclidean space. defined by two points.
    Line(Arc<Weighed<Expression>>, Arc<Weighed<Expression>>),
    /// The point where two lines cross.
    LineCrossing(Box<Weighed<Expression>>, Box<Weighed<Expression>>),
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
    Negation(Arc<Weighed<Expression>>)
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
    pub fn new(content: Arc<T>) -> Self {
        Self(content)
    }
}

impl<T> Hash for HashableArc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
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
    pub fn new(content: Rc<T>) -> Self {
        Self(content)
    }
}

impl<T: ?Sized> Hash for HashableRc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
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
