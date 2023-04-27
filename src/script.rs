use std::{
    fmt::Display,
    hash::Hash,
    ops::{Deref, DerefMut, Div, Mul},
    rc::Rc,
    sync::{self, Arc},
};

use serde::Serialize;

use crate::{
    cli::{AnnotationKind, Change, DiagnosticData, Fix},
    generator::expression::{AnyExpr, Expression, PointExpr, ScalarExpr},
    span,
};

use self::parser::Type;
use self::token::{NamedIdent, Position, Span, Token};

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
    IteratorIdMustBeAnInteger {
        error_span: Span,
    },
    IteratorIdExceeds255 {
        error_span: Span,
    },
    SingleVariantExplicitIterator {
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
    IteratorWithSameIdIterator {
        error_span: Span,
        parent_span: Span,
        contained_span: Span,
    },
    InconsistentTypes {
        // boxes here to reduce size
        expected: (Type, Box<Span>),
        got: (Type, Box<Span>),
        error_span: Box<Span>,
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
        suggested: Option<String>,
    },
    UndefinedFunction {
        error_span: Span,
        function_name: String,
        suggested: Option<String>,
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
    LetStatMoreThanOneIterator {
        error_span: Span,
        first_span: Span,
        second_span: Span,
    },
    NonPointInPointCollection {
        error_span: Span,
        received: (Span, Type),
    },
    FlagDoesNotExist {
        flag_name: String,
        flag_span: Span,
        error_span: Span,
        suggested: Option<String>,
    },
    FlagSetExpected {
        error_span: Span,
    },
    FlagStringExpected {
        error_span: Span,
    },
    FlagBooleanExpected {
        error_span: Span,
    },
    RedefinedFlag {
        error_span: Span,
        first_defined: Span,
        flag_name: String,
    },
    FlagEnumInvalidValue {
        error_span: Span,
        available_values: &'static [&'static str],
        received_value: String,
    },
    RequiredFlagNotSet {
        flag_name: &'static str,
        required_because: Span,
        flagdef_span: Option<Span>,
        available_values: &'static [&'static str],
    },
}

impl Error {
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
    pub fn feature_not_supported(error_span: Span, feature_name: &'static str) -> Self {
        Self::FetureNotSupported {
            error_span,
            feature_name,
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
            Self::RedefinedVariable {
                defined_at,
                error_span,
                variable_name,
            } => DiagnosticData::new(&format!("redefined variable: `{variable_name}`"))
                .add_span(error_span)
                .add_annotation(defined_at, AnnotationKind::Note, "First defined here."),
            Self::UndefinedTypeVariable { definition } => {
                DiagnosticData::new("variable of undefined type")
                    .add_span(definition)
                    .add_annotation(definition, AnnotationKind::Note, "Defined here.")
            }
            Self::UndefinedVariable {
                error_span,
                variable_name,
                suggested
            } => {
                let message = suggested.map(|v| format!("Did you mean: `{v}`?"));
                DiagnosticData::new(&format!("undefined variable: `{variable_name}`"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
            Self::UndefinedFunction {
                error_span,
                function_name,
                suggested
            } => {
                let message = suggested.map(|v| format!("Did you mean: `{v}`?"));
                DiagnosticData::new(&format!("undefined function: `{function_name}`"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
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
            Self::IteratorWithSameIdIterator { error_span, parent_span, contained_span } => {
                DiagnosticData::new(&"An iterator with an id of `x` must not contain an iterator with an id of `x`.")
                    .add_span(error_span)
                    .add_annotation(parent_span, AnnotationKind::Note, "Parent iterator here.")
                    .add_annotation(contained_span, AnnotationKind::Note, "Child iterator here.")
            }
            Self::LetStatMoreThanOneIterator { error_span, first_span, second_span } => {
                DiagnosticData::new(&"Right hand side of a let statement must contain at most a single level of iteration.")
                    .add_span(error_span)
                    .add_annotation(first_span, AnnotationKind::Note, "First iterator here.")
                    .add_annotation(second_span, AnnotationKind::Note, "Second iterator here.")
            }
            Self::IteratorIdMustBeAnInteger { error_span } => {
                DiagnosticData::new(&"Iterator id must be an integer.")
                    .add_span(error_span)
            }
            Self::IteratorIdExceeds255 { error_span } => {
                DiagnosticData::new(&"Iterator id must be smaller than 256.")
                    .add_span(error_span)
            }
            Self::SingleVariantExplicitIterator { error_span } => {
                DiagnosticData::new(&"Explicit iterators must have at least two variants.")
                    .add_span(error_span)
            }
            Self::NonPointInPointCollection { error_span, received } => {
                DiagnosticData::new(&"All values in a point collection constructor must be points.")
                    .add_span(error_span)
                    .add_annotation(received.0, AnnotationKind::Note, &format!("Value should be a point, received {}.", received.1))
            }
            Self::FlagDoesNotExist { flag_name, flag_span, error_span, suggested } => {
                let message = suggested.map(|v| format!("Did you mean: `{v}`?"));
                DiagnosticData::new(&format!("Compiler flag `{flag_name}` does not exist."))
                    .add_span(error_span)
                    .add_annotation(flag_span, AnnotationKind::Note, &"This does not exist.")
                    .add_annotation_opt_msg(flag_span, AnnotationKind::Help, message.as_ref())
            }
            Self::FlagSetExpected { error_span } => {
                DiagnosticData::new(&"Expected a flag set ({...}).")
                    .add_span(error_span)
            }
            Self::FlagStringExpected { error_span } => {
                DiagnosticData::new(&"Expected a string (identifier).")
                    .add_span(error_span)
            }
            Self::FlagBooleanExpected { error_span } => {
                DiagnosticData::new(&"Expected a boolean value (enabled, disabled, on, off, true, false, 1 or 0).")
                    .add_span(error_span)
            }
            Self::RedefinedFlag {
                first_defined,
                error_span,
                flag_name,
            } => DiagnosticData::new(&format!("redefined flag: `{flag_name}`"))
                .add_span(error_span)
                .add_annotation(first_defined, AnnotationKind::Note, "First defined here."),
            Self::FlagEnumInvalidValue { error_span, available_values, received_value } => {
                DiagnosticData::new(&format!("Invalid value for an enum flag: `{received_value}`"))
                    .add_span(error_span)
                    .add_annotation(error_span, AnnotationKind::Help, &format!("Supported values: {}", available_values.iter().map(
                        |v| format!("`{v}`")
                    ).collect::<Vec<String>>().join(", ")))
            }
            Self::RequiredFlagNotSet { flag_name, required_because, flagdef_span, available_values } => {
                DiagnosticData::new(&format!("You must set a value for flag `{flag_name}`."))
                    .add_annotation(required_because, AnnotationKind::Note, &"Required because of this line.")
                    .add_annotation(required_because, AnnotationKind::Help, &format!("Possible values: {}", available_values.iter().map(
                        |v| format!("`{v}`")
                    ).collect::<Vec<String>>().join(", ")))
                    .add_annotation_opt_span(flagdef_span, AnnotationKind::Note, &"Flag defined here")
                    .add_fix(Fix {
                        message: String::from("Consider defining this flag at the top of the file."),
                        changes: vec![
                            Change {
                                span: span!(1, 1, 1, 1),
                                new_content: vec![
                                    format!("@{flag_name}: {};", available_values[0]),
                                    String::new()
                                ]
                            }
                        ]
                    })
            }
        }
    }
}

/// Defines an object with assigned weight
#[derive(Debug, Clone, Serialize)]
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
    Angle,
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
#[derive(Clone, PartialEq, Eq, Debug, Serialize)]
pub struct ComplexUnit([i8; SimpleUnit::Scalar as usize]);

pub mod unit {
    use super::{ComplexUnit, SimpleUnit};

    pub const DISTANCE: ComplexUnit = ComplexUnit::new(SimpleUnit::Distance);
    pub const ANGLE: ComplexUnit = ComplexUnit::new(SimpleUnit::Angle);
    pub const SCALAR: ComplexUnit = ComplexUnit::new(SimpleUnit::Scalar);
}

pub mod ty {
    use super::{
        parser::{PredefinedType, Type},
        ComplexUnit, SimpleUnit,
    };

    pub const DISTANCE: Type = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
        SimpleUnit::Distance,
    ))));
    pub const POINT: Type = Type::Predefined(PredefinedType::Point);
    pub const ANGLE: Type = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
        SimpleUnit::Angle,
    ))));
    pub const LINE: Type = Type::Predefined(PredefinedType::Line);
    pub const SCALAR: Type = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
        SimpleUnit::Scalar,
    ))));
    pub const SCALAR_UNKNOWN: Type = Type::Predefined(PredefinedType::Scalar(None));

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

impl Mul<&ComplexUnit> for ComplexUnit {
    type Output = ComplexUnit;

    fn mul(mut self, rhs: &Self) -> Self::Output {
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

impl Div<&ComplexUnit> for ComplexUnit {
    type Output = ComplexUnit;

    fn div(mut self, rhs: &Self) -> Self::Output {
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

/// Defines the kind and information about criteria the figure must obey.
#[derive(Debug)]
pub enum CriteriaKind {
    /// Equality. Quality rises quickly as two values approach each other, drops quickly as their difference grows.
    EqualScalar(Arc<Expression<ScalarExpr>>, Arc<Expression<ScalarExpr>>),
    /// Equality. Quality rises quickly as two values approach each other, drops quickly as their difference grows.
    EqualPoint(Arc<Expression<PointExpr>>, Arc<Expression<PointExpr>>),
    /// Less. Quality starts rising on equality.
    Less(Arc<Expression<ScalarExpr>>, Arc<Expression<ScalarExpr>>),
    /// Greater. Quality starts rising on equality.
    Greater(Arc<Expression<ScalarExpr>>, Arc<Expression<ScalarExpr>>),
    /// Inverts the criteria. The quality is calculated as 1 - the quality of the inverted criteria.
    Inverse(Box<CriteriaKind>),
    /// Bias. Always evaluates to 1.0. Artificially raises quality for everything contained  in the arc.
    Bias(Arc<Expression<AnyExpr>>),
}

impl CriteriaKind {
    pub fn collect(&self, exprs: &mut Vec<usize>) {
        match self {
            CriteriaKind::EqualScalar(lhs, rhs)
            | CriteriaKind::Less(lhs, rhs)
            | CriteriaKind::Greater(lhs, rhs) => {
                lhs.collect(exprs);
                rhs.collect(exprs);
            }
            CriteriaKind::Inverse(v) => v.collect(exprs),
            CriteriaKind::Bias(v) => v.collect(exprs),
            CriteriaKind::EqualPoint(lhs, rhs) => {
                lhs.collect(exprs);
                rhs.collect(exprs);
            }
        }
    }
}

/// Defines a weighed piece of criteria the figure must obey.
pub type Criteria = Weighed<CriteriaKind>;

#[derive(Serialize, Debug, Clone)]
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

#[derive(Debug)]
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

pub struct HashableWeakArc<T>(sync::Weak<T>);

impl<T> HashableWeakArc<T> {
    #[must_use]
    pub fn new(content: sync::Weak<T>) -> Self {
        Self(content)
    }
}

impl<T> Hash for HashableWeakArc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        sync::Weak::as_ptr(&self.0).hash(state);
    }
}

impl<T> PartialEq for HashableWeakArc<T> {
    fn eq(&self, other: &Self) -> bool {
        sync::Weak::as_ptr(&self.0) == sync::Weak::as_ptr(&other.0)
    }
}

impl<T> Eq for HashableWeakArc<T> {
    fn assert_receiver_is_total_eq(&self) {}
}
