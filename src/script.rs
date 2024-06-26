/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use std::{
    fmt::Display,
    hash::Hash,
    ops::{Deref, DerefMut, Div, Mul},
    rc::Rc,
    sync::{self, Arc},
};

use num_traits::{One, Zero};
use serde::Serialize;

use crate::{
    cli::{AnnotationKind, Change, DiagnosticData, Fix},
    span,
};

use self::token::{number::CompExponent, NamedIdent, Span, Token};
use self::{figure::SPECIAL_MATH, parser::Type};

mod builtins;
pub mod figure;
pub mod math;
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
    NewLineInString {
        error_span: Span,
    },
    NumberTooLarge {
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
        occurred_span: Span,
        occurred_length: usize,
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
    UndefinedMethod {
        error_span: Span,
        function_name: String,
        suggested: Option<String>,
        on_type: Type,
    },
    UndefinedField {
        error_span: Span,
        field: String,
        on_type: Type,
        suggested: Option<String>,
    },
    NoFieldsOnType {
        error_span: Span,
        on_type: Type,
    },
    FeatureNotSupported {
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
        error_span: Box<Span>,
        got: (Type, Box<Span>),
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
    StringExpected {
        error_span: Span,
    },
    StringOrIdentExpected {
        error_span: Span,
    },
    NonRawStringOrIdentExpected {
        error_span: Span,
    },
    BooleanExpected {
        error_span: Span,
    },
    NumberExpected {
        error_span: Span,
    },
    InvalidIdentMathString {
        error_span: Span,
    },
    RedefinedFlag {
        error_span: Span,
        first_defined: Span,
        flag_name: String,
    },
    EnumInvalidValue {
        error_span: Span,
        available_values: &'static [&'static str],
        received_value: String,
    },
    RequiredFlagNotSet {
        flag_name: &'static str,
        required_because: Span,
        definition_span: Option<Span>,
        available_values: &'static [&'static str],
    },
    ComparisonDoesNotExist {
        error_span: Span,
        ty: Type,
    },
    EmptyLabel {
        error_span: Span,
    },
    UnclosedSpecial {
        error_span: Span,
        parsed_special: String,
    },
    SpecialNotRecognised {
        error_span: Span,
        code: String,
        suggested: Option<String>,
    },
    UnclosedString {
        error_span: Span,
    },
    LabelIndexInsideIndex {
        error_span: Span,
    },
    UnexpectedDisplayOption {
        error_span: Span,
        option: String,
        suggested: Option<String>,
    },
    RepeatedDisplayOption {
        error_span: Span,
        first_span: Span,
        option: String,
    },
    InvalidPC {
        error_span: Span,
    },
    ZeroDenominator {
        error_span: Span,
    },
    ExpectedFunction {
        error_span: Span,
    },
}

impl Error {
    #[must_use]
    pub fn as_implicit_does_not_exist(&self) -> Option<(&Span, &Type, &Type)> {
        match self {
            Self::ImplicitConversionDoesNotExist {
                error_span,
                from,
                to,
            } => Some((error_span, from, to)),
            _ => None,
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
            Self::NewLineInString {
                error_span,
            } => DiagnosticData::new(&"newline in string")
                .add_span(error_span),
            Self::EndOfInput => DiagnosticData::new("unexpected end of input"),
            Self::UndefinedRuleOperator { operator } => {
                DiagnosticData::new(&format!("undefined rule operator: `{}`", operator.ident))
                    .add_span(operator.span)
            }
            Self::InconsistentIterators {
                first_span,
                first_length,
                occurred_span,
                occurred_length,
                error_span,
            } => DiagnosticData::new(&"inconsistent iterator lengths")
                .add_span(error_span)
                .add_annotation(
                    first_span,
                    AnnotationKind::Note,
                    &format!("First iterator with length {first_length} here."),
                )
                .add_annotation(
                    occurred_span,
                    AnnotationKind::Note,
                    &format!("Inconsistency (iterator with length {occurred_length}) here."),
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
                let message = suggested.map(|v| format!("did you mean: `{v}`?"));
                DiagnosticData::new(&format!("undefined variable: `{variable_name}`"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
            Self::UndefinedFunction {
                error_span,
                function_name,
                suggested
            } => {
                let message = suggested.map(|v| format!("did you mean: `{v}`?"));
                DiagnosticData::new(&format!("function `{function_name}` not found"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
            Self::UndefinedMethod {
                error_span,
                function_name,
                suggested,
                on_type
            } => {
                let message = suggested.map(|v| format!("did you mean: `{v}`?"));
                DiagnosticData::new(&format!("method `{function_name}` not found on type {on_type}"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
            Self::UndefinedField {
                error_span,
                field,
                on_type,
                suggested
            } => {
                let message = suggested.map(|v| format!("did you mean: `{v}`?"));
                DiagnosticData::new(&format!("field `{field}` not found on type {on_type}"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
            Self::NoFieldsOnType {
                error_span,
                on_type,
            } => {
                DiagnosticData::new(&format!("there are no fields on type {on_type}"))
                    .add_span(error_span)
            }
            Self::FeatureNotSupported {
                error_span,
                feature_name,
            } => {
                DiagnosticData::new(&format!("feature `{feature_name}` not supported"))
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
                    .add_span(*error_span)
                    .add_annotation(*got.1, AnnotationKind::Note, "this is of invalid type")
            }
            Self::LetStatUnexpectedIterator {
                var_span,
                error_span,
            } => {
                DiagnosticData::new(&"unexpected iterator in right-hand side of `let` statement")
                    .add_span(error_span)
                    .add_annotation(var_span, AnnotationKind::Note, "there was no iterator of left-hand side, so the same is expected for the right")
            }
            Self::IteratorWithSameIdIterator { error_span, parent_span, contained_span } => {
                DiagnosticData::new(&"an iterator with an id of `x` must not contain an iterator with an id of `x`")
                    .add_span(error_span)
                    .add_annotation(parent_span, AnnotationKind::Note, "parent iterator here")
                    .add_annotation(contained_span, AnnotationKind::Note, "child iterator here")
            }
            Self::LetStatMoreThanOneIterator { error_span, first_span, second_span } => {
                DiagnosticData::new(&"right hand side of a let statement must contain at most a single level of iteration")
                    .add_span(error_span)
                    .add_annotation(first_span, AnnotationKind::Note, "first iterator here")
                    .add_annotation(second_span, AnnotationKind::Note, "second iterator here")
            }
            Self::NumberTooLarge { error_span } => {
                DiagnosticData::new(&"number too large")
                    .add_span(error_span)
            }
            Self::SingleVariantExplicitIterator { error_span } => {
                DiagnosticData::new(&"explicit iterators must have at least two variants")
                    .add_span(error_span)
            }
            Self::NonPointInPointCollection { error_span, received } => {
                DiagnosticData::new(&"all values in a point collection constructor must be points")
                    .add_span(error_span)
                    .add_annotation(received.0, AnnotationKind::Note, &format!("value should be a point, received {}", received.1))
            }
            Self::FlagDoesNotExist { flag_name, flag_span, error_span, suggested } => {
                let message = suggested.map(|v| format!("Did you mean: `{v}`?"));
                DiagnosticData::new(&format!("compiler flag `{flag_name}` does not exist"))
                    .add_span(error_span)
                    .add_annotation(flag_span, AnnotationKind::Note, &"This does not exist.")
                    .add_annotation_opt_msg(flag_span, AnnotationKind::Help, message.as_ref())
            }
            Self::FlagSetExpected { error_span } => {
                DiagnosticData::new(&"expected a flag set ({...})")
                    .add_span(error_span)
            }
            Self::StringExpected { error_span } => {
                DiagnosticData::new(&"expected a string")
                    .add_span(error_span)
            }
            Self::StringOrIdentExpected { error_span } => {
                DiagnosticData::new(&"expected a string or an identifier")
                    .add_span(error_span)
            }
            Self::NonRawStringOrIdentExpected { error_span } => {
                DiagnosticData::new(&"expected a non-raw string or an identifier")
                    .add_span(error_span)
            }
            Self::BooleanExpected { error_span } => {
                DiagnosticData::new(&"expected a boolean value (enabled, disabled, on, off, true, false, 1 or 0)")
                    .add_span(error_span)
            }
            Self::NumberExpected { error_span } => {
                DiagnosticData::new(&"expected a number value")
                    .add_span(error_span)
            }
            Self::InvalidIdentMathString { error_span } => {
                DiagnosticData::new(&"invalid ident for a math string")
                    .add_span(error_span)
            }
            Self::RedefinedFlag {
                first_defined,
                error_span,
                flag_name,
            } => DiagnosticData::new(&format!("redefined flag: `{flag_name}`"))
                .add_span(error_span)
                .add_annotation(first_defined, AnnotationKind::Note, "first defined here"),
            Self::EnumInvalidValue { error_span, available_values, received_value } => {
                DiagnosticData::new(&format!("invalid value for an enum flag or property: `{received_value}`"))
                    .add_span(error_span)
                    .add_annotation(error_span, AnnotationKind::Help, &format!("supported values: {}", available_values.iter().map(
                        |v| format!("`{v}`")
                    ).collect::<Vec<String>>().join(", ")))
            }
            Self::RequiredFlagNotSet { flag_name, required_because, definition_span: flagdef_span, available_values } => {
                DiagnosticData::new(&format!("you must set a value for flag `{flag_name}`."))
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
            Self::ComparisonDoesNotExist { error_span, ty } => {
                DiagnosticData::new(&format!("comparison between values of type {ty} does not exist."))
                    .add_span(error_span)
            }
            Self::EmptyLabel { error_span } => {
                DiagnosticData::new(&"labels cannot be empty.")
                    .add_span(error_span)
            }
            Self::UnclosedSpecial { error_span, parsed_special } => {
                // Try to figure out if any of the chars could show up here.
                let found = SPECIAL_MATH.iter().find(|x| parsed_special.starts_with(*x));

                let d = DiagnosticData::new(&"there's a missing ']' somewhere here. Braces denote special characters To escape a brace, use \\[.")
                    .add_span(error_span);

               if let Some(found) = found {
                    d.add_fix(Fix {
                        message: String::from("You may have forgotten to put a `]` here."),
                        changes: vec![
                            Change {
                                span: span!(
                                    error_span.start.line, error_span.start.column + found.len() + 1,
                                    error_span.start.line, error_span.start.column + found.len() + 1
                                ),
                                new_content: vec![
                                    String::from("]")
                                ]
                            }
                        ]
                    })
                } else {
                    d
                }
            }
            Self::SpecialNotRecognised {
                error_span,
                code,
                suggested
            } => {
                let message = suggested.map(|v| format!("Did you mean: `{v}`?"));
                DiagnosticData::new(&format!("special code not recognised: `{code}`"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
            Self::UnclosedString { error_span } => {
                DiagnosticData::new(&"unclosed special tag")
                    .add_span(error_span)
            }
            Self::LabelIndexInsideIndex { error_span } => {
                DiagnosticData::new(&"lower index cannot be used inside another lower index")
                    .add_span(error_span)
            }
            Self::UnexpectedDisplayOption { error_span, option, suggested } => {
                let message = suggested.map(|v| format!("did you mean: `{v}`?"));
                DiagnosticData::new(&format!("unexpected display option: `{option}`"))
                    .add_span(error_span)
                    .add_annotation_opt_msg(error_span, AnnotationKind::Help, message.as_ref())
            }
            Self::RepeatedDisplayOption { error_span, first_span, option } => {
                DiagnosticData::new(&format!("repeated display option: `{option}`"))
                    .add_span(error_span)
                    .add_annotation(first_span, AnnotationKind::Help, &"first defined here")
            }
            Self::InvalidPC { error_span } => {
                DiagnosticData::new(&"point collections in this place are ambiguous and therefore not valid")
                    .add_span(error_span)
            }
            Self::ZeroDenominator { error_span } => {
                DiagnosticData::new(&"denominator in a fraction cannot be equal to zero")
                    .add_span(error_span)
            }
            Self::ExpectedFunction { error_span } => {
                DiagnosticData::new(&"expected function, found, value")
                    .add_span(error_span)
            }
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

const fn unit_count() -> usize {
    SimpleUnit::Scalar as usize
}

/// Defines a complex unit: a product of simple units.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize, Default)]
pub struct ComplexUnit([CompExponent; unit_count()]);

pub mod unit {
    use super::{ComplexUnit, SimpleUnit};

    pub const DISTANCE: ComplexUnit = ComplexUnit::new(SimpleUnit::Distance);
    pub const ANGLE: ComplexUnit = ComplexUnit::new(SimpleUnit::Angle);
    pub const SCALAR: ComplexUnit = ComplexUnit::new(SimpleUnit::Scalar);
}

pub mod ty {
    use super::{parser::Type, ComplexUnit, SimpleUnit};

    pub const DISTANCE: Type = Type::Scalar(Some(ComplexUnit::new(SimpleUnit::Distance)));
    pub const POINT: Type = Type::Point;
    pub const ANGLE: Type = Type::Scalar(Some(ComplexUnit::new(SimpleUnit::Angle)));
    pub const LINE: Type = Type::Line;
    pub const CIRCLE: Type = Type::Circle;
    pub const SCALAR: Type = Type::Scalar(Some(ComplexUnit::new(SimpleUnit::Scalar)));
    pub const SCALAR_UNKNOWN: Type = Type::Scalar(None);

    #[must_use]
    pub const fn collection(length: usize) -> Type {
        Type::PointCollection(length)
    }

    #[must_use]
    pub const fn bundle(t: &'static str) -> Type {
        Type::Bundle(t)
    }
}

impl ComplexUnit {
    #[must_use]
    pub const fn new(simple: SimpleUnit) -> Self {
        let mut arr = [CompExponent::new_raw(0, 1); unit_count()];

        match simple {
            SimpleUnit::Scalar => (),
            _ => arr[simple as usize] = CompExponent::new_raw(1, 1),
        }

        Self(arr)
    }

    #[must_use]
    pub fn pow(mut self, exp: CompExponent) -> Self {
        for v in &mut self.0 {
            *v *= exp;
        }

        self
    }
}

impl Display for ComplexUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        for i in 0..(SimpleUnit::Scalar as usize) {
            if !self.0[i].is_zero() {
                let name = match i {
                    0 => "Distance",
                    1 => "Point",
                    2 => "Angle",
                    3 => "Line",
                    _ => unreachable!(),
                };

                if self.0[i].is_one() {
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
    type Target = [CompExponent; unit_count()];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ComplexUnit {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

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
        std::ptr::addr_eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0))
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
