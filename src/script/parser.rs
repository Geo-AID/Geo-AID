use geo_aid_derive::Parse;
use num_traits::Zero;

use crate::script::builtins;
use crate::script::token::number::ProcNum;
use std::fmt::Formatter;
use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    marker::PhantomData,
};

use crate::span;

use super::{
    token::{
        number::CompExponent, Ampersant, Asterisk, At, Caret, Colon, Comma, Dollar, Dot, Eq,
        Exclamation, Gt, Gteq, Ident, LBrace, LParen, LSquare, Let, Lt, Lteq, Minus, NamedIdent,
        Number, Plus, Question, RBrace, RParen, RSquare, Semi, Slash, Span, StrLit, TokInteger,
        Token,
    },
    unit, ComplexUnit, Error,
};

pub trait Parse {
    type FirstToken: CheckParses;

    /// # Errors
    /// Returns an error if parsing was unsuccessful.
    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &mut InputStream<'t, I>,
    ) -> Result<Self, Error>
    where
        Self: Sized;

    fn get_span(&self) -> Span;
}

pub trait CheckParses {
    fn check_parses<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &InputStream<'t, I>,
    ) -> Option<bool>;
}

impl<T: Parse> CheckParses for T {
    fn check_parses<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &InputStream<'t, I>,
    ) -> Option<bool> {
        Some(input.clone().parse::<Self>().is_ok())
    }
}

#[derive(Debug)]
pub struct TokenOr<T, U> {
    phantom_t: PhantomData<T>,
    phantom_u: PhantomData<U>,
}

impl<T: CheckParses, U: CheckParses> CheckParses for TokenOr<T, U> {
    fn check_parses<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &InputStream<'t, I>,
    ) -> Option<bool> {
        T::check_parses(input).or_else(|| U::check_parses(input))
    }
}

#[derive(Debug)]
pub struct Maybe<T> {
    phantom_t: PhantomData<T>,
}

impl<T: CheckParses> CheckParses for Maybe<T> {
    fn check_parses<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &InputStream<'t, I>,
    ) -> Option<bool> {
        T::check_parses(input).and_then(|x| if x { Some(x) } else { None })
    }
}

impl<T: Parse> Parse for Vec<T> {
    type FirstToken = Maybe<T::FirstToken>;

    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &mut InputStream<'t, I>,
    ) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let mut parsed = Self::new();

        while let Ok(Some(v)) = input.parse() {
            parsed.push(v);
        }

        Ok(parsed)
    }

    fn get_span(&self) -> Span {
        self.first().map_or(Span::empty(), |x| {
            x.get_span().join(self.last().unwrap().get_span())
        })
    }
}

impl<T: Parse, U: Parse> Parse for (T, U) {
    type FirstToken = T::FirstToken;

    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &mut InputStream<'t, I>,
    ) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Ok((input.parse()?, input.parse()?))
    }

    fn get_span(&self) -> Span {
        self.0.get_span().join(self.1.get_span())
    }
}

impl<T: Parse> Parse for Option<T> {
    type FirstToken = Maybe<T::FirstToken>;

    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &mut InputStream<'t, I>,
    ) -> Result<Self, Error>
    where
        Self: Sized,
    {
        if T::FirstToken::check_parses(input) == Some(false) {
            Ok(None)
        } else {
            Ok(Some(input.parse()?))
        }
    }

    fn get_span(&self) -> Span {
        match self {
            Some(v) => v.get_span(),
            None => span!(0, 0, 0, 0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InputStream<'t, I: Iterator<Item = &'t Token> + Clone> {
    it: Peekable<I>,
}

impl<'t, I: Iterator<Item = &'t Token> + Clone> InputStream<'t, I> {
    #[must_use]
    pub fn new<It: IntoIterator<IntoIter = I>>(it: It) -> Self {
        Self {
            it: it.into_iter().peekable(),
        }
    }

    /// # Errors
    /// Returns an error if failed to parse
    pub fn parse<P: Parse>(&mut self) -> Result<P, Error> {
        P::parse(self)
    }

    /// # Errors
    /// Returns an EOF error if there is no next token.
    pub fn get_token(&mut self) -> Result<&'t Token, Error> {
        self.it.next().ok_or(Error::EndOfInput)
    }

    /// # Errors
    /// Returns an EOF error if there is no next token. Does not advance the inner iterator.
    pub fn expect_token(&mut self) -> Result<(), Error> {
        if self.eof() {
            Err(Error::EndOfInput)
        } else {
            Ok(())
        }
    }

    #[must_use]
    pub fn eof(&mut self) -> bool {
        self.it.peek().is_none()
    }
}

/// A binary operator, like `+`, `-`, `*` or `/`.
#[derive(Debug, Parse)]
pub enum BinaryOperator {
    /// Addition
    Add(Plus),
    /// Subtraction
    Sub(Minus),
    /// Multiplication
    Mul(Asterisk),
    /// Division
    Div(Slash),
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add(_) => write!(f, "+"),
            BinaryOperator::Sub(_) => write!(f, "-"),
            BinaryOperator::Mul(_) => write!(f, "*"),
            BinaryOperator::Div(_) => write!(f, "/"),
        }
    }
}

#[derive(Debug, Parse)]
#[parse(first_token = Ampersant)]
pub struct PointCollectionConstructor {
    pub ampersant: Ampersant,
    pub left_paren: LParen,
    pub points: Punctuated<Expression<false>, Comma>,
    pub right_paren: RParen,
}

/// Punctuated expressions.
#[derive(Debug)]
pub struct ImplicitIterator<const ITER: bool> {
    pub exprs: Punctuated<SimpleExpression, Comma>,
}

impl<const ITER: bool> ImplicitIterator<ITER> {
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&SimpleExpression> {
        if ITER {
            self.exprs.get(index)
        } else {
            Some(&self.exprs.first)
        }
    }
}

impl<const ITER: bool> Parse for ImplicitIterator<ITER> {
    type FirstToken = <SimpleExpression as Parse>::FirstToken;

    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &mut InputStream<'t, I>,
    ) -> Result<Self, Error>
    where
        Self: Sized,
    {
        if ITER {
            Ok(Self {
                exprs: input.parse()?,
            })
        } else {
            Ok(Self {
                exprs: Punctuated {
                    first: input.parse()?,
                    collection: Vec::new(),
                },
            })
        }
    }

    fn get_span(&self) -> Span {
        self.exprs.get_span()
    }
}

/// $id(a, b, ...).
#[derive(Debug)]
pub struct ExplicitIterator {
    pub dollar: Dollar,
    pub id_token: TokInteger,
    pub id: u8,
    pub left_paren: LParen,
    pub exprs: Punctuated<Expression<false>, Comma>,
    pub right_paren: RParen,
}

impl ExplicitIterator {
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&Expression<false>> {
        self.exprs.get(index)
    }
}

impl Parse for ExplicitIterator {
    type FirstToken = Dollar;

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        input: &mut InputStream<'r, I>,
    ) -> Result<Self, Error> {
        let mut parsed = Self {
            dollar: input.parse()?,
            id_token: input.parse()?,
            id: 0,
            left_paren: input.parse()?,
            exprs: input.parse()?,
            right_paren: input.parse()?,
        };

        parsed.id = parsed
            .id_token
            .parsed
            .parse()
            .map_err(|_| Error::NumberTooLarge {
                error_span: parsed.id_token.get_span(),
            })?;

        if parsed.exprs.len() == 1 {
            return Err(Error::SingleVariantExplicitIterator {
                error_span: parsed.dollar.span.join(parsed.right_paren.span),
            });
        }

        Ok(parsed)
    }

    fn get_span(&self) -> Span {
        self.dollar.span.join(self.right_paren.span)
    }
}

/// A parsed expression.
#[derive(Debug)]
pub enum Expression<const ITER: bool> {
    /// Simple values separated by a comma.
    ImplicitIterator(ImplicitIterator<ITER>),
    /// A binary operator expression.
    Binop(ExprBinop<ITER>),
}

impl<const ITER: bool> Parse for Expression<ITER> {
    type FirstToken = <SimpleExpression as Parse>::FirstToken;

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        input: &mut InputStream<'r, I>,
    ) -> Result<Self, Error> {
        let mut expr = Expression::ImplicitIterator(input.parse()?);

        while let Ok(Some(op)) = input.parse() {
            let rhs = Expression::ImplicitIterator(input.parse()?);

            expr = dispatch_order(expr, op, rhs);
        }

        Ok(expr)
    }

    fn get_span(&self) -> Span {
        match self {
            Expression::ImplicitIterator(it) => it.get_span(),
            Expression::Binop(e) => e.lhs.get_span().join(e.rhs.get_span()),
        }
    }
}

/// A rational exponent.
#[derive(Debug)]
pub struct RationalExponent {
    pub lparen: LParen,
    pub nom: TokInteger,
    pub slash: Slash,
    pub denom: TokInteger,
    pub rparen: RParen,
}

impl Parse for RationalExponent {
    type FirstToken = LParen;

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        input: &mut InputStream<'r, I>,
    ) -> Result<Self, Error> {
        let parsed = Self {
            lparen: input.parse()?,
            nom: input.parse()?,
            slash: input.parse()?,
            denom: input.parse()?,
            rparen: input.parse()?,
        };

        if parsed.denom.parsed.is_zero() {
            return Err(Error::ZeroDenominator {
                error_span: parsed.denom.span,
            });
        }

        Ok(parsed)
    }

    fn get_span(&self) -> Span {
        self.lparen.span.join(self.rparen.span)
    }
}

/// An integer or a ratio.
#[derive(Debug, Parse)]
pub enum Exponent {
    Simple(TokInteger),
    Parenthesized(RationalExponent),
}

impl Exponent {
    /// # Errors
    /// Returns an error if the numbers can't fit.
    pub fn as_comp(&self) -> Result<CompExponent, Error> {
        match self {
            Self::Simple(i) => Ok(CompExponent::new(
                i.parsed
                    .parse()
                    .map_err(|_| Error::NumberTooLarge { error_span: i.span })?,
                1,
            )),
            Self::Parenthesized(exp) => Ok(CompExponent::new(
                exp.nom.parsed.parse().map_err(|_| Error::NumberTooLarge {
                    error_span: exp.nom.span,
                })?,
                exp.denom
                    .parsed
                    .parse()
                    .map_err(|_| Error::NumberTooLarge {
                        error_span: exp.denom.span,
                    })?,
            )),
        }
    }
}

/// A value being raised to a rational power.
#[derive(Debug, Parse)]
pub struct Exponentiation {
    /// Caret token
    pub caret: Caret,
    /// Possible negation
    pub minus: Option<Minus>,
    /// The exponent.
    pub exponent: Exponent,
}

#[derive(Debug, Parse)]
pub struct FieldIndex {
    /// The indexed thing
    pub name: Box<Name>,
    /// The dot
    pub dot: Dot,
    /// The field
    pub field: Ident,
}

/// A name (field, method or variable).
#[derive(Debug)]
pub enum Name {
    // Variant order matters, as it defines the parsing priority
    Call(ExprCall),
    FieldIndex(FieldIndex),
    Ident(Ident),
    Expression(ExprParenthesised),
}

impl Parse for Name {
    type FirstToken = TokenOr<Maybe<Ident>, LParen>;

    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
        input: &mut InputStream<'t, I>,
    ) -> Result<Self, Error>
    where
        Self: Sized,
    {
        let mut name = if let Some(expr) = input.parse()? {
            Self::Expression(expr)
        } else {
            Self::Ident(input.parse()?)
        };

        loop {
            name = if let Some(dot) = input.parse::<Option<Dot>>()? {
                Self::FieldIndex(FieldIndex {
                    name: Box::new(name),
                    dot,
                    field: input.parse()?,
                })
            } else if let Some(lparen) = input.parse::<Option<LParen>>()? {
                Self::Call(ExprCall {
                    name: Box::new(name),
                    lparen,
                    params: input.parse()?,
                    rparen: input.parse()?,
                })
            } else {
                break Ok(name);
            };
        }
    }

    fn get_span(&self) -> Span {
        match self {
            Self::Call(v) => v.get_span(),
            Self::FieldIndex(v) => v.get_span(),
            Self::Expression(v) => v.get_span(),
            Self::Ident(v) => v.get_span(),
        }
    }
}

/// A parsed simple expression.
#[derive(Debug, Parse)]
pub struct SimpleExpression {
    /// Possible minus for negation.
    pub minus: Option<Minus>,
    /// The kind of the expression.
    pub kind: SimpleExpressionKind,
    /// Possible exponentiation.
    pub exponent: Option<Exponentiation>,
    /// The additional display information.
    pub display: Option<DisplayProperties>,
}

/// A parsed simple expression.
#[derive(Debug, Parse)]
pub enum SimpleExpressionKind {
    /// A named (variable, field or function call)
    Name(Name),
    /// A raw number
    Number(Number),
    /// An explicit iterator.
    ExplicitIterator(ExplicitIterator),
    /// A point collection construction
    PointCollection(PointCollectionConstructor),
}

/// A parsed function call
#[derive(Debug, Parse)]
pub struct ExprCall {
    /// The called thing.
    pub name: Box<Name>,
    /// The `(` token.
    pub lparen: LParen,
    /// Punctuated params. `None` if no params are given.
    pub params: Option<Punctuated<Expression<false>, Comma>>,
    /// The `)` token.
    pub rparen: RParen,
}

/// A parsed parenthesised expression
#[derive(Debug, Parse)]
pub struct ExprParenthesised {
    /// The `(` token.
    pub lparen: LParen,
    /// The contained `Expression`.
    pub content: Box<Expression<true>>,
    /// The `)` token.
    pub rparen: RParen,
}

/// A parsed binary operator expression.
#[derive(Debug, Parse)]
pub struct ExprBinop<const ITER: bool> {
    /// Left hand side
    pub lhs: Box<Expression<ITER>>,
    /// The operator
    pub operator: BinaryOperator,
    /// Right hand side.
    pub rhs: Box<Expression<ITER>>,
}

impl BinaryOperator {
    fn index(&self) -> u8 {
        match self {
            BinaryOperator::Add(_) | BinaryOperator::Sub(_) => 1,
            BinaryOperator::Mul(_) | BinaryOperator::Div(_) => 2,
        }
    }
}

/// Inserts an operator with an rhs into an operator series, considering the order of operations.
fn dispatch_order<const ITER: bool>(
    lhs: Expression<ITER>,
    op: BinaryOperator,
    rhs: Expression<ITER>, // We have to trust, that it is a valid expression.
) -> Expression<ITER> {
    match lhs {
        // if lhs is simple, there is no order to consider.
        lhs @ Expression::ImplicitIterator(_) => Expression::Binop(ExprBinop {
            lhs: Box::new(lhs),
            operator: op,
            rhs: Box::new(rhs),
        }),
        // Otherwise we compare indices of the operators and act accordingly.
        Expression::Binop(lhs) => {
            if op.index() > lhs.operator.index() {
                Expression::Binop(ExprBinop {
                    lhs: lhs.lhs,
                    operator: lhs.operator,
                    rhs: Box::new(dispatch_order(*lhs.rhs, op, rhs)),
                })
            } else {
                Expression::Binop(ExprBinop {
                    lhs: Box::new(Expression::Binop(lhs)),
                    operator: op,
                    rhs: Box::new(rhs),
                })
            }
        }
    }
}

/// A builtin rule operator
#[derive(Debug, Parse)]
pub enum PredefinedRuleOperator {
    /// Equality
    Eq(Eq),
    /// Less than
    Lt(Lt),
    /// Greater than
    Gt(Gt),
    /// Less than or equal
    Lteq(Lteq),
    /// Greater than or equal
    Gteq(Gteq),
}

/// A rule operator.
#[derive(Debug, Parse)]
pub enum RuleOperator {
    /// An inverted rule operator (!op)
    Inverted(InvertedRuleOperator),
    Predefined(PredefinedRuleOperator),
    Defined(NamedIdent),
}

/// An inverted rule operator.
#[derive(Debug, Parse)]
#[parse(first_token = Exclamation)]
pub struct InvertedRuleOperator {
    /// The `!` token
    pub exclamation: Exclamation,
    /// The operator.
    pub operator: Box<RuleOperator>,
}

/// Defines the first half of a flag statement.
#[derive(Debug, Parse)]
pub struct FlagName {
    pub at: At,
    pub name: Punctuated<NamedIdent, Dot>,
    pub colon: Colon,
}

/// A set of flags.
#[derive(Debug, Parse)]
#[parse(first_token = LBrace)]
pub struct FlagSet {
    pub lbrace: LBrace,
    pub flags: Vec<FlagStatement>,
    pub rbrace: RBrace,
}

/// Defines the second half of a flag statement.
#[derive(Debug, Parse)]
pub enum FlagValue {
    Ident(NamedIdent),
    Set(FlagSet),
    Number(Number),
}

/// Defines a compiler flag or flagset.
#[derive(Debug, Parse)]
pub struct FlagStatement {
    pub name: FlagName,
    pub value: FlagValue,
}

/// A single variable definition. Contains its name and optional display properties
#[derive(Debug, Clone, Parse)]
pub struct VariableDefinition {
    /// Name of the variable.
    pub name: Ident,
    /// Display properties.
    pub display_properties: Option<DisplayProperties>,
}

/// `let <something> = <something else>`.
/// Defines variables and possibly adds rules to them.
#[derive(Debug, Parse)]
pub struct LetStatement {
    /// The `let` token.
    pub let_token: Let,
    /// The lhs ident iterator.
    pub ident: Punctuated<VariableDefinition, Comma>,
    /// The `=` token.
    pub eq: Eq,
    /// The rhs expression.
    pub expr: Expression<true>,
    /// The rules after the rhs expression.
    pub rules: Vec<(RuleOperator, Expression<true>)>,
    /// The ending semicolon.
    pub semi: Semi,
}

/// `lhs ruleop rhs`.
/// Defines a rule.
#[derive(Debug, Parse)]
pub struct RuleStatement {
    /// Display properties.
    pub display: Option<DisplayProperties>,
    /// Left hand side
    pub lhs: Expression<true>,
    /// Rule operator
    pub op: RuleOperator,
    /// Right hand side
    pub rhs: Expression<true>,
    /// The ending semicolon.
    pub semi: Semi,
}

/// `?expr`
#[derive(Debug, Parse)]
pub struct RefStatement {
    /// Display properties.
    pub display: Option<DisplayProperties>,
    /// The starting question mark.
    pub question: Question,
    /// Operand.
    pub operand: Expression<true>,
    /// The ending semicolon.
    pub semi: Semi,
}

/// A general statement.
#[derive(Debug, Parse)]
pub enum Statement {
    /// No operation
    Noop(Semi),
    /// let
    Let(LetStatement),
    /// Flag
    Flag(FlagStatement),
    /// Reference
    Ref(RefStatement),
    /// rule
    Rule(RuleStatement),
}

impl Statement {
    #[must_use]
    pub fn as_flag(&self) -> Option<&FlagStatement> {
        if let Self::Flag(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A utility struct for collections of parsed items with punctuators between them.
#[derive(Debug, Clone, Parse)]
pub struct Punctuated<T: Parse, P: Parse> {
    /// The first parsed item.
    pub first: Box<T>,
    /// The next items with punctuators.
    pub collection: Vec<(P, T)>,
}

impl<T: Parse, P: Parse> Punctuated<T, P> {
    /// Creates a new instance of `Punctuated`.
    #[must_use]
    pub fn new(first: T) -> Punctuated<T, P> {
        Self {
            first: Box::new(first),
            collection: Vec::new(),
        }
    }

    /// Turns the punctuated into an iterator on the items.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        vec![self.first.as_ref()]
            .into_iter()
            .chain(self.collection.iter().map(|x| &x.1))
    }

    /// Turns the punctuated into an iterator on the items.
    pub fn into_parsed_iter(self) -> impl Iterator<Item = T> {
        vec![*self.first]
            .into_iter()
            .chain(self.collection.into_iter().map(|x| x.1))
    }

    /// Gets the item count.
    #[must_use]
    pub fn len(&self) -> usize {
        self.collection.len() + 1
    }

    /// Checks if there are no items (always false).
    #[must_use]
    pub fn is_empty(&self) -> bool {
        false
    }

    /// Tries to get the element on `index`.
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&T> {
        match index {
            0 => Some(&self.first),
            _ => self.collection.get(index - 1).map(|x| &x.1),
        }
    }
}

impl Parse for TokInteger {
    type FirstToken = Number;

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        input: &mut InputStream<'r, I>,
    ) -> Result<Self, Error> {
        match input.get_token()? {
            Token::Number(Number::Integer(tok)) => Ok(tok.clone()),
            t => Err(Error::InvalidToken { token: t.clone() }),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for NamedIdent {
    type FirstToken = Ident;

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        input: &mut InputStream<'r, I>,
    ) -> Result<Self, Error> {
        match input.get_token()? {
            Token::Ident(Ident::Named(named)) => Ok(named.clone()),
            t => Err(Error::InvalidToken { token: t.clone() }),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl<T: Parse> Parse for Box<T> {
    type FirstToken = T::FirstToken;

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        input: &mut InputStream<'r, I>,
    ) -> Result<Self, Error> {
        Ok(Box::new(input.parse()?))
    }

    fn get_span(&self) -> Span {
        (**self).get_span()
    }
}

/// A builtin type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// A point
    Point,
    /// A line
    Line,
    /// A scalar of a certain unit.
    Scalar(Option<ComplexUnit>),
    /// A point collection.
    PointCollection(usize),
    /// A circle
    Circle,
    /// A bundle type.
    Bundle(&'static str),
    /// Marks unknown type. Unknown type pretends to be valid, but isn't really.
    Unknown,
}

impl Type {
    #[must_use]
    pub fn as_scalar(&self) -> Option<&Option<ComplexUnit>> {
        if let Self::Scalar(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A user-defined type.
pub struct DefinedType {
    /// The type's name.
    pub name: String,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Point => write!(f, "Point"),
            Self::Line => write!(f, "Line"),
            Self::Scalar(unit) => match unit {
                Some(unit) => write!(f, "Scalar ({unit})"),
                None => write!(f, "Scalar (no unit)"),
            },
            Self::PointCollection(l) => write!(f, "Point collection ({l})"),
            Self::Circle => write!(f, "Circle"),
            Self::Bundle(name) => write!(f, "{name}"),
            Type::Unknown => write!(f, "undefined"),
        }
    }
}

impl Type {
    /// Whether `self` can be cast to `into`.
    #[must_use]
    pub fn can_cast(&self, into: &Type) -> bool {
        match self {
            // A point can only be cast into another point or a point collection with length one.
            Type::Point => matches!(into, Type::Point | Type::PointCollection(1)),
            // A line can only be cast into another line.
            Type::Line => matches!(into, Type::Line),
            // A scalar with a defined unit can only be cast into another scalar with the same unit.
            Type::Scalar(Some(unit1)) => {
                if let Type::Scalar(Some(unit2)) = into {
                    unit1 == unit2
                } else {
                    false
                }
            }
            // A scalar with no defined unit can be cast into any other scalar, except angle.
            Type::Scalar(None) => match into {
                Type::Scalar(unit) => match unit {
                    Some(unit) => unit.0[1].is_zero(), // no angle
                    None => true,
                },
                _ => false,
            },
            Type::PointCollection(l) => match into {
                Type::Point => *l == 1,
                Type::Line => *l == 2,
                Type::Scalar(Some(u)) => *u == unit::DISTANCE && *l == 2,
                Type::PointCollection(v) => v == l || *v == 0,
                _ => false,
            },
            Type::Circle => matches!(into, Type::Circle),
            Type::Bundle(name) => {
                if into == self {
                    true
                } else if let Type::PointCollection(count) = into {
                    builtins::get_bundle_pc(name) == *count
                } else {
                    false
                }
            }
            Type::Unknown => false,
        }
    }
}

/// A property
#[derive(Debug, Clone, Parse)]
pub struct Property {
    /// Property name.
    pub name: NamedIdent,
    /// '='
    pub eq: Eq,
    /// Property value.
    pub value: PropertyValue,
}

/// A property's value
#[derive(Debug, Clone, Parse)]
pub enum PropertyValue {
    Number(Number),
    Ident(Ident),
    RawString(RawString),
    String(StrLit),
}

impl Display for PropertyValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::Ident(i) => write!(f, "{i}"),
            Self::RawString(s) => write!(f, "!{}", s.lit),
            Self::String(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Clone, Parse)]
pub struct RawString {
    pub excl: Exclamation,
    pub lit: StrLit,
}

pub trait FromProperty: Sized {
    /// # Errors
    /// Causes an error if the value is not properly convertible.
    fn from_property(property: PropertyValue) -> Result<Self, Error>;
}

impl<T: FromProperty> FromProperty for Result<T, Error> {
    fn from_property(property: PropertyValue) -> Result<Self, Error> {
        Ok(T::from_property(property))
    }
}

impl FromProperty for bool {
    fn from_property(property: PropertyValue) -> Result<Self, Error> {
        match property {
            PropertyValue::Ident(ident) => match ident {
                Ident::Named(ident) => match ident.ident.as_str() {
                    "enabled" | "on" | "true" | "yes" => Ok(true),
                    "disabled" | "off" | "false" | "no" => Ok(false),
                    _ => Err(Error::BooleanExpected {
                        error_span: ident.get_span(),
                    }),
                },
                Ident::Collection(_) => Err(Error::BooleanExpected {
                    error_span: ident.get_span(),
                }),
            },
            PropertyValue::Number(num) => match num {
                Number::Integer(i) => match i.parsed.parse::<u8>() {
                    Ok(0) => Ok(false),
                    Ok(1) => Ok(true),
                    _ => Err(Error::BooleanExpected { error_span: i.span }),
                },
                Number::Float(f) => Err(Error::BooleanExpected { error_span: f.span }),
            },
            PropertyValue::String(s) => match s.content.as_str() {
                "enabled" | "on" | "true" | "yes" => Ok(true),
                "disabled" | "off" | "false" | "no" => Ok(false),
                _ => Err(Error::BooleanExpected {
                    error_span: s.get_span(),
                }),
            },
            PropertyValue::RawString(s) => Err(Error::BooleanExpected {
                error_span: s.get_span(),
            }),
        }
    }
}

impl FromProperty for String {
    fn from_property(property: PropertyValue) -> Result<String, Error> {
        match property {
            PropertyValue::Ident(ident) => Ok(ident.to_string()),
            PropertyValue::Number(num) => Err(Error::StringExpected {
                error_span: num.get_span(),
            }),
            PropertyValue::RawString(s) => Ok(s.lit.content),
            PropertyValue::String(s) => Ok(s.content),
        }
    }
}

impl FromProperty for ProcNum {
    fn from_property(property: PropertyValue) -> Result<Self, Error> {
        match property {
            PropertyValue::Number(num) => Ok(ProcNum::from(&num)),
            PropertyValue::RawString(s) => Err(Error::NumberExpected {
                error_span: s.get_span(),
            }),
            PropertyValue::String(s) => Err(Error::NumberExpected {
                error_span: s.get_span(),
            }),
            PropertyValue::Ident(ident) => Err(Error::NumberExpected {
                error_span: ident.get_span(),
            }),
        }
    }
}

/// Properties related to displaying things.
#[derive(Debug, Clone, Parse)]
pub struct DisplayProperties {
    /// '['
    pub lsquare: LSquare,
    /// Properties
    pub properties: Punctuated<Property, Semi>,
    /// ']'
    pub rsquare: RSquare,
}
