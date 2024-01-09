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

use num_traits::Zero;

use crate::script::builtins;
use std::{
    fmt::{Debug, Display},
    iter::Peekable,
};

use crate::span;

use super::{
    token::{
        number::CompExponent, Ampersant, Asterisk, At, Caret, Colon, Comma, Dollar, Dot, Eq,
        Exclamation, Gt, Gteq, Ident, LBrace, LParen, LSquare, Let, Lt, Lteq, Minus, NamedIdent,
        Number, Plus, Question, RBrace, RParen, RSquare, Semi, Slash, Span, StrLit, TokInteger,
        Token, Vertical,
    },
    unit, ComplexUnit, Error,
};

macro_rules! impl_token_parse {
    ($token:ident) => {
        impl Parse for $token {
            fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
                it: &mut Peekable<I>,
            ) -> Result<Self, Error> {
                match it.next() {
                    Some(Token::$token(tok)) => Ok(*tok),
                    Some(t) => Err(Error::InvalidToken { token: t.clone() }),
                    None => Err(Error::EndOfInput),
                }
            }

            fn get_span(&self) -> Span {
                self.span
            }
        }
    };
}

/// A unary operator, like `-`.
#[derive(Debug)]
pub enum UnaryOperator {
    /// A negation, as in `-x`.
    Neg(NegOp),
}

impl UnaryOperator {
    /// Gets the type that this operator returns.
    #[must_use]
    pub fn get_returned(&self, param: &Type) -> Type {
        match self {
            UnaryOperator::Neg(_) => match param {
                Type::Point
                | Type::Line
                | Type::Circle
                | Type::Unknown
                | Type::Bundle(_)
                | Type::PointCollection(_) => Type::Unknown,
                t @ Type::Scalar(_) => *t,
            },
        }
    }
}

/// A parsed unary `-` operator.
#[derive(Debug)]
pub struct NegOp {
    /// The `-` token.
    pub minus: Minus,
}

/// A parsed `+` operator.
#[derive(Debug)]
pub struct AddOp {
    //. The `+` token.
    pub plus: Plus,
}

/// A parsed `-` operator.
#[derive(Debug)]
pub struct SubOp {
    //. The `-` token.
    pub minus: Minus,
}

/// A parsed `*` operator.
#[derive(Debug)]
pub struct MulOp {
    //. The `*` token.
    pub asterisk: Asterisk,
}

/// A parsed `/` operator.
#[derive(Debug)]
pub struct DivOp {
    //. The `/` token.
    pub slash: Slash,
}

/// A binary operator, like `+`, `-`, `*` or `/`.
#[derive(Debug)]
pub enum BinaryOperator {
    /// Addition
    Add(AddOp),
    /// Subtraction
    Sub(SubOp),
    /// Multiplication
    Mul(MulOp),
    /// Division
    Div(DivOp),
}

impl ToString for BinaryOperator {
    fn to_string(&self) -> String {
        match self {
            BinaryOperator::Add(_) => String::from("+"),
            BinaryOperator::Sub(_) => String::from("-"),
            BinaryOperator::Mul(_) => String::from("*"),
            BinaryOperator::Div(_) => String::from("/"),
        }
    }
}

#[derive(Debug)]
pub struct PointCollectionConstructor {
    pub ampersant: Ampersant,
    pub left_paren: LParen,
    pub points: Punctuated<Expression<false>, Comma>,
    pub right_paren: RParen,
}

impl Parse for PointCollectionConstructor {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            ampersant: Ampersant::parse(it)?,
            left_paren: LParen::parse(it)?,
            points: Punctuated::parse(it)?,
            right_paren: RParen::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.ampersant.span.join(self.right_paren.span)
    }
}

/// Punctuated expressions.
#[derive(Debug)]
pub struct ImplicitIterator {
    pub exprs: Punctuated<SimpleExpression, Comma>,
}

impl ImplicitIterator {
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&SimpleExpression> {
        self.exprs.get(index)
    }
}

/// $id(a, b, ...).
#[derive(Debug)]
pub struct ExplicitIterator {
    pub exprs: Punctuated<Expression<false>, Comma>,
    pub id_token: TokInteger,
    pub id: u8,
    pub dollar: Dollar,
    pub left_paren: LParen,
    pub right_paren: RParen,
}

impl ExplicitIterator {
    #[must_use]
    pub fn get(&self, index: usize) -> Option<&Expression<false>> {
        self.exprs.get(index)
    }
}

impl Parse for ImplicitIterator {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(ImplicitIterator {
            exprs: Punctuated::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.exprs.get_span()
    }
}

impl Parse for ExplicitIterator {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let dollar = Dollar::parse(it)?;
        let id_token = TokInteger::parse(it)?;
        let left_paren = LParen::parse(it)?;
        let exprs = Punctuated::parse(it)?;
        let right_paren = RParen::parse(it)?;

        if exprs.len() == 1 {
            return Err(Error::SingleVariantExplicitIterator {
                error_span: dollar.span.join(right_paren.span),
            });
        }

        let id = id_token.parsed.parse().map_err(|_| Error::NumberTooLarge {
            error_span: id_token.get_span(),
        })?;

        Ok(ExplicitIterator {
            exprs,
            id_token,
            id,
            dollar,
            left_paren,
            right_paren,
        })
    }

    fn get_span(&self) -> Span {
        self.dollar.span.join(self.right_paren.span)
    }
}

/// A parsed expression.
#[derive(Debug)]
pub enum Expression<const ITER: bool> {
    /// Simple values separated by a comma.
    ImplicitIterator(ImplicitIterator),
    /// A single simple expression
    Single(Box<SimpleExpression>),
    /// A binary operator expression.
    Binop(ExprBinop<ITER>),
}

impl<const ITER: bool> Expression<ITER> {
    /// Returns `true` if the expression is [`Single`].
    ///
    /// [`Single`]: Expression::Single
    #[must_use]
    pub fn is_single(&self) -> bool {
        matches!(self, Self::Single(..))
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
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let parsed = Self {
            lparen: LParen::parse(it)?,
            nom: TokInteger::parse(it)?,
            slash: Slash::parse(it)?,
            denom: TokInteger::parse(it)?,
            rparen: RParen::parse(it)?,
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
#[derive(Debug)]
pub enum Exponent {
    Simple(TokInteger),
    Parenthised(RationalExponent),
}

impl Exponent {
    /// # Errors
    /// Returns an error if the numbers can't fit.
    pub fn into_comp(&self) -> Result<CompExponent, Error> {
        match self {
            Self::Simple(i) => Ok(CompExponent::new(
                i.parsed
                    .parse()
                    .map_err(|_| Error::NumberTooLarge { error_span: i.span })?,
                1,
            )),
            Self::Parenthised(exp) => Ok(CompExponent::new(
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

impl Parse for Exponent {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let peeked = it.peek().copied();

        Ok(match peeked {
            Some(Token::Number(Number::Integer(_))) => Self::Simple(TokInteger::parse(it)?),
            Some(Token::LParen(_)) => Self::Parenthised(RationalExponent::parse(it)?),
            Some(t) => return Err(Error::InvalidToken { token: t.clone() }),
            None => return Err(Error::EndOfInput),
        })
    }

    fn get_span(&self) -> Span {
        match self {
            Self::Simple(v) => v.span,
            Self::Parenthised(v) => v.get_span(),
        }
    }
}

/// A value being raised to a rational power.
#[derive(Debug)]
pub struct Exponentiation {
    /// The raised value.
    pub base: Box<SimpleExpressionKind>,
    /// Caret token
    pub caret: Caret,
    /// Possible negation
    pub minus: Option<Minus>,
    /// The exponent.
    pub exponent: Exponent,
}

impl Parse for Exponentiation {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            base: Box::parse(it)?,
            caret: Caret::parse(it)?,
            minus: Option::parse(it)?,
            exponent: Exponent::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.base.get_span().join(self.exponent.get_span())
    }
}

/// A parsed simple expression.
#[derive(Debug)]
pub struct SimpleExpression {
    /// The kind of the expression.
    pub kind: SimpleExpressionKind,
    /// The additional display information.
    pub display: Option<DisplayProperties>,
}

/// A parsed simple expression.
#[derive(Debug)]
pub enum SimpleExpressionKind {
    /// An identifier (variable access, most likely)
    Ident(Ident),
    /// A raw number
    Number(Number),
    /// A function call
    Call(ExprCall),
    /// A unary operator expression
    Unop(ExprUnop),
    /// An expression inside parentheses.
    Parenthised(ExprParenthised),
    /// An explicit iterator.
    ExplicitIterator(ExplicitIterator),
    /// A point collection construction
    PointCollection(PointCollectionConstructor),
    /// Exponentiation.
    Exponentiation(Exponentiation),
}

impl SimpleExpressionKind {
    #[must_use]
    pub fn as_ident(&self) -> Option<&Ident> {
        if let Self::Ident(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A parsed function call
#[derive(Debug)]
pub struct ExprCall {
    /// The ident of the function.
    pub name: NamedIdent,
    /// The `(` token.
    pub lparen: LParen,
    /// The `)` token.
    pub rparen: RParen,
    /// Punctuated params. `None` if no params are given.
    pub params: Option<Punctuated<Expression<false>, Comma>>,
}

/// A parsed parenthesed expression
#[derive(Debug)]
pub struct ExprParenthised {
    /// The `(` token.
    pub lparen: LParen,
    /// The `)` token.
    pub rparen: RParen,
    /// The contained `Expression`.
    pub content: Box<Expression<true>>,
}

/// A parsed unary operator expression.
#[derive(Debug)]
pub struct ExprUnop {
    /// The operator.
    pub operator: UnaryOperator,
    /// The operand (right hand side).
    pub rhs: Box<SimpleExpressionKind>,
}

/// A parsed binary operator expression.
#[derive(Debug)]
pub struct ExprBinop<const ITER: bool> {
    /// The operator
    pub operator: BinaryOperator,
    /// Left hand side
    pub lhs: Box<Expression<ITER>>,
    /// Right hand side.
    pub rhs: Box<Expression<ITER>>,
}

/// Floating point or an integer.
#[derive(Debug, Clone, Copy)]
pub enum FloatOrInteger {
    /// Integer version.
    Integer(i64),
    /// Floating point.
    Float(f64),
}

impl FloatOrInteger {
    /// Returns float if is float, converts if integer.
    #[must_use]
    pub fn to_float(self) -> f64 {
        match self {
            #[allow(clippy::cast_precision_loss)]
            FloatOrInteger::Integer(i) => i as f64,
            FloatOrInteger::Float(f) => f,
        }
    }
}

/// A no-operation statement - a single semicolon.
#[derive(Debug)]
pub struct Noop {
    /// The `;` token.
    pub semi: Semi,
}

/// A `=` rule operator.
#[derive(Debug)]
pub struct EqOp {
    /// The `=` token.
    pub eq: Eq,
}

/// A `<` rule operator.
#[derive(Debug)]
pub struct LtOp {
    /// The `=` token.
    pub lt: Lt,
}

/// A `>` rule operator.
#[derive(Debug)]
pub struct GtOp {
    /// The `>` token.
    pub gt: Gt,
}

/// A `<=` rule operator.
#[derive(Debug)]
pub struct LteqOp {
    /// The `<=` token.
    pub lteq: Lteq,
}

/// A `>=` rule operator.
#[derive(Debug)]
pub struct GteqOp {
    /// The `>=` token.
    pub gteq: Gteq,
}

/// A user-defined rule operator.
#[derive(Debug)]
pub struct DefinedRuleOperator {
    /// The ident.
    pub ident: NamedIdent,
}

/// A builtin rule operator
#[derive(Debug)]
pub enum PredefinedRuleOperator {
    /// Equality
    Eq(EqOp),
    /// Less than
    Lt(LtOp),
    /// Greater than
    Gt(GtOp),
    /// Less than or equal
    Lteq(LteqOp),
    /// Greater than or equal
    Gteq(GteqOp),
}

/// A rule operator.
#[derive(Debug)]
pub enum RuleOperator {
    Predefined(PredefinedRuleOperator),
    Defined(DefinedRuleOperator),
    /// A inverted rule operator (!op)
    Inverted(InvertedRuleOperator),
}

/// An inverted rule operator.
#[derive(Debug)]
pub struct InvertedRuleOperator {
    /// The `!` token
    pub exlamation: Exclamation,
    /// The operator.
    pub operator: Box<RuleOperator>,
}

/// Defines the first half of a flag statement.
#[derive(Debug)]
pub struct FlagName {
    pub at: At,
    pub name: Punctuated<NamedIdent, Dot>,
    pub colon: Colon,
}

impl Parse for FlagName {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            at: At::parse(it)?,
            name: Punctuated::parse(it)?,
            colon: Colon::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.at.span.join(self.colon.span)
    }
}

/// A set of flags.
#[derive(Debug)]
pub struct FlagSet {
    pub lbrace: LBrace,
    pub flags: Vec<FlagStatement>,
    pub rbrace: RBrace,
}

impl Parse for FlagSet {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let mut flags = Vec::new();

        let lbrace = LBrace::parse(it)?;

        while let Some(Token::At(_)) = it.peek().copied() {
            flags.push(FlagStatement::parse(it)?);
        }

        Ok(Self {
            lbrace,
            flags,
            rbrace: RBrace::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.lbrace.span.join(self.rbrace.span)
    }
}

/// Defines the second half of a flag statement.
#[derive(Debug)]
pub enum FlagValue {
    Ident(NamedIdent),
    Set(FlagSet),
    Number(Number),
}

impl Parse for FlagValue {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let peeked = it.peek().copied();

        Ok(match peeked {
            Some(Token::Ident(Ident::Named(_))) => FlagValue::Ident(NamedIdent::parse(it)?),
            Some(Token::LBrace(_)) => FlagValue::Set(FlagSet::parse(it)?),
            Some(Token::Number(_)) => FlagValue::Number(Number::parse(it)?),
            Some(t) => return Err(Error::InvalidToken { token: t.clone() }),
            None => return Err(Error::EndOfInput),
        })
    }

    fn get_span(&self) -> Span {
        match self {
            FlagValue::Ident(v) => v.span,
            FlagValue::Set(v) => v.get_span(),
            FlagValue::Number(v) => v.get_span(),
        }
    }
}

/// Defines a compiler flag or flagset.
#[derive(Debug)]
pub struct FlagStatement {
    pub name: FlagName,
    pub value: FlagValue,
}

impl Parse for FlagStatement {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            name: FlagName::parse(it)?,
            value: FlagValue::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.name.get_span().join(self.value.get_span())
    }
}

/// A single variable definition. Contains its name and optional display properties
#[derive(Debug, Clone)]
pub struct VariableDefinition {
    /// Name of the variable.
    pub name: Ident,
    /// Display properties.
    pub display_properties: Option<DisplayProperties>,
}

/// `let <something> = <something else>`.
/// Defines variables and possibly adds rules to them.
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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

impl Parse for RefStatement {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            display: Option::parse(it)?,
            question: Question::parse(it)?,
            operand: Expression::parse(it)?,
            semi: Semi::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.question.span.join(self.semi.span)
    }
}

/// A general statement.
#[derive(Debug)]
pub enum Statement {
    /// No operation
    Noop(Noop),
    /// let
    Let(LetStatement),
    /// rule
    Rule(RuleStatement),
    /// Flag
    Flag(FlagStatement),
    /// Reference
    Ref(RefStatement),
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
#[derive(Debug, Clone)]
pub struct Punctuated<T, P> {
    /// The first parsed item.
    pub first: Box<T>,
    /// The next items with punctuators.
    pub collection: Vec<(P, T)>,
}

impl<T, P> Punctuated<T, P> {
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

pub trait Parse: Sized {
    /// Tries to parse input tokens into Self.
    ///
    /// # Errors
    /// Errors originate from invalid scripts.
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error>;

    /// Gets the parsed item's span.
    fn get_span(&self) -> Span;
}

impl Parse for ExprCall {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        _it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        unreachable!("ExprCall::parse should never be called.")
    }

    fn get_span(&self) -> Span {
        // From the ident to the ).
        self.name.span.join(self.rparen.span)
    }
}

impl Parse for Statement {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let tok = it.peek().unwrap();
        Ok(match tok {
            Token::Let(_) => Statement::Let(LetStatement::parse(it)?),
            Token::Semi(_) => Statement::Noop(Noop::parse(it)?),
            Token::At(_) => Statement::Flag(FlagStatement::parse(it)?),
            Token::Question(_) => Statement::Ref(RefStatement::parse(it)?),
            _ => Statement::Rule(RuleStatement::parse(it)?),
        })
    }

    fn get_span(&self) -> Span {
        match self {
            Statement::Noop(v) => v.get_span(),
            Statement::Let(v) => v.get_span(),
            Statement::Rule(v) => v.get_span(),
            Statement::Flag(v) => v.get_span(),
            Self::Ref(v) => v.get_span(),
        }
    }
}

impl Parse for Noop {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Noop {
            semi: Semi::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.semi.get_span()
    }
}

impl Parse for RuleStatement {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(RuleStatement {
            display: Option::parse(it)?,
            lhs: Expression::parse(it)?,
            op: RuleOperator::parse(it)?,
            rhs: Expression::parse(it)?,
            semi: Semi::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.lhs.get_span().join(self.semi.span)
    }
}

impl Parse for VariableDefinition {
    fn get_span(&self) -> Span {
        self.display_properties.as_ref().map_or_else(
            || self.name.get_span(),
            |v| self.name.get_span().join(v.get_span()),
        )
    }

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            name: Ident::parse(it)?,
            display_properties: match it.peek().copied() {
                Some(Token::LSquare(_)) => Some(DisplayProperties::parse(it)?),
                _ => None,
            },
        })
    }
}

impl Parse for LetStatement {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let let_token = Let::parse(it)?;
        let ident = Punctuated::parse(it)?;
        let eq = Eq::parse(it)?;
        let expr = Expression::parse(it)?;
        let mut rules = Vec::new();

        // After the defining expression there can be rules.
        loop {
            let next = it.peek().copied();

            match next {
                Some(Token::Semi(_)) => break,
                Some(_) => rules.push((RuleOperator::parse(it)?, Expression::parse(it)?)),
                None => return Err(Error::EndOfInput),
            };
        }

        Ok(LetStatement {
            let_token,
            ident,
            eq,
            expr,
            rules,
            semi: Semi::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.let_token.span.join(self.semi.span)
    }
}

impl<const ITER: bool> Parse for Expression<ITER> {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let mut expr = if ITER {
            let punct = Punctuated::parse(it)?;
            if punct.len() == 1 {
                Expression::Single(punct.first)
            } else {
                // Implicit iterators have id of 0.
                Expression::ImplicitIterator(ImplicitIterator { exprs: punct })
            }
        } else {
            // We can only parse one expression.
            Expression::Single(Box::new(SimpleExpression::parse(it)?))
        };

        loop {
            let next = it.peek().copied();

            let op = match next {
                Some(next) => match next {
                    Token::Asterisk(asterisk) => BinaryOperator::Mul(MulOp {
                        asterisk: *asterisk,
                    }),
                    Token::Plus(plus) => BinaryOperator::Add(AddOp { plus: *plus }),
                    Token::Minus(minus) => BinaryOperator::Sub(SubOp { minus: *minus }),
                    Token::Slash(slash) => BinaryOperator::Div(DivOp { slash: *slash }),
                    _ => break,
                },
                None => break,
            };

            it.next();

            let rhs = {
                let punct = Punctuated::parse(it)?;
                if punct.len() == 1 {
                    Expression::Single(punct.first)
                } else {
                    // Implicit iterators have id of 0.
                    Expression::ImplicitIterator(ImplicitIterator { exprs: punct })
                }
            };

            expr = dispatch_order(expr, op, rhs);
        }

        Ok(expr)
    }

    fn get_span(&self) -> Span {
        match self {
            Expression::ImplicitIterator(it) => it.get_span(),
            Expression::Single(expr) => expr.get_span(),
            Expression::Binop(e) => e.lhs.get_span().join(e.rhs.get_span()),
        }
    }
}

impl BinaryOperator {
    fn index(&self) -> u8 {
        match self {
            BinaryOperator::Add(_) | BinaryOperator::Sub(_) => 1,
            BinaryOperator::Mul(_) | BinaryOperator::Div(_) => 2,
        }
    }
}

/// Inserts an operator with an rhs into a operator series, considering the order of operations.
fn dispatch_order<const ITER: bool>(
    lhs: Expression<ITER>,
    op: BinaryOperator,
    rhs: Expression<ITER>, // We have to trust, that it is a valid expression.
) -> Expression<ITER> {
    assert!(ITER || rhs.is_single());

    match lhs {
        // if lhs is simple, there is no order to consider.
        lhs @ (Expression::ImplicitIterator(_) | Expression::Single(_)) => {
            Expression::Binop(ExprBinop {
                lhs: Box::new(lhs),
                operator: op,
                rhs: Box::new(rhs),
            })
        }
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

impl Parse for SimpleExpression {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            kind: SimpleExpressionKind::parse(it)?,
            display: Option::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        if let Some(display) = self.display.as_ref() {
            self.kind.get_span().join(display.get_span())
        } else {
            self.kind.get_span()
        }
    }
}

impl Parse for SimpleExpressionKind {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let next = it.peek().copied();

        let mut expr = match next {
            Some(next) => match next {
                Token::Number(_) => Self::Number(Number::parse(it)?),
                Token::Minus(m) => {
                    it.next();
                    // negation
                    Self::Unop(ExprUnop {
                        operator: UnaryOperator::Neg(NegOp { minus: *m }),
                        rhs: Box::new(SimpleExpressionKind::parse(it)?),
                    })
                }
                Token::Ident(ident) => {
                    it.next();
                    match ident {
                        Ident::Named(name) => {
                            let next = it.peek().copied();

                            // Names can mean either function calls
                            if let Some(Token::LParen(lparen)) = next {
                                it.next();

                                let params = Option::parse(it)?;

                                Self::Call(ExprCall {
                                    name: name.clone(),
                                    lparen: *lparen,
                                    rparen: RParen::parse(it)?,
                                    params,
                                })
                            } else {
                                // or variable access.
                                Self::Ident(Ident::Named(name.clone()))
                            }
                        }
                        Ident::Collection(c) => Self::Ident(Ident::Collection(c.clone())),
                    }
                }
                Token::LParen(_) => Self::Parenthised(ExprParenthised::parse(it)?),
                Token::Dollar(_) => Self::ExplicitIterator(ExplicitIterator::parse(it)?),
                Token::Ampersant(_) => {
                    Self::PointCollection(PointCollectionConstructor::parse(it)?)
                }
                tok => return Err(Error::InvalidToken { token: tok.clone() }),
            },
            None => return Err(Error::EndOfInput),
        };

        let peeked = it.peek().copied();
        while let Some(Token::Caret(_)) = peeked {
            expr = Self::Exponentiation(Exponentiation {
                base: Box::new(expr),
                caret: Caret::parse(it)?,
                minus: Option::parse(it)?,
                exponent: Exponent::parse(it)?,
            });
        }

        Ok(expr)
    }

    fn get_span(&self) -> Span {
        match self {
            Self::Ident(v) => v.get_span(),
            Self::Number(v) => v.get_span(),
            Self::Call(v) => v.name.span.join(v.rparen.get_span()),
            Self::Unop(v) => v.rhs.get_span().join(match &v.operator {
                UnaryOperator::Neg(v) => v.minus.span,
            }),
            Self::Parenthised(v) => v.get_span(),
            Self::ExplicitIterator(v) => v.get_span(),
            Self::PointCollection(v) => v.get_span(),
            Self::Exponentiation(v) => v.get_span(),
        }
    }
}

impl<T: Parse, U: Parse> Parse for Punctuated<T, U> {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let mut collection = Vec::new();

        let first = Box::parse(it)?;

        while let Some(punct) = Option::<U>::parse(it).unwrap() {
            collection.push((punct, T::parse(it)?));
        }

        Ok(Punctuated { first, collection })
    }

    fn get_span(&self) -> Span {
        match self.collection.last() {
            Some(v) => self.first.get_span().join(v.1.get_span()),
            None => self.first.get_span(),
        }
    }
}

impl<T: Parse> Parse for Option<T> {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let mut it_cloned = it.clone();

        Ok(match T::parse(&mut it_cloned) {
            Ok(res) => {
                *it = it_cloned;
                Some(res)
            }
            Err(_) => None,
        })
    }

    fn get_span(&self) -> Span {
        match self {
            Some(v) => v.get_span(),
            None => span!(0, 0, 0, 0),
        }
    }
}

impl Parse for ExprParenthised {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            lparen: LParen::parse(it)?,
            content: Box::parse(it)?,
            rparen: RParen::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.lparen.span.join(self.rparen.span)
    }
}

impl Parse for RuleOperator {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let next = it.next();
        match next {
            Some(t) => match t {
                Token::Lt(lt) => Ok(RuleOperator::Predefined(PredefinedRuleOperator::Lt(LtOp {
                    lt: *lt,
                }))),
                Token::Gt(gt) => Ok(RuleOperator::Predefined(PredefinedRuleOperator::Gt(GtOp {
                    gt: *gt,
                }))),
                Token::Lteq(lteq) => Ok(RuleOperator::Predefined(PredefinedRuleOperator::Lteq(
                    LteqOp { lteq: *lteq },
                ))),
                Token::Gteq(gteq) => Ok(RuleOperator::Predefined(PredefinedRuleOperator::Gteq(
                    GteqOp { gteq: *gteq },
                ))),
                Token::Eq(eq) => Ok(RuleOperator::Predefined(PredefinedRuleOperator::Eq(EqOp {
                    eq: *eq,
                }))),
                Token::Ident(Ident::Named(name)) => {
                    Ok(RuleOperator::Defined(DefinedRuleOperator {
                        ident: name.clone(),
                    }))
                }
                Token::Exclamation(excl) => Ok(RuleOperator::Inverted(InvertedRuleOperator {
                    exlamation: *excl,
                    operator: Box::new(RuleOperator::parse(it)?),
                })),
                t => Err(Error::InvalidToken { token: t.clone() }),
            },
            None => Err(Error::EndOfInput),
        }
    }

    fn get_span(&self) -> Span {
        match self {
            RuleOperator::Predefined(pre) => match pre {
                PredefinedRuleOperator::Eq(v) => v.eq.span,
                PredefinedRuleOperator::Lt(v) => v.lt.span,
                PredefinedRuleOperator::Gt(v) => v.gt.span,
                PredefinedRuleOperator::Lteq(v) => v.lteq.span,
                PredefinedRuleOperator::Gteq(v) => v.gteq.span,
            },
            RuleOperator::Defined(def) => def.ident.span,
            RuleOperator::Inverted(inv) => inv.exlamation.get_span().join(inv.operator.get_span()),
        }
    }
}

impl_token_parse! {At}
impl_token_parse! {LBrace}
impl_token_parse! {RBrace}
impl_token_parse! {LSquare}
impl_token_parse! {RSquare}
impl_token_parse! {Dollar}
impl_token_parse! {Vertical}
impl_token_parse! {Semi}
impl_token_parse! {Comma}
impl_token_parse! {Ampersant}
impl_token_parse! {Lt}
impl_token_parse! {Gt}
impl_token_parse! {Lteq}
impl_token_parse! {Gteq}
impl_token_parse! {Eq}
impl_token_parse! {LParen}
impl_token_parse! {RParen}
impl_token_parse! {Let}
impl_token_parse! {Colon}
impl_token_parse! {Exclamation}
impl_token_parse! {Dot}
impl_token_parse! {Question}
impl_token_parse! {Slash}
impl_token_parse! {Caret}
impl_token_parse! {Minus}

impl Parse for Number {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Number(tok)) => Ok(tok.clone()),
            Some(t) => Err(Error::InvalidToken { token: t.clone() }),
            None => Err(Error::EndOfInput),
        }
    }

    fn get_span(&self) -> Span {
        match self {
            Self::Float(f) => f.span,
            Self::Integer(i) => i.span,
        }
    }
}

impl Parse for TokInteger {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Number(Number::Integer(tok))) => Ok(tok.clone()),
            Some(t) => Err(Error::InvalidToken { token: t.clone() }),
            None => Err(Error::EndOfInput),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for StrLit {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::String(s)) => Ok(s.clone()),
            Some(t) => Err(Error::InvalidToken { token: t.clone() }),
            None => Err(Error::EndOfInput),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for NamedIdent {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Ident(Ident::Named(named))) => Ok(named.clone()),
            Some(t) => Err(Error::InvalidToken { token: t.clone() }),
            None => Err(Error::EndOfInput),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Ident {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Ident(ident)) => Ok(ident.clone()),
            Some(t) => Err(Error::InvalidToken { token: t.clone() }),
            None => Err(Error::EndOfInput),
        }
    }

    fn get_span(&self) -> Span {
        match self {
            Ident::Named(n) => n.span,
            Ident::Collection(c) => c.span,
        }
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Box::new(T::parse(it)?))
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
#[derive(Debug, Clone)]
pub struct Property {
    /// Property name.
    pub name: NamedIdent,
    /// '='
    pub eq: Eq,
    /// Property value.
    pub value: PropertyValue,
}

impl Parse for Property {
    fn get_span(&self) -> Span {
        self.name.span.join(self.value.get_span())
    }

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            name: NamedIdent::parse(it)?,
            eq: Eq::parse(it)?,
            value: PropertyValue::parse(it)?,
        })
    }
}

/// A property's value
#[derive(Debug, Clone)]
pub enum PropertyValue {
    Number(Number),
    Ident(Ident),
    RawString(RawString),
    String(StrLit),
}

#[derive(Debug, Clone)]
pub struct RawString {
    pub excl: Exclamation,
    pub lit: StrLit,
}

impl Parse for RawString {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            excl: Exclamation::parse(it)?,
            lit: StrLit::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.excl.span.join(self.lit.span)
    }
}

pub trait FromProperty: Sized {
    /// # Errors
    /// Causes an error if the value is not properly convertible.
    fn from_property(property: PropertyValue) -> Result<Self, Error>;
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

impl Parse for PropertyValue {
    fn get_span(&self) -> Span {
        match self {
            Self::Number(n) => n.get_span(),
            Self::Ident(i) => i.get_span(),
            Self::String(s) => s.get_span(),
            Self::RawString(s) => s.get_span(),
        }
    }

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        let peeked = it.peek().copied();

        match peeked {
            Some(Token::Ident(_)) => Ok(Self::Ident(Ident::parse(it)?)),
            Some(Token::Number(_)) => Ok(Self::Number(Number::parse(it)?)),
            Some(Token::Exclamation(_)) => Ok(Self::RawString(RawString::parse(it)?)),
            Some(Token::String(_)) => Ok(Self::String(StrLit::parse(it)?)),
            Some(t) => Err(Error::InvalidToken { token: t.clone() }),
            None => Err(Error::EndOfInput),
        }
    }
}

/// Properties related to displaying things.
#[derive(Debug, Clone)]
pub struct DisplayProperties {
    /// '['
    pub lsquare: LSquare,
    /// Properties
    pub properties: Punctuated<Property, Semi>,
    /// ']'
    pub rsquare: RSquare,
}

impl Parse for DisplayProperties {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
    ) -> Result<Self, Error> {
        Ok(Self {
            lsquare: LSquare::parse(it)?,
            properties: Punctuated::parse(it)?,
            rsquare: RSquare::parse(it)?,
        })
    }

    fn get_span(&self) -> Span {
        self.lsquare.span.join(self.rsquare.span)
    }
}
