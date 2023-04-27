use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    rc::Rc,
};

use crate::span;

use super::{
    token::{
        Ampersant, Asterisk, At, Colon, Comma, Dollar, Eq, Exclamation, Gt, Gteq, Ident, LBrace,
        LParen, Let, Lt, Lteq, Minus, NamedIdent, Number, Plus, Position, RBrace, RParen, Semi,
        Slash, Span, Token, Vertical, Dot,
    },
    unroll::{CompileContext, RuleOperatorDefinition},
    ComplexUnit, Error, SimpleUnit,
};

macro_rules! impl_token_parse {
    ($token:ident) => {
        impl Parse for $token {
            fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
                it: &mut Peekable<I>,
                _context: &CompileContext,
            ) -> Result<Self, Error> {
                match it.next() {
                    Some(Token::$token(tok)) => Ok(*tok),
                    Some(t) => Err(Error::invalid_token(t.clone())),
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
                Type::Predefined(pre) => match pre {
                    PredefinedType::Point
                    | PredefinedType::Line
                    | PredefinedType::PointCollection(_) => Type::Undefined,
                    t @ PredefinedType::Scalar(_) => Type::Predefined(t.clone()),
                },
                _ => Type::Undefined,
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(Self {
            ampersant: Ampersant::parse(it, context)?,
            left_paren: LParen::parse(it, context)?,
            points: Punctuated::parse(it, context)?,
            right_paren: RParen::parse(it, context)?,
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
    pub id_token: Number,
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(ImplicitIterator {
            exprs: Punctuated::parse(it, context)?,
        })
    }

    fn get_span(&self) -> Span {
        self.exprs.get_span()
    }
}

impl Parse for ExplicitIterator {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let dollar = Dollar::parse(it, context)?;
        let id_token = ExprNumber::parse(it, context)?;
        let left_paren = LParen::parse(it, context)?;
        let exprs = Punctuated::parse(it, context)?;
        let right_paren = RParen::parse(it, context)?;

        if exprs.len() == 1 {
            return Err(Error::SingleVariantExplicitIterator {
                error_span: dollar.span.join(right_paren.span),
            });
        }

        Ok(ExplicitIterator {
            dollar,
            id_token: id_token.token,
            left_paren,
            exprs,
            right_paren,
            id: if id_token.token.dot.is_none() {
                if id_token.token.integral < 256 {
                    id_token.token.integral.try_into().unwrap()
                } else {
                    return Err(Error::IteratorIdExceeds255 {
                        error_span: id_token.get_span(),
                    });
                }
            } else {
                return Err(Error::IteratorIdMustBeAnInteger {
                    error_span: id_token.get_span(),
                });
            },
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

/// A parsed simple expression.
#[derive(Debug)]
pub enum SimpleExpression {
    /// An identifier (variable access, most likely)
    Ident(Ident),
    /// A raw number
    Number(ExprNumber),
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
}

impl SimpleExpression {
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
    pub rhs: Box<SimpleExpression>,
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
#[derive(Debug, Clone)]
pub enum FloatOrInteger {
    /// Integer version.
    Integer(i64),
    /// Floating point.
    Float(f64)
}

/// A parsed raw number.
#[derive(Debug, Clone)]
pub struct ExprNumber {
    /// Its value.
    pub value: FloatOrInteger,
    /// Its token.
    pub token: Number,
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
    /// Pointer to the definition.
    pub definition: Rc<RuleOperatorDefinition>,
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(Self {
            at: At::parse(it, context)?,
            name: Punctuated::parse(it, context)?,
            colon: Colon::parse(it, context)?,
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let mut flags = Vec::new();

        let lbrace = LBrace::parse(it, context)?;

        while let Some(Token::At(_)) = it.peek().copied() {
            flags.push(FlagStatement::parse(it, context)?);
        }

        Ok(Self {
            lbrace,
            flags,
            rbrace: RBrace::parse(it, context)?,
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let peeked = it.peek().copied();

        Ok(match peeked {
            Some(Token::Ident(Ident::Named(_))) => {
                FlagValue::Ident(NamedIdent::parse(it, context)?)
            }
            Some(Token::LBrace(_)) => FlagValue::Set(FlagSet::parse(it, context)?),
            Some(Token::Number(_)) => FlagValue::Number(Number::parse(it, context)?),
            Some(t) => return Err(Error::InvalidToken { token: t.clone() }),
            None => return Err(Error::EndOfInput),
        })
    }

    fn get_span(&self) -> Span {
        match self {
            FlagValue::Ident(v) => v.span,
            FlagValue::Set(v) => v.get_span(),
            FlagValue::Number(v) => v.span,
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(Self {
            name: FlagName::parse(it, context)?,
            value: FlagValue::parse(it, context)?,
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
    pub display_properties: Option<DisplayProperties>
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
    /// Left hand side
    pub lhs: Expression<true>,
    /// Rule operator
    pub op: RuleOperator,
    /// Right hand side
    pub rhs: Expression<true>,
    /// The ending semicolon.
    pub semi: Semi,
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
        context: &CompileContext,
    ) -> Result<Self, Error>;

    /// Gets the parsed item's span.
    fn get_span(&self) -> Span;
}

impl Parse for ExprCall {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        _it: &mut Peekable<I>,
        _context: &CompileContext,
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let tok = it.peek().unwrap();
        Ok(match tok {
            Token::Let(_) => Statement::Let(LetStatement::parse(it, context)?),
            Token::Semi(_) => Statement::Noop(Noop::parse(it, context)?),
            Token::At(_) => Statement::Flag(FlagStatement::parse(it, context)?),
            _ => Statement::Rule(RuleStatement::parse(it, context)?),
        })
    }

    fn get_span(&self) -> Span {
        match self {
            Statement::Noop(v) => v.get_span(),
            Statement::Let(v) => v.get_span(),
            Statement::Rule(v) => v.get_span(),
            Statement::Flag(v) => v.get_span(),
        }
    }
}

impl Parse for Noop {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(Noop {
            semi: Semi::parse(it, context)?,
        })
    }

    fn get_span(&self) -> Span {
        self.semi.get_span()
    }
}

impl Parse for RuleStatement {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(RuleStatement {
            lhs: Expression::parse(it, context)?,
            op: RuleOperator::parse(it, context)?,
            rhs: Expression::parse(it, context)?,
            semi: Semi::parse(it, context)?,
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
            |v| self.name.get_span().join(v.get_span())
        )
    }
    
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
            it: &mut Peekable<I>,
            context: &CompileContext,
        ) -> Result<Self, Error> {
        Ok(Self {
            name: Ident::parse(it, context)?,
            display_properties: Option::parse(it, context)?
        })
    }
}

impl Parse for LetStatement {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let let_token = Let::parse(it, context)?;
        let ident = Punctuated::parse(it, context)?;
        let eq = Eq::parse(it, context)?;
        let expr = Expression::parse(it, context)?;
        let mut rules = Vec::new();

        // After the defining expression there can be rules.
        loop {
            let next = it.peek().copied();

            match next {
                Some(Token::Semi(_)) => break,
                Some(_) => rules.push((
                    RuleOperator::parse(it, context)?,
                    Expression::parse(it, context)?,
                )),
                None => return Err(Error::EndOfInput),
            };
        }

        Ok(LetStatement {
            let_token,
            ident,
            eq,
            expr,
            rules,
            semi: Semi::parse(it, context)?,
        })
    }

    fn get_span(&self) -> Span {
        self.let_token.span.join(self.semi.span)
    }
}

impl Parse for ExprNumber {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            // The integral and decimal parts have to be merged into one floating point number.
            #[allow(clippy::cast_precision_loss)]
            Some(Token::Number(num)) => Ok(ExprNumber {
                value: if num.dot.is_some() {
                    FloatOrInteger::Float(num.integral as f64
                        + num.decimal as f64 * f64::powi(10.0, -i32::from(num.decimal_places)))
                } else {
                    FloatOrInteger::Integer(num.integral as i64)
                },
                token: *num,
            }),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::EndOfInput),
        }
    }

    fn get_span(&self) -> Span {
        self.token.span
    }
}

impl<const ITER: bool> Parse for Expression<ITER> {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let mut expr = if ITER {
            let punct = Punctuated::parse(it, context)?;
            if punct.len() == 1 {
                Expression::Single(punct.first)
            } else {
                // Implicit iterators have id of 0.
                Expression::ImplicitIterator(ImplicitIterator { exprs: punct })
            }
        } else {
            // We can only parse one expression.
            Expression::Single(Box::new(SimpleExpression::parse(it, context)?))
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
                let punct = Punctuated::parse(it, context)?;
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let next = it.peek().copied();

        let expr = match next {
            Some(next) => match next {
                Token::Number(_) => SimpleExpression::Number(ExprNumber::parse(it, context)?),
                Token::Minus(m) => {
                    it.next();
                    // negation
                    SimpleExpression::Unop(ExprUnop {
                        operator: UnaryOperator::Neg(NegOp { minus: *m }),
                        rhs: Box::new(SimpleExpression::parse(it, context)?),
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

                                let params = Option::parse(it, context)?;

                                SimpleExpression::Call(ExprCall {
                                    name: name.clone(),
                                    lparen: *lparen,
                                    rparen: RParen::parse(it, context)?,
                                    params,
                                })
                            } else {
                                // or variable access.
                                SimpleExpression::Ident(Ident::Named(name.clone()))
                            }
                        }
                        Ident::Collection(c) => {
                            SimpleExpression::Ident(Ident::Collection(c.clone()))
                        }
                    }
                }
                Token::LParen(_) => {
                    SimpleExpression::Parenthised(ExprParenthised::parse(it, context)?)
                }
                Token::Dollar(_) => {
                    SimpleExpression::ExplicitIterator(ExplicitIterator::parse(it, context)?)
                }
                Token::Ampersant(_) => SimpleExpression::PointCollection(
                    PointCollectionConstructor::parse(it, context)?,
                ),
                tok => return Err(Error::invalid_token(tok.clone())),
            },
            None => return Err(Error::EndOfInput),
        };

        Ok(expr)
    }

    fn get_span(&self) -> Span {
        match self {
            SimpleExpression::Ident(v) => v.get_span(),
            SimpleExpression::Number(v) => v.get_span(),
            SimpleExpression::Call(v) => v.name.span.join(v.rparen.get_span()),
            SimpleExpression::Unop(v) => v.rhs.get_span().join(match &v.operator {
                UnaryOperator::Neg(v) => v.minus.span,
            }),
            SimpleExpression::Parenthised(v) => v.get_span(),
            Self::ExplicitIterator(v) => v.get_span(),
            Self::PointCollection(v) => v.get_span(),
        }
    }
}

impl<T: Parse, U: Parse> Parse for Punctuated<T, U> {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let mut collection = Vec::new();

        let first = Box::parse(it, context)?;

        while let Some(punct) = Option::<U>::parse(it, context).unwrap() {
            collection.push((punct, T::parse(it, context)?));
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let mut it_cloned = it.clone();

        Ok(match T::parse(&mut it_cloned, context) {
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(Self {
            lparen: LParen::parse(it, context)?,
            content: Box::parse(it, context)?,
            rparen: RParen::parse(it, context)?,
        })
    }

    fn get_span(&self) -> Span {
        self.lparen.span.join(self.rparen.span)
    }
}

impl Parse for RuleOperator {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
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
                    if let Some(definition) = context.rule_ops.get(&name.ident) {
                        Ok(RuleOperator::Defined(DefinedRuleOperator {
                            ident: name.clone(),
                            definition: Rc::clone(definition),
                        }))
                    } else {
                        Err(Error::undefined_rule_operator(name.clone()))
                    }
                }
                Token::Exclamation(excl) => Ok(RuleOperator::Inverted(InvertedRuleOperator {
                    exlamation: *excl,
                    operator: Box::new(RuleOperator::parse(it, context)?),
                })),
                t => Err(Error::invalid_token(t.clone())),
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
impl_token_parse! {Number}
impl_token_parse! {Dot}

impl Parse for NamedIdent {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Ident(Ident::Named(named))) => Ok(named.clone()),
            Some(t) => Err(Error::invalid_token(t.clone())),
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
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Ident(ident)) => Ok(ident.clone()),
            Some(t) => Err(Error::invalid_token(t.clone())),
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
        context: &CompileContext,
    ) -> Result<Self, Error> {
        Ok(Box::new(T::parse(it, context)?))
    }

    fn get_span(&self) -> Span {
        (**self).get_span()
    }
}

/// A builtin type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PredefinedType {
    /// A point
    Point,
    /// A line
    Line,
    /// A scalar of a certain unit.
    Scalar(Option<ComplexUnit>),
    /// A point collection.
    PointCollection(usize),
}

impl PredefinedType {
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

/// A type of an expression or a variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Builtin
    Predefined(PredefinedType),
    /// User-defined
    Defined,
    /// undefined, unknown
    Undefined,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Predefined(pre) => match pre {
                PredefinedType::Point => write!(f, "Point"),
                PredefinedType::Line => write!(f, "Line"),
                PredefinedType::Scalar(unit) => match unit {
                    Some(unit) => write!(f, "Scalar ({unit})"),
                    None => write!(f, "Scalar (no unit)"),
                },
                PredefinedType::PointCollection(l) => write!(f, "Point collection ({l})"),
            },
            Type::Defined => write!(f, "Defined"),
            Type::Undefined => write!(f, "Undefined"),
        }
    }
}

impl Type {
    /// Whether `self` can be cast to `into`.
    #[must_use]
    pub fn can_cast(&self, into: &Type) -> bool {
        match self {
            Type::Predefined(pre) => match pre {
                // A point can only be cast into another point or a point collection with length one.
                PredefinedType::Point => match into {
                    Type::Predefined(pre) => matches!(
                        pre,
                        PredefinedType::Point | PredefinedType::PointCollection(1)
                    ),
                    _ => false,
                },
                // A line can only be cast into another line.
                PredefinedType::Line => matches!(into, Type::Predefined(PredefinedType::Line)),
                // A scalar with a defined unit can only be cast into another scalar with the same unit.
                PredefinedType::Scalar(Some(unit1)) => {
                    if let Type::Predefined(PredefinedType::Scalar(Some(unit2))) = into {
                        unit1 == unit2
                    } else {
                        false
                    }
                }
                // A scalar with no defined unit can be cast into any other scalar, except angle.
                PredefinedType::Scalar(None) => match into {
                    Type::Predefined(PredefinedType::Scalar(unit)) => match unit {
                        Some(unit) => unit.0[3] == 0, // no angle
                        None => true,
                    },
                    _ => false,
                },
                PredefinedType::PointCollection(l) => match into {
                    Type::Predefined(pre) => match pre {
                        PredefinedType::Point => *l == 1,
                        PredefinedType::Line => *l == 2,
                        PredefinedType::Scalar(Some(unit)) => {
                            unit == &ComplexUnit::new(SimpleUnit::Distance) && *l == 2
                        }
                        PredefinedType::Scalar(None) => false,
                        PredefinedType::PointCollection(v) => v == l,
                    },
                    _ => false,
                },
            },
            Type::Defined | Type::Undefined => false,
        }
    }

    #[must_use]
    pub fn as_predefined(&self) -> Option<&PredefinedType> {
        if let Self::Predefined(v) = self {
            Some(v)
        } else {
            None
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
    pub value: PropertyValue
}

impl Parse for Property {
    fn get_span(&self) -> Span {
        self.name.span.join(self.value.get_span())
    }

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
            it: &mut Peekable<I>,
            context: &CompileContext,
        ) -> Result<Self, Error> {
        Ok(Self {
            name: NamedIdent::parse(it, context)?,
            eq: Eq::parse(it, context)?,
            value: PropertyValue::parse(it, context)?
        })
    }
}

/// A property's value
#[derive(Debug, Clone)]
pub enum PropertyValue {
    Number(ExprNumber),
    Ident(NamedIdent)
}

impl PropertyValue {
    pub fn as_bool(&self) -> Result<bool, Error> {
        Ok(match self {
            PropertyValue::Ident(ident) => {
                match ident.ident.as_str() {
                    "enabled" | "on" | "true" => true,
                    "disabled" | "off" | "false" => false,
                    _ => {
                        return Err(Error::BooleanExpected {
                            error_span: ident.get_span(),
                        })
                    }
                }
            },
            PropertyValue::Number(num) => {
                match num.value {
                    FloatOrInteger::Integer(1) => true,
                    FloatOrInteger::Integer(0) => false,
                    _ => return Err(Error::BooleanExpected {
                        error_span: num.get_span(),
                    })
                }
            },
        })
    }
}

impl Parse for PropertyValue {
    fn get_span(&self) -> Span {
        match self {
            Self::Number(n) => n.get_span(),
            Self::Ident(i) => i.span
        }
    }

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
            it: &mut Peekable<I>,
            _context: &CompileContext,
        ) -> Result<Self, Error> {
            let peeked = it.peek().cloned();

            match peeked {
                Some(Token::Ident(Ident::Named(ident))) => Ok(Self::Ident(ident.clone())),
                Some(Token::Number(number)) => Ok(Self::Number(ExprNumber::parse(it, _context)?)),
                Some(t) => Err(Error::invalid_token(t.clone())),
                None => Err(Error::EndOfInput),
            }
    }
}

/// Properties related to displaying things.
#[derive(Debug, Clone)]
pub struct DisplayProperties {
    /// '['
    pub lbrace: LBrace,
    /// Properties
    pub properties: Punctuated<Property, Comma>,
    /// ']'
    pub rbrace: RBrace
}

impl Parse for DisplayProperties {
    fn get_span(&self) -> Span {
        self.lbrace.span.join(self.rbrace.span)
    }

    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
            it: &mut Peekable<I>,
            context: &CompileContext,
        ) -> Result<Self, Error> {
        Ok(Self {
            lbrace: LBrace::parse(it, context)?,
            properties: Punctuated::parse(it, context)?,
            rbrace: RBrace::parse(it, context)?
        })
    }
}
