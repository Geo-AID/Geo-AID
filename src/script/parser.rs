use std::{fmt::Debug, iter::Peekable, rc::Rc};

use crate::span;

use super::{
    token::{
        Asterisk, Comma, Eq, Exclamation, Gt, Gteq, Ident, LParen, Let, Lt, Lteq, Minus,
        NamedIdent, Number, Plus, PointCollection, Position, RParen, Semi, Slash, Span, Token,
        Vertical,
    },
    unroll::{CompileContext, RuleOperatorDefinition},
    ComplexUnit, Error, SimpleUnit,
};

#[derive(Debug)]
pub enum UnaryOperator {
    Neg(NegOp),
}

impl UnaryOperator {
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

#[derive(Debug)]
pub struct NegOp {
    pub minus: Minus,
}

#[derive(Debug)]
pub struct AddOp {
    pub plus: Plus,
}

#[derive(Debug)]
pub struct SubOp {
    pub minus: Minus,
}

#[derive(Debug)]
pub struct MulOp {
    pub asterisk: Asterisk,
}

#[derive(Debug)]
pub struct DivOp {
    pub slash: Slash,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add(AddOp),
    Sub(SubOp),
    Mul(MulOp),
    Div(DivOp),
}

impl BinaryOperator {
    #[must_use]
    pub fn get_returned(&self, lhs: &Type, rhs: &Type) -> Type {
        match self {
            BinaryOperator::Add(_) | BinaryOperator::Sub(_) => {
                let left_unit = match lhs {
                    Type::Predefined(PredefinedType::Scalar(Some(unit))) => {
                        Some(Some(unit.clone()))
                    }
                    Type::Predefined(PredefinedType::Scalar(None)) => Some(None),
                    Type::Predefined(PredefinedType::PointCollection(2)) => {
                        Some(Some(ComplexUnit::new(SimpleUnit::Distance)))
                    }
                    _ => None,
                };

                if let Some(ltype) = left_unit {
                    if let Some(ltype) = ltype {
                        if rhs.can_cast(&Type::Predefined(PredefinedType::Scalar(Some(
                            ltype.clone(),
                        )))) {
                            Type::Predefined(PredefinedType::Scalar(Some(ltype)))
                        } else {
                            Type::Undefined
                        }
                    } else {
                        let right_unit = match rhs {
                            Type::Predefined(PredefinedType::Scalar(Some(unit))) => {
                                Some(Some(unit.clone()))
                            }
                            Type::Predefined(PredefinedType::Scalar(None)) => Some(None),
                            Type::Predefined(PredefinedType::PointCollection(2)) => {
                                Some(Some(ComplexUnit::new(SimpleUnit::Distance)))
                            }
                            _ => None,
                        };

                        if let Some(rtype) = right_unit {
                            if let Some(rtype) = rtype {
                                Type::Predefined(PredefinedType::Scalar(Some(rtype)))
                            } else {
                                Type::Predefined(PredefinedType::Scalar(None))
                            }
                        } else {
                            Type::Undefined
                        }
                    }
                } else {
                    Type::Undefined
                }
            }
            BinaryOperator::Mul(_) | BinaryOperator::Div(_) => {
                let left_unit = match lhs {
                    Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
                    Type::Predefined(PredefinedType::Scalar(None)) => {
                        Some(ComplexUnit::new(SimpleUnit::Scalar))
                    }
                    Type::Predefined(PredefinedType::PointCollection(2)) => {
                        Some(ComplexUnit::new(SimpleUnit::Distance))
                    }
                    _ => None,
                };

                let right_unit = match rhs {
                    Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
                    Type::Predefined(PredefinedType::Scalar(None)) => {
                        Some(ComplexUnit::new(SimpleUnit::Scalar))
                    }
                    Type::Predefined(PredefinedType::PointCollection(2)) => {
                        Some(ComplexUnit::new(SimpleUnit::Distance))
                    }
                    _ => None,
                };

                if let Some(ltype) = left_unit {
                    if let Some(rtype) = right_unit {
                        Type::Predefined(PredefinedType::Scalar(Some(ltype * rtype)))
                    } else {
                        Type::Undefined
                    }
                } else {
                    Type::Undefined
                }
            }
        }
    }
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
pub enum Expression {
    Simple(Punctuated<SimpleExpression, Vertical>),
    Binop(ExprBinop),
}

impl Expression {
    #[must_use]
    pub fn as_simple(&self) -> Option<&Punctuated<SimpleExpression, Vertical>> {
        if let Self::Simple(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum SimpleExpression {
    Ident(Ident),
    Number(ExprNumber),
    Call(ExprCall),
    Unop(ExprUnop),
    Parenthised(ExprParenthised),
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

#[derive(Debug)]
pub struct ExprCall {
    pub name: NamedIdent,
    pub lparen: LParen,
    pub rparen: RParen,
    pub params: Option<Punctuated<Box<Expression>, Comma>>,
}

#[derive(Debug)]
pub struct ExprParenthised {
    pub lparen: LParen,
    pub rparen: RParen,
    pub content: Box<Expression>,
}

#[derive(Debug)]
pub struct ExprUnop {
    pub operator: UnaryOperator,
    pub rhs: Box<SimpleExpression>,
}

#[derive(Debug)]
pub struct ExprBinop {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct ExprNumber {
    pub value: f64,
    pub token: Number,
}

#[derive(Debug)]
pub struct Noop {
    pub semi: Semi,
}

#[derive(Debug)]
pub struct EqOp {
    pub eq: Eq,
}

#[derive(Debug)]
pub struct LtOp {
    pub lt: Lt,
}

#[derive(Debug)]
pub struct GtOp {
    pub gt: Gt,
}

#[derive(Debug)]
pub struct LteqOp {
    pub lteq: Lteq,
}

#[derive(Debug)]
pub struct GteqOp {
    pub gteq: Gteq,
}

#[derive(Debug)]
pub struct DefinedRuleOperator {
    pub ident: NamedIdent,
    pub definition: Rc<RuleOperatorDefinition>,
}

#[derive(Debug)]
pub enum PredefinedRuleOperator {
    Eq(EqOp),
    Lt(LtOp),
    Gt(GtOp),
    Lteq(LteqOp),
    Gteq(GteqOp),
}

#[derive(Debug)]
pub enum RuleOperator {
    Predefined(PredefinedRuleOperator),
    Defined(DefinedRuleOperator),
    Inverted(InvertedRuleOperator),
}

#[derive(Debug)]
pub struct InvertedRuleOperator {
    pub exlamation: Exclamation,
    pub operator: Box<RuleOperator>,
}

#[derive(Debug)]
pub struct LetStatement {
    pub let_token: Let,
    pub ident: Punctuated<Ident, Vertical>,
    pub eq: Eq,
    pub expr: Expression,
    pub rules: Vec<(RuleOperator, Expression)>,
    pub semi: Semi,
}

#[derive(Debug)]
pub struct RuleStatement {
    pub lhs: Expression,
    pub op: RuleOperator,
    pub rhs: Expression,
    pub semi: Semi,
}

#[derive(Debug)]
pub enum Statement {
    Noop(Noop),
    Let(LetStatement),
    Rule(RuleStatement),
}

#[derive(Debug)]
pub struct Punctuated<T, P> {
    pub first: T,
    pub collection: Vec<(P, T)>,
}

impl<T, P> Punctuated<T, P> {
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        vec![&self.first]
            .into_iter()
            .chain(self.collection.iter().map(|x| &x.1))
    }

    pub fn len(&self) -> usize {
        self.collection.len() + 1
    }

    pub fn is_empty(&self) -> bool {
        false
    }

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
    /// Errors originate from ivalid scripts.
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error>;

    fn get_span(&self) -> Span;
}

pub trait GetType {
    /// Gets the type of self.
    ///
    /// # Errors
    /// Some constructs can fail to determine type (inconsistent iterators).
    fn get_type(&self, context: &CompileContext) -> Result<Type, Error>;

    /// Tries to see if self's type matches t.
    ///
    /// # Errors
    /// Raises an error if it doesn't.
    fn match_type(&self, context: &CompileContext, t: &Type) -> Result<(), Error>;
}

impl Parse for ExprCall {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        _it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        unreachable!("ExprCall::parse should never be called.")
    }

    fn get_span(&self) -> Span {
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
            _ => Statement::Rule(RuleStatement::parse(it, context)?),
        })
    }

    fn get_span(&self) -> Span {
        match self {
            Statement::Noop(v) => v.get_span(),
            Statement::Let(v) => v.get_span(),
            Statement::Rule(v) => v.get_span(),
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

impl Parse for Let {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Let(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Semi {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Semi(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
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

        loop {
            let next = it.peek().copied();

            match next {
                Some(Token::Semi(_)) => break,
                Some(_) => rules.push((
                    RuleOperator::parse(it, context)?,
                    Expression::parse(it, context)?,
                )),
                None => return Err(Error::end_of_input()),
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
            #[allow(clippy::cast_precision_loss)]
            Some(Token::Number(num)) => Ok(ExprNumber {
                value: {
                    num.integral as f64
                        + num.decimal as f64 * f64::powi(10.0, -i32::from(num.decimal_places))
                },
                token: *num,
            }),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.token.span
    }
}

impl Parse for Expression {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let mut expr = Expression::Simple(Punctuated::parse(it, context)?);

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

            expr = dispatch_order(expr, op, Punctuated::parse(it, context)?);
        }

        Ok(expr)
    }

    fn get_span(&self) -> Span {
        match self {
            Expression::Simple(s) => s.get_span(),
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

fn dispatch_order(
    lhs: Expression,
    op: BinaryOperator,
    rhs: Punctuated<SimpleExpression, Vertical>,
) -> Expression {
    match lhs {
        Expression::Simple(s) => Expression::Binop(ExprBinop {
            lhs: Box::new(Expression::Simple(s)),
            operator: op,
            rhs: Box::new(Expression::Simple(rhs)),
        }),
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
                    rhs: Box::new(Expression::Simple(rhs)),
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
                tok => return Err(Error::invalid_token(tok.clone())),
            },
            None => return Err(Error::end_of_input()),
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
        }
    }
}

impl<T: Parse + Debug, U: Parse> Parse for Punctuated<T, U> {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        context: &CompileContext,
    ) -> Result<Self, Error> {
        let mut collection = Vec::new();

        let first = T::parse(it, context)?;

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
                        Err(Error::undefined_operator(name.clone()))
                    }
                }
                Token::Exclamation(excl) => Ok(RuleOperator::Inverted(InvertedRuleOperator {
                    exlamation: *excl,
                    operator: Box::new(RuleOperator::parse(it, context)?),
                })),
                t => Err(Error::invalid_token(t.clone())),
            },
            None => Err(Error::end_of_input()),
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

impl Parse for Vertical {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Vertical(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Comma {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Comma(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Lt {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Lt(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Gt {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Gt(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Lteq {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Lteq(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Gteq {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Gteq(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for Exclamation {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Exclamation(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for LParen {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::LParen(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
    }
}

impl Parse for RParen {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::RParen(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
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
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        match self {
            Ident::Named(n) => n.span,
            Ident::Collection(c) => c.span,
        }
    }
}

impl Parse for Eq {
    fn parse<'r, I: Iterator<Item = &'r Token> + Clone>(
        it: &mut Peekable<I>,
        _context: &CompileContext,
    ) -> Result<Self, Error> {
        match it.next() {
            Some(Token::Eq(tok)) => Ok(*tok),
            Some(t) => Err(Error::invalid_token(t.clone())),
            None => Err(Error::end_of_input()),
        }
    }

    fn get_span(&self) -> Span {
        self.span
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

impl<T: GetType + Parse, U> GetType for Punctuated<T, U> {
    fn get_type(&self, context: &CompileContext) -> Result<Type, Error> {
        let t = self.first.get_type(context)?;

        for (_, v) in &self.collection {
            v.match_type(context, &t).map_err(|err| match err {
                Error::InvalidType { expected, got } => {
                    Error::inconsistent_types(expected, self.first.get_span(), got.0, got.1)
                }
                err => err,
            })?;
        }

        Ok(t)
    }

    fn match_type(&self, context: &CompileContext, t: &Type) -> Result<(), Error> {
        for v in self.iter() {
            v.match_type(context, t)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PredefinedType {
    Point,
    Line,
    Scalar(Option<ComplexUnit>),
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

pub struct DefinedType {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Predefined(PredefinedType),
    Defined,
    Undefined,
}

impl Type {
    #[must_use]
    pub fn can_infer_pc(&self, collection: &PointCollection) -> bool {
        match self {
            Self::Predefined(pre) => match pre {
                PredefinedType::Point => collection.len() == 1,
                PredefinedType::Line => collection.len() == 2,
                PredefinedType::PointCollection(l) => collection.len() == *l,
                PredefinedType::Scalar(None) => false,
                PredefinedType::Scalar(Some(unit)) => {
                    unit == &ComplexUnit::new(SimpleUnit::Distance) && collection.len() == 2
                }
            },
            Self::Defined | Self::Undefined => false,
        }
    }

    #[must_use]
    pub fn can_cast(&self, into: &Type) -> bool {
        match self {
            Type::Predefined(pre) => match pre {
                PredefinedType::Point => match into {
                    Type::Predefined(pre) => matches!(
                        pre,
                        PredefinedType::Point | PredefinedType::PointCollection(1)
                    ),
                    _ => false,
                },
                PredefinedType::Line => match into {
                    Type::Predefined(pre) => matches!(
                        pre,
                        PredefinedType::Line | PredefinedType::PointCollection(2)
                    ),
                    _ => false,
                },
                PredefinedType::Scalar(Some(unit1)) => {
                    if let Type::Predefined(PredefinedType::Scalar(Some(unit2))) = into {
                        unit1 == unit2
                    } else {
                        false
                    }
                }
                PredefinedType::Scalar(None) => {
                    matches!(into, Type::Predefined(PredefinedType::Scalar(_)))
                }
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

impl GetType for Expression {
    fn get_type(&self, context: &CompileContext) -> Result<Type, Error> {
        match self {
            Expression::Simple(s) => s.get_type(context),
            Expression::Binop(b) => Ok(b
                .operator
                .get_returned(&b.lhs.get_type(context)?, &b.rhs.get_type(context)?)),
        }
    }

    /// Checks if the type of the expression matches type `t`.
    fn match_type(&self, context: &CompileContext, t: &Type) -> Result<(), Error> {
        let vt = self.get_type(context)?;

        if vt.can_cast(t) {
            Ok(())
        } else {
            Err(Error::invalid_type(t.clone(), vt, self.get_span()))
        }
    }
}

impl GetType for SimpleExpression {
    fn get_type(&self, context: &CompileContext) -> Result<Type, Error> {
        Ok(match self {
            SimpleExpression::Ident(ident) => match ident {
                Ident::Named(named) => {
                    if let Some(var) = context.variables.get(&named.ident) {
                        var.get_type(context)?
                    } else {
                        Type::Undefined
                    }
                }
                Ident::Collection(c) => Type::Predefined(PredefinedType::PointCollection(c.len())),
            },
            SimpleExpression::Number(_) => Type::Predefined(PredefinedType::Scalar(None)),
            SimpleExpression::Call(c) => {
                if let Some(f) = context.functions.get(&c.name.ident) {
                    let params = c.params.as_ref().map_or_else(
                        || Ok(Vec::new()),
                        |x| {
                            x.iter()
                                .map(|v| v.get_type(context))
                                .collect::<Result<Vec<Type>, Error>>()
                        },
                    )?;

                    f.get_returned(&params)
                } else {
                    Type::Undefined
                }
            }
            SimpleExpression::Unop(u) => u.operator.get_returned(&u.rhs.get_type(context)?),
            SimpleExpression::Parenthised(p) => p.content.get_type(context)?,
        })
    }

    /// Checks if the type of the expression matches type `t`.
    fn match_type(&self, context: &CompileContext, t: &Type) -> Result<(), Error> {
        let vt = self.get_type(context)?;

        if vt.can_cast(t) {
            Ok(())
        } else {
            Err(Error::invalid_type(t.clone(), vt, self.get_span()))
        }
    }
}
