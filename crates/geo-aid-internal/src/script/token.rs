//! All functionality for turning scripts into series of tokens.

use std::{fmt::Display, iter::Peekable};

use serde::Serialize;

use self::number::{ParsedFloat, ParsedInt, ParsedIntBuilder};

use super::{parser::Parse, Error};
use geo_aid_derive::Parse;

pub mod number;

/// Defines a position in the script.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Position {
    /// The line number, starting at 1
    pub line: usize,
    /// The column index (character index), starting at 1
    pub column: usize,
}

/// Defines a span in the script.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    /// Starting position (included)
    pub start: Position,
    /// Ending position (exluded)
    pub end: Position,
}

impl Span {
    /// Create a span containing both `self` and `other`. If one of them is empty,
    /// returns the other.
    #[must_use]
    pub fn join(self, other: Span) -> Self {
        if self.is_empty() {
            other
        } else if other.is_empty() {
            self
        } else {
            Self {
                start: if self.start < other.start {
                    self.start
                } else {
                    other.start
                },
                end: if self.end > other.end {
                    self.end
                } else {
                    other.end
                },
            }
        }
    }

    /// Check if the spans are overlapping (share a position)
    #[must_use]
    pub fn overlaps(self, other: Span) -> bool {
        (self.start <= other.start && self.end >= other.start)
            || (other.start <= self.start && other.end >= self.start)
    }

    /// Check if the span is contained within a single line
    #[must_use]
    pub const fn is_single_line(&self) -> bool {
        self.start.line == self.end.line
    }

    /// Create an empty span. This is a special value used in different cases.
    #[must_use]
    pub const fn empty() -> Self {
        Span {
            start: Position { line: 0, column: 0 },
            end: Position { line: 0, column: 0 },
        }
    }

    /// Check if the span is empty (`start == end`)
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.start.cmp(&other.start) {
            std::cmp::Ordering::Equal => self.end.cmp(&other.end),
            v => v,
        }
    }
}

/// A helper span macro accepting start line and column and end line and column.
/// All numbers are 1-based
#[macro_export]
macro_rules! span {
    ($start_ln:expr, $start_col:expr, $end_ln:expr, $end_col:expr) => {
        $crate::script::token::Span {
            start: $crate::script::token::Position {
                line: $start_ln,
                column: $start_col,
            },
            end: $crate::script::token::Position {
                line: $end_ln,
                column: $end_col,
            },
        }
    };
}

/// A ';' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Semi {
    pub span: Span,
}

/// A '=' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Eq {
    pub span: Span,
}

/// A '(' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct LParen {
    pub span: Span,
}

/// A ')' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct RParen {
    pub span: Span,
}

/// A '{' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct LBrace {
    pub span: Span,
}

/// A '}' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct RBrace {
    pub span: Span,
}

/// A '[' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct LSquare {
    pub span: Span,
}

/// A ']' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct RSquare {
    pub span: Span,
}

/// A ',' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Comma {
    pub span: Span,
}

/// A '^' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Caret {
    pub span: Span,
}

/// A ':' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Colon {
    pub span: Span,
}

/// A '$' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Dollar {
    pub span: Span,
}

/// A '@' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct At {
    pub span: Span,
}

/// A 'let' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Let {
    pub span: Span,
}

/// A '+' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Plus {
    pub span: Span,
}

/// A '-' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Minus {
    pub span: Span,
}

/// A '*' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Asterisk {
    pub span: Span,
}

/// A '|' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Vertical {
    pub span: Span,
}

/// A '/' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Slash {
    pub span: Span,
}

/// A '<' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Lt {
    pub span: Span,
}

/// A '>' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Gt {
    pub span: Span,
}

/// A '<=' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Lteq {
    pub span: Span,
}

/// A '.' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Dot {
    pub span: Span,
}

/// A '>=' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Gteq {
    pub span: Span,
}

/// A '!' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Exclamation {
    pub span: Span,
}

/// A string, delimited by quotation marks.
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct StrLit {
    pub span: Span,
    pub content: String,
}

impl Display for StrLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.content)
    }
}

/// A '&' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Ampersant {
    pub span: Span,
}

/// A '?' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Parse)]
#[parse(token)]
pub struct Question {
    pub span: Span,
}

/// Any valid token of `GeoScript`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Semi(Semi),
    Eq(Eq),
    Comma(Comma),
    Caret(Caret),
    Let(Let),
    Plus(Plus),
    Minus(Minus),
    Asterisk(Asterisk),
    Vertical(Vertical),
    LParen(LParen),
    RParen(RParen),
    Slash(Slash),
    Lt(Lt),
    Gt(Gt),
    Lteq(Lteq),
    Gteq(Gteq),
    Exclamation(Exclamation),
    Ident(Ident),
    Number(Number),
    Dollar(Dollar),
    Ampersant(Ampersant),
    LBrace(LBrace),
    RBrace(RBrace),
    LSquare(LSquare),
    RSquare(RSquare),
    At(At),
    Colon(Colon),
    Dot(Dot),
    StrLit(StrLit),
    Question(Question),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Semi(_) => write!(f, ";"),
            Self::Eq(_) => write!(f, "="),
            Self::Comma(_) => write!(f, ","),
            Self::Dot(_) => write!(f, "."),
            Self::Let(_) => write!(f, "let"),
            Self::Plus(_) => write!(f, "+"),
            Self::Minus(_) => write!(f, "-"),
            Self::Asterisk(_) => write!(f, "*"),
            Self::Vertical(_) => write!(f, "|"),
            Self::LParen(_) => write!(f, "("),
            Self::RParen(_) => write!(f, ")"),
            Self::Slash(_) => write!(f, "/"),
            Self::Lt(_) => write!(f, "<"),
            Self::Gt(_) => write!(f, ">"),
            Self::Lteq(_) => write!(f, "<="),
            Self::Gteq(_) => write!(f, ">="),
            Self::Exclamation(_) => write!(f, "!"),
            Self::Dollar(_) => write!(f, "$"),
            Self::Ampersant(_) => write!(f, "&"),
            Self::Question(_) => write!(f, "?"),
            Self::At(_) => write!(f, "@"),
            Self::LBrace(_) => write!(f, "{{"),
            Self::RBrace(_) => write!(f, "}}"),
            Self::LSquare(_) => write!(f, "["),
            Self::RSquare(_) => write!(f, "]"),
            Self::Caret(_) => write!(f, "^"),
            Self::Colon(_) => write!(f, ":"),
            Self::StrLit(s) => write!(f, "\"{}\"", s.content),
            Self::Ident(ident) => write!(
                f,
                "{}",
                match ident {
                    Ident::Named(named) => named.ident.clone(),
                    Ident::Collection(col) => format!("{col}"),
                }
            ),
            Self::Number(num) => match num {
                Number::Integer(v) => write!(f, "{}", v.parsed),
                Number::Float(v) => write!(f, "{}", v.parsed),
            },
        }
    }
}

impl Token {
    /// Get the token's span.
    #[must_use]
    pub fn get_span(&self) -> Span {
        match self {
            Self::Semi(v) => v.span,
            Self::Eq(v) => v.span,
            Self::Comma(v) => v.span,
            Self::Caret(v) => v.span,
            Self::Let(v) => v.span,
            Self::Plus(v) => v.span,
            Self::Minus(v) => v.span,
            Self::Asterisk(v) => v.span,
            Self::Vertical(v) => v.span,
            Self::LParen(v) => v.span,
            Self::RParen(v) => v.span,
            Self::Slash(v) => v.span,
            Self::Lt(v) => v.span,
            Self::Gt(v) => v.span,
            Self::Lteq(v) => v.span,
            Self::Gteq(v) => v.span,
            Self::Exclamation(v) => v.span,
            Self::Ident(v) => v.get_span(),
            Self::Number(v) => v.get_span(),
            Self::Dollar(v) => v.span,
            Self::At(v) => v.span,
            Self::LBrace(v) => v.span,
            Self::RBrace(v) => v.span,
            Self::LSquare(v) => v.span,
            Self::RSquare(v) => v.span,
            Self::Ampersant(v) => v.span,
            Self::Question(v) => v.span,
            Self::Colon(v) => v.span,
            Self::Dot(v) => v.span,
            Self::StrLit(s) => s.span,
        }
    }
}

/// A name identifier, as opposed to a point collection identifier.
/// For more details, see [`PointCollection`]
#[derive(Debug, Clone)]
pub struct NamedIdent {
    /// The identifier span
    pub span: Span,
    /// The identifier characters
    pub ident: String,
    /// How likely it is that this identifier should have been a collection.
    /// Used for error reporting.
    pub collection_likeness: f64,
}

impl PartialEq for NamedIdent {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.ident == other.ident
    }
}

impl std::cmp::Eq for NamedIdent {}

/// An item of a point collection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointCollectionItem {
    /// The point's letter
    pub letter: char,
    /// The point's optional index.
    pub index: Option<String>,
    /// The prime count.
    pub primes: u8,
    /// The span of the point
    pub span: Span,
}

impl Display for PointCollectionItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.letter,
            "'".repeat(self.primes as usize),
            self.index
                .as_ref()
                .map_or(String::new(), |x| format!("_{x}"))
        )
    }
}

/// A point collection composed of single point identifiers.
/// A point identifier is an uppercase alphabetic character and a number of `'` characters following it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointCollection {
    /// Each point identifier of this collection
    pub collection: Vec<PointCollectionItem>,
    pub span: Span,
}

impl PointCollection {
    /// How many points are in this collection
    #[must_use]
    pub fn len(&self) -> usize {
        self.collection.len()
    }

    /// Whether this collection is empty.
    /// It's here mostly to shut clippy up. Point collections cannot
    /// be 0-length.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.collection.is_empty()
    }
}

impl Display for PointCollection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.collection
                .iter()
                .fold(String::new(), |b, x| b + &x.to_string())
        )
    }
}

/// An identifier. Either a point collection or a name.
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
#[parse(token)]
pub enum Ident {
    Named(NamedIdent),
    Collection(PointCollection),
}

impl Ident {
    #[must_use]
    pub fn as_collection(&self) -> Option<&PointCollection> {
        if let Self::Collection(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_ident(&self) -> Option<&NamedIdent> {
        if let Self::Named(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ident::Named(named) => write!(f, "{}", named.ident),
            Ident::Collection(col) => write!(f, "{col}"),
        }
    }
}

/// A number token. Can represent an integer or a decimal.
#[derive(Debug, Clone, PartialEq, Eq, Parse)]
#[parse(token)]
pub enum Number {
    Integer(TokInteger),
    Float(TokFloat),
}

impl Number {
    /// Convert this number to a float.
    #[must_use]
    pub fn to_float(&self) -> f64 {
        match self {
            Self::Integer(i) => i.parsed.to_float(),
            Self::Float(f) => f.parsed.to_float(),
        }
    }

    /// Check if this token represents a 0.
    #[must_use]
    pub fn is_zero(&self) -> bool {
        match self {
            Self::Integer(i) => i.parsed.is_zero(),
            Self::Float(f) => f.parsed.is_zero(),
        }
    }

    /// Check if this token represents a 1.
    #[must_use]
    pub fn is_one(&self) -> bool {
        match self {
            Self::Integer(i) => i.parsed.is_one(),
            Self::Float(f) => f.parsed.is_one(),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
        }
    }
}

/// An integer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokInteger {
    pub span: Span,
    /// The parsed integer.
    pub parsed: ParsedInt,
}

impl Display for TokInteger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parsed)
    }
}

/// A decimal number.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokFloat {
    pub span: Span,
    /// The splitting dot.
    pub dot: Dot,
    /// The parsed digits.
    pub parsed: ParsedFloat,
}

impl Display for TokFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parsed)
    }
}

/// Check if `c` is a valid character for an identifier.
fn is_identifier_character(c: char) -> bool {
    c.is_alphabetic() || c.is_ascii_digit() || c == '_' || c == '\''
}

/// Read an identifier (without distinguishing point collections from names)
/// from a char iterator.
fn read_identifier<I: Iterator<Item = char>>(
    it: &mut Peekable<I>,
    position: &mut Position,
) -> (Span, String) {
    let mut str = String::new();
    let begin_pos = *position;

    while let Some(&c) = it.peek() {
        if is_identifier_character(c) {
            str.push(c);
            position.column += 1;
            it.next();
        } else {
            break;
        }
    }

    (
        span!(
            begin_pos.line,
            begin_pos.column,
            position.line,
            position.column
        ),
        str,
    )
}

/// Read a string literal from a string iterator.
fn read_string<I: Iterator<Item = char>>(
    it: &mut Peekable<I>,
    position: &mut Position,
) -> Result<StrLit, Error> {
    let mut content = String::new();
    let begin_pos = *position;
    let mut closed = false;

    // Assume first char to be correct.
    it.next();
    position.column += 1;

    for c in it.by_ref() {
        position.column += 1;

        // Guard for non-ASCII
        if !c.is_ascii() {
            return Err(Error::InvalidCharacter {
                character: c,
                error_span: span!(
                    position.line,
                    position.column - 1,
                    position.line,
                    position.column
                ),
            });
        }

        if c == '\n' {
            return Err(Error::NewLineInString {
                error_span: span!(
                    begin_pos.line,
                    begin_pos.column,
                    position.line,
                    position.column
                ),
            });
        } else if c == '"' {
            closed = true;
            break;
        } else if c.is_ascii_whitespace() {
            content.push(' ');
        } else if c.is_ascii_control() {
            return Err(Error::InvalidCharacter {
                character: c,
                error_span: span!(
                    position.line,
                    position.column - 1,
                    position.line,
                    position.column
                ),
            });
        } else {
            content.push(c);
        }
    }

    if !closed {
        return Err(Error::UnclosedString {
            error_span: span!(
                begin_pos.line,
                begin_pos.column,
                position.line,
                position.column
            ),
        });
    }

    Ok(StrLit {
        span: span!(
            begin_pos.line,
            begin_pos.column,
            position.line,
            position.column
        ),
        content,
    })
}

/// Read a number token from a char iterator.
fn read_number<I: Iterator<Item = char>>(it: &mut Peekable<I>, position: &mut Position) -> Number {
    let mut integer = ParsedIntBuilder::new();
    let mut floating = None;
    let begin_pos = *position;
    let mut dot = None;

    while let Some(&c) = it.peek() {
        if c.is_ascii_digit() {
            integer.push_digit((c as u8) - b'0');
            position.column += 1;
            it.next();
        } else if c == '.' {
            dot = Some(Dot {
                span: span!(
                    position.line,
                    position.column,
                    position.line,
                    position.column + 1
                ),
            });
            position.column += 1;
            it.next();
            floating = Some(integer.dot());
            break;
        } else {
            return Number::Integer(TokInteger {
                span: span!(
                    begin_pos.line,
                    begin_pos.column,
                    position.line,
                    position.column
                ),
                parsed: integer.build(),
            });
        }
    }

    if let Some(mut floating) = floating {
        while let Some(&c) = it.peek() {
            if c.is_ascii_digit() {
                floating.push_digit((c as u8) - b'0');
                position.column += 1;
                it.next();
            } else {
                break;
            }
        }

        Number::Float(TokFloat {
            span: span!(
                begin_pos.line,
                begin_pos.column,
                position.line,
                position.column
            ),
            dot: dot.unwrap(),
            parsed: floating.build(),
        })
    } else {
        unreachable!()
    }
}

/// Decides whether the given string is a standard named identifier or a point collection.
fn dispatch_ident(sp: Span, ident: String) -> Ident {
    let mut collection = PointCollection {
        collection: vec![],
        span: sp,
    };

    let mut chars = ident.chars().peekable();
    let mut offset = 0;

    // If the point collection is not a point collection, this will be non-zero
    let mut invalid = 0;

    while let Some(letter) = chars.next() {
        if !letter.is_ascii_uppercase() {
            invalid += 1;
        }

        let mut len = 1;
        let mut primes = 0;

        while let Some('\'') = chars.peek().copied() {
            primes += 1;
            len += 1;
            chars.next();
        }

        let index = if chars.peek().copied() == Some('_') {
            chars.next();
            len += 1;
            let mut index = String::new();

            while chars.peek().is_some_and(char::is_ascii_digit) {
                len += 1;
                index.push(chars.next().unwrap());
            }

            if index.is_empty() {
                // Assume index exists, go on
                invalid += 1;
            }

            Some(index)
        } else {
            None
        };

        collection.collection.push(PointCollectionItem {
            letter,
            index,
            primes,
            span: span!(
                sp.start.line,
                sp.start.column + offset,
                sp.start.line,
                sp.start.column + offset + len
            ),
        });

        offset += len;
    }

    invalid += chars.count();

    if invalid > 0 {
        #[allow(clippy::cast_precision_loss)]
        return Ident::Named(NamedIdent {
            span: sp,
            ident,
            collection_likeness: (offset - invalid) as f64 / (offset as f64),
        });
    }

    Ident::Collection(collection)
}

/// Recognise special characters
fn tokenize_special<I: Iterator<Item = char>>(
    position: &mut Position,
    tokens: &mut Vec<Token>,
    c: char,
    it: &mut Peekable<I>,
) -> Result<(), Error> {
    let sp = span!(
        position.line,
        position.column,
        position.line,
        position.column + 1
    );

    if c == '=' {
        let last = tokens.last().cloned();

        match last {
            Some(Token::Lt(Lt { span })) => {
                if span
                    == span!(
                        sp.start.line,
                        sp.start.column - 1,
                        sp.start.line,
                        sp.start.column
                    )
                {
                    *tokens.last_mut().unwrap() = Token::Lteq(Lteq {
                        span: span!(
                            sp.start.line,
                            sp.start.column - 1,
                            sp.start.line,
                            sp.start.column + 1
                        ),
                    });
                }
            }
            Some(Token::Gt(Gt { span })) => {
                if span
                    == span!(
                        sp.start.line,
                        sp.start.column - 1,
                        sp.start.line,
                        sp.start.column
                    )
                {
                    *tokens.last_mut().unwrap() = Token::Gteq(Gteq {
                        span: span!(
                            sp.start.line,
                            sp.start.column - 1,
                            sp.start.line,
                            sp.start.column + 1
                        ),
                    });
                }
            }
            _ => tokens.push(Token::Eq(Eq { span: sp })),
        }
    } else {
        tokens.push(match c {
            ';' => Token::Semi(Semi { span: sp }),
            ',' => Token::Comma(Comma { span: sp }),
            '.' => Token::Dot(Dot { span: sp }),
            '+' => Token::Plus(Plus { span: sp }),
            '-' => Token::Minus(Minus { span: sp }),
            '*' => Token::Asterisk(Asterisk { span: sp }),
            '/' => Token::Slash(Slash { span: sp }),
            '(' => Token::LParen(LParen { span: sp }),
            ')' => Token::RParen(RParen { span: sp }),
            '|' => Token::Vertical(Vertical { span: sp }),
            '<' => Token::Lt(Lt { span: sp }),
            '>' => Token::Gt(Gt { span: sp }),
            '!' => Token::Exclamation(Exclamation { span: sp }),
            '$' => Token::Dollar(Dollar { span: sp }),
            '&' => Token::Ampersant(Ampersant { span: sp }),
            '?' => Token::Question(Question { span: sp }),
            '@' => Token::At(At { span: sp }),
            '^' => Token::Caret(Caret { span: sp }),
            '{' => Token::LBrace(LBrace { span: sp }),
            '}' => Token::RBrace(RBrace { span: sp }),
            '[' => Token::LSquare(LSquare { span: sp }),
            ']' => Token::RSquare(RSquare { span: sp }),
            ':' => Token::Colon(Colon { span: sp }),
            _ => {
                return Err(Error::InvalidCharacter {
                    character: c,
                    error_span: sp,
                })
            }
        });
    }

    position.column += 1;
    it.next();

    Ok(())
}

/// Tokenizes the given script (turns it into a series of tokens).
///
/// # Errors
/// Emits an appropriate error if the script is invalid and tokenization fails.
pub fn tokenize(input: &str) -> Result<Vec<Token>, Error> {
    let mut it = input.chars().peekable();
    let mut tokens = vec![];
    let mut position = Position { line: 1, column: 1 };

    loop {
        match it.peek() {
            None => break,
            Some(&c) => {
                if c.is_whitespace() {
                    if c == '\n' {
                        position.line += 1;
                        position.column = 0;
                    }

                    position.column += 1;
                    it.next();
                } else if c.is_alphabetic() || c == '_' {
                    let (sp, ident) = read_identifier(&mut it, &mut position);

                    tokens.push(match ident.as_str() {
                        "let" => Token::Let(Let { span: sp }),
                        _ => Token::Ident(dispatch_ident(sp, ident)),
                    });
                } else if c.is_ascii_digit() {
                    tokens.push(Token::Number(read_number(&mut it, &mut position)));
                } else if c == '#' {
                    position.line += 1;
                    position.column = 1;
                    while let Some(comment) = it.by_ref().next() {
                        if comment == '\n' {
                            break;
                        }
                    }
                } else if c == '"' {
                    let s = read_string(&mut it, &mut position)?;

                    tokens.push(Token::StrLit(s));
                } else {
                    tokenize_special(&mut position, &mut tokens, c, &mut it)?;
                }
            }
        }
    }

    Ok(tokens)
}
