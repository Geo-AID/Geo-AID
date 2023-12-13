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

use std::{fmt::Display, iter::Peekable};

use super::{parser::Parse, Error};

#[cfg(test)]
mod tests {
    use crate::{
        script::token::{Dot, Eq, Ident, Let, NamedIdent, Number, PointCollection, Semi, PointCollectionItem},
        span,
    };

    use super::{tokenize, Token};

    #[test]
    fn tokenizing() {
        let script = r#"let x = 5; # Setting x
let y = 5.5;
let ABC = Triangle;
        "#;

        assert_eq!(
            tokenize(script).unwrap(),
            vec![
                Token::Let(Let {
                    span: span!(1, 1, 1, 4)
                }),
                Token::Ident(Ident::Named(NamedIdent {
                    ident: String::from("x"),
                    span: span!(1, 5, 1, 6),
                    collection_likeness: 0.0
                })),
                Token::Eq(Eq {
                    span: span!(1, 7, 1, 8)
                }),
                Token::Number(Number {
                    span: span!(1, 9, 1, 10),
                    integral: 5,
                    decimal: 0,
                    decimal_places: 0,
                    dot: None
                }),
                Token::Semi(Semi {
                    span: span!(1, 10, 1, 11)
                }),
                Token::Let(Let {
                    span: span!(2, 1, 2, 4)
                }),
                Token::Ident(Ident::Named(NamedIdent {
                    ident: String::from("y"),
                    span: span!(2, 5, 2, 6),
                    collection_likeness: 0.0
                })),
                Token::Eq(Eq {
                    span: span!(2, 7, 2, 8)
                }),
                Token::Number(Number {
                    span: span!(2, 9, 2, 12),
                    integral: 5,
                    decimal: 5,
                    decimal_places: 1,
                    dot: Some(Dot {
                        span: span!(2, 10, 2, 11)
                    })
                }),
                Token::Semi(Semi {
                    span: span!(2, 12, 2, 13)
                }),
                Token::Let(Let {
                    span: span!(3, 1, 3, 4)
                }),
                Token::Ident(Ident::Collection(PointCollection {
                    collection: vec![
                        PointCollectionItem {
                            letter: 'A',
                            index: None,
                            primes: 0,
                            span: span!(3, 5, 3, 6)
                        },
                        PointCollectionItem {
                            letter: 'B',
                            index: None,
                            primes: 0,
                            span: span!(3, 6, 3, 7)
                        },
                        PointCollectionItem {
                            letter: 'B',
                            index: None,
                            primes: 0,
                            span: span!(3, 7, 3, 8)
                        }
                    ],
                    span: span!(3, 5, 3, 8)
                })),
                Token::Eq(Eq {
                    span: span!(3, 9, 3, 10)
                }),
                Token::Ident(Ident::Named(NamedIdent {
                    ident: String::from("Triangle"),
                    span: span!(3, 11, 3, 19),
                    collection_likeness: 0.0
                })),
                Token::Semi(Semi {
                    span: span!(3, 19, 3, 20)
                })
            ]
        );
    }
}

/// Defines a position in the script.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.line.cmp(&other.line) {
            std::cmp::Ordering::Equal => self.column.cmp(&other.column),
            v => v,
        }
    }
}

/// Defines a span in the script.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    #[must_use]
    pub fn join(self, other: Span) -> Self {
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

    #[must_use]
    pub fn overlaps(self, other: Span) -> bool {
        (self.start <= other.start && self.end >= other.start)
            || (other.start <= self.start && other.end >= self.start)
    }

    #[must_use]
    pub fn is_singleline(&self) -> bool {
        self.start.line == self.end.line
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.start.cmp(&other.start) {
            std::cmp::Ordering::Equal => Some(self.end.cmp(&other.end)),
            v => Some(v),
        }
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Semi {
    pub span: Span,
}

/// A '=' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Eq {
    pub span: Span,
}

/// A '(' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LParen {
    pub span: Span,
}

/// A ')' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RParen {
    pub span: Span,
}

/// A '{' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LBrace {
    pub span: Span,
}

/// A '}' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RBrace {
    pub span: Span,
}

/// A '[' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LSquare {
    pub span: Span,
}

/// A ']' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RSquare {
    pub span: Span,
}

/// A ',' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comma {
    pub span: Span,
}

/// A ':' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Colon {
    pub span: Span,
}

/// A '$' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Dollar {
    pub span: Span,
}

/// A '@' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct At {
    pub span: Span,
}

/// A 'let' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Let {
    pub span: Span,
}

/// A '+' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Plus {
    pub span: Span,
}

/// A '-' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Minus {
    pub span: Span,
}

/// A '*' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Asterisk {
    pub span: Span,
}

/// A '|' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Vertical {
    pub span: Span,
}

/// A '/' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Slash {
    pub span: Span,
}

/// A '<' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lt {
    pub span: Span,
}

/// A '>' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Gt {
    pub span: Span,
}

/// A '<=' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lteq {
    pub span: Span,
}

/// A '.' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Dot {
    pub span: Span,
}

/// A '>=' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Gteq {
    pub span: Span,
}

/// A '!' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Exclamation {
    pub span: Span,
}

/// A string, delimited by quotation marks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StrLit {
    pub span: Span,
    pub content: String
}

/// A '&' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ampersant {
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Semi(Semi),
    Eq(Eq),
    Comma(Comma),
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
    String(StrLit)
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
            Self::At(_) => write!(f, "@"),
            Self::LBrace(_) => write!(f, "{{"),
            Self::RBrace(_) => write!(f, "}}"),
            Self::LSquare(_) => write!(f, "["),
            Self::RSquare(_) => write!(f, "]"),
            Self::Colon(_) => write!(f, ":"),
            Self::String(s) => write!(f, "\"{}\"", s.content),
            Self::Ident(ident) => write!(
                f,
                "{}",
                match ident {
                    Ident::Named(named) => named.ident.clone(),
                    Ident::Collection(col) => format!("{col}")
                }
            ),
            Self::Number(num) => match num.dot {
                Some(_) => write!(
                    f,
                    "{}.{:0>places$}",
                    num.integral,
                    num.decimal,
                    places = num.decimal_places as usize
                ),
                None => write!(f, "{}", num.integral),
            },
        }
    }
}

impl Token {
    #[must_use]
    pub fn get_span(&self) -> Span {
        match self {
            Self::Semi(v) => v.span,
            Self::Eq(v) => v.span,
            Self::Comma(v) => v.span,
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
            Self::Number(v) => v.span,
            Self::Dollar(v) => v.span,
            Self::At(v) => v.span,
            Self::LBrace(v) => v.span,
            Self::RBrace(v) => v.span,
            Self::LSquare(v) => v.span,
            Self::RSquare(v) => v.span,
            Self::Ampersant(v) => v.span,
            Self::Colon(v) => v.span,
            Self::Dot(v) => v.span,
            Self::String(s) => s.span
        }
    }
}

/// A name.
#[derive(Debug, Clone)]
pub struct NamedIdent {
    pub span: Span,
    pub ident: String,
    pub collection_likeness: f64
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
    pub span: Span
}

impl Display for PointCollectionItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.letter,
            "'".repeat(self.primes as usize),
            self.index.as_ref().map_or(String::new(), |x| format!("_{x}"))
        )
    }
}

/// A point collection composed of point characters and the number of `'` characters following them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointCollection {
    pub collection: Vec<PointCollectionItem>,
    pub span: Span,
}

impl PointCollection {
    #[must_use]
    pub fn len(&self) -> usize {
        self.collection.len()
    }

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
                .map(|item| format!("{item}"))
                .collect::<String>()
        )
    }
}

/// An identifier. Either a point collection or a name.
#[derive(Debug, Clone, PartialEq, Eq)]
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

/// A number.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Number {
    pub span: Span,
    pub integral: u64,
    pub decimal: u64,
    pub decimal_places: u8,
    pub dot: Option<Dot>,
}

fn is_identifier_character(c: char) -> bool {
    c.is_alphabetic() || c.is_ascii_digit() || c == '_' || c == '\''
}

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

fn read_string<I: Iterator<Item = char>>(
    it: &mut Peekable<I>,
    position: &mut Position
) -> Result<StrLit, Error> {
    let mut content = String::new();
    let begin_pos = *position;
    let mut closed = false;

    // Assume first char to be correct.
    it.next();
    position.column += 1;

    while let Some(c) = it.next() {
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
                )
            });
        }

        if c == '\n' {
            return Err(Error::NewLineInString {
                error_span: span!(
                    begin_pos.line,
                    begin_pos.column,
                    position.line,
                    position.column
                )
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
                )
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
            )
        })
    }

    Ok(StrLit {
        span: span!(
            begin_pos.line,
            begin_pos.column,
            position.line,
            position.column
        ),
        content
    })
}

fn read_number<I: Iterator<Item = char>>(it: &mut Peekable<I>, position: &mut Position) -> Number {
    let mut integral: u64 = 0;
    let mut decimal: u64 = 0;
    let begin_pos = *position;
    let mut dot = None;

    while let Some(&c) = it.peek() {
        if c.is_ascii_digit() {
            integral *= 10;
            integral += (c as u64) - u64::from(b'0');
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
            break;
        } else {
            return Number {
                span: span!(
                    begin_pos.line,
                    begin_pos.column,
                    position.line,
                    position.column
                ),
                integral,
                decimal: 0,
                decimal_places: 0,
                dot,
            };
        }
    }

    let mut decimal_places = 0;

    while let Some(&c) = it.peek() {
        if c.is_ascii_digit() {
            decimal *= 10;
            decimal += (c as u64) - u64::from(b'0');
            decimal_places += 1;
            position.column += 1;
            it.next();
        } else {
            break;
        }
    }

    Number {
        span: span!(
            begin_pos.line,
            begin_pos.column,
            position.line,
            position.column
        ),
        integral,
        decimal,
        decimal_places,
        dot,
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
            span: span!(sp.start.line, sp.start.column + offset, sp.start.line, sp.start.column + offset + len)
        });

        offset += len;
    }

    invalid += chars.count();

    if invalid > 0 {
        return Ident::Named(NamedIdent {
            span: sp,
            ident,
            collection_likeness: (offset - invalid) as f64 / (offset as f64)
        });
    }

    Ident::Collection(collection)
}

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
            '@' => Token::At(At { span: sp }),
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

/// Tokenizes the given script.
///
/// # Errors
/// Emits an appropiate error if the script is invalid and tokenization fails.
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

                    tokens.push(Token::String(s));
                } else {
                    tokenize_special(&mut position, &mut tokens, c, &mut it)?;
                }
            }
        }
    }

    Ok(tokens)
}
