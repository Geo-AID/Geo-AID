use std::iter::Peekable;

use super::Error;

#[cfg(test)]
mod tests {
    use crate::{
        script::token::{
            Eq, Ident, Let, NamedIdent, Number, PointCollection, Position, Semi, Span,
        },
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
                    span: span!(1, 5, 1, 6)
                })),
                Token::Eq(Eq {
                    span: span!(1, 7, 1, 8)
                }),
                Token::Number(Number {
                    span: span!(1, 9, 1, 10),
                    integral: 5,
                    decimal: 0,
                    decimal_places: 0
                }),
                Token::Semi(Semi {
                    span: span!(1, 10, 1, 11)
                }),
                Token::Let(Let {
                    span: span!(2, 1, 2, 4)
                }),
                Token::Ident(Ident::Named(NamedIdent {
                    ident: String::from("y"),
                    span: span!(2, 5, 2, 6)
                })),
                Token::Eq(Eq {
                    span: span!(2, 7, 2, 8)
                }),
                Token::Number(Number {
                    span: span!(2, 9, 2, 12),
                    integral: 5,
                    decimal: 5,
                    decimal_places: 2
                }),
                Token::Semi(Semi {
                    span: span!(2, 12, 2, 13)
                }),
                Token::Let(Let {
                    span: span!(3, 1, 3, 4)
                }),
                Token::Ident(Ident::Collection(PointCollection {
                    collection: vec![('A', 0), ('B', 0), ('C', 0),],
                    span: span!(3, 5, 3, 8)
                })),
                Token::Eq(Eq {
                    span: span!(3, 9, 3, 10)
                }),
                Token::Ident(Ident::Named(NamedIdent {
                    ident: String::from("Triangle"),
                    span: span!(3, 11, 3, 19)
                })),
                Token::Semi(Semi {
                    span: span!(3, 19, 3, 20)
                })
            ]
        );
    }
}

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
            std::cmp::Ordering::Less => std::cmp::Ordering::Less,
            std::cmp::Ordering::Equal => match self.column.cmp(&other.column) {
                std::cmp::Ordering::Less => std::cmp::Ordering::Less,
                std::cmp::Ordering::Equal => std::cmp::Ordering::Equal,
                std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
            },
            std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
        }
    }
}

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
}

#[macro_export]
macro_rules! span {
    ($start_ln:expr, $start_col:expr, $end_ln:expr, $end_col:expr) => {
        Span {
            start: Position {
                line: $start_ln,
                column: $start_col,
            },
            end: Position {
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

/// A ',' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comma {
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

#[derive(Debug, Clone, PartialEq)]
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
}

/// A name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedIdent {
    pub span: Span,
    pub ident: String,
}

/// A point collection composed of point characters and the number of `'` characters following them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointCollection {
    pub collection: Vec<(char, u8)>,
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
}

/// A 'ident' token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Number {
    pub span: Span,
    pub integral: u64,
    pub decimal: u64,
    pub decimal_places: u8,
}

fn is_identifier_character(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '\''
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

fn read_number<I: Iterator<Item = char>>(it: &mut Peekable<I>, position: &mut Position) -> Number {
    let mut integral: u64 = 0;
    let mut decimal: u64 = 0;
    let begin_pos = *position;

    while let Some(&c) = it.peek() {
        if c.is_ascii_digit() {
            integral *= 10;
            integral += (c as u64) - u64::from(b'0');
            position.column += 1;
            it.next();
        } else if c == '.' {
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
    }
}

fn dispatch_ident(sp: Span, ident: String) -> Ident {
    let mut collection = PointCollection {
        collection: vec![],
        span: sp,
    };

    for c in ident.chars() {
        if c.is_uppercase() {
            collection.collection.push((c, 0));
        } else if c == '\'' && !collection.collection.is_empty() {
            collection.collection.last_mut().unwrap().1 += 1;
        } else {
            return Ident::Named(NamedIdent { span: sp, ident });
        }
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
            _ => return Err(Error::invalid_character(c)),
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
                } else {
                    tokenize_special(&mut position, &mut tokens, c, &mut it)?;
                }
            }
        }
    }

    Ok(tokens)
}
