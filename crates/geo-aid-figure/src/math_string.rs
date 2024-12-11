use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use serde::de::{Error, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;

/// Special math characters as their string representatives
pub const SPECIAL_MATH: [&str; 49] = [
    "alpha", "Alpha", "beta", "Beta", "gamma", "Gamma", "delta", "Delta", "epsilon", "Epsilon",
    "zeta", "Zeta", "eta", "Eta", "theta", "Theta", "iota", "Iota", "kappa", "Kappa", "lambda",
    "Lambda", "mu", "Mu", "nu", "Nu", "xi", "Xi", "omicron", "Omicron", "phi", "Phi", "rho", "Rho",
    "sigma", "Sigma", "tau", "Tau", "upsilon", "Upsilon", "phi", "Phi", "chi", "Chi", "psi", "Psi",
    "omega", "Omega", "quote",
];

/// A span in a string. Does not account for lines
#[derive(Debug, Clone, Copy, Default)]
pub struct StringSpan {
    /// The first position in the span
    pub start: usize,
    /// The first position NOT in the span
    pub end: usize,
}

impl Display for StringSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{})", self.start, self.end)
    }
}

/// An error while parsing math string
#[derive(Debug, Clone)]
pub struct ParseError {
    /// Span of the error
    pub span: StringSpan,
    /// Kind of the error
    pub kind: ParseErrorKind,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "error at {}: {}", self.span, self.kind)
    }
}

impl std::error::Error for ParseError {}

/// The parsing error kind
#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    /// A special character code was not recognised
    SpecialNotRecognised(String),
    /// A nested index (a `_{...}` inside a `_{...}`). This is not valid
    NestedIndex,
    /// A special tag (`[`) was left unclosed
    UnclosedSpecialTag(String),
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SpecialNotRecognised(special) => write!(f, "special '{special}' not recognised"),
            Self::NestedIndex => write!(f, "nested index is not valid"),
            Self::UnclosedSpecialTag(special) => {
                write!(f, "unclosed special tag. Parsed '{special}'")
            }
        }
    }
}

/// Normal/lower index in math text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum MathIndex {
    /// Set the current indexing to normal text
    Normal,
    /// Set the current indexing to lower (subscript)
    Lower,
}

impl Display for MathIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal => write!(f, "}}"),
            Self::Lower => write!(f, "_{{"),
        }
    }
}

/// A math character is either just an ASCII character or a special character.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum MathChar {
    /// A standard ASCII character.
    Ascii(char),
    /// Special character denoted by a string.
    Special(MathSpecial),
    /// Starts lower index.
    SetIndex(MathIndex),
    /// Prime (a tick)
    Prime,
}

impl Display for MathChar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ascii(c) => write!(f, "{c}"),
            Self::Special(c) => write!(f, "[{c}]"),
            Self::SetIndex(x) => write!(f, "{x}"),
            Self::Prime => write!(f, "'"),
        }
    }
}

/// A special character
#[derive(Debug, Clone, Copy, FromPrimitive, ToPrimitive, PartialEq, Eq, Serialize)]
pub enum MathSpecial {
    /// &alpha;
    Alpha,
    /// &Alpha;
    AlphaUpper,
    /// &beta;
    Beta,
    /// &Beta;
    BetaUpper,
    /// &gamma;
    Gamma,
    /// &Gamma;
    GammaUpper,
    /// &delta;
    Delta,
    /// &Delta;
    DeltaUpper,
    /// &epsilon;
    Epsilon,
    /// &Epsilon;
    EpsilonUpper,
    /// &zeta;
    Zeta,
    /// &Zeta;
    ZetaUpper,
    /// &eta;
    Eta,
    /// &Eta;
    EtaUpper,
    /// &theta;
    Theta,
    /// &Theta;
    ThetaUpper,
    /// &iota;
    Iota,
    /// &Iota;
    IotaUpper,
    /// &kappa;
    Kappa,
    /// &Kappa;
    KappaUpper,
    /// &lambda;
    Lambda,
    /// &Lambda;
    LambdaUpper,
    /// &mu;
    Mu,
    /// &Mu;
    MuUpper,
    /// &nu;
    Nu,
    /// &Nu;
    NuUpper,
    /// &xi;
    Xi,
    /// &Xi;
    XiUpper,
    /// &omicron;
    Omicron,
    /// &Omicron;
    OmicronUpper,
    /// &pi;
    Pi,
    /// &Pi;
    PiUpper,
    /// &rho;
    Rho,
    /// &Rho;
    RhoUpper,
    /// &sigme;
    Sigma,
    /// &Sigme;
    SigmaUpper,
    /// &tau;
    Tau,
    /// &Tau;
    TauUpper,
    /// &upsilon;
    Upsilon,
    /// &Upsilon;
    UpsilonUpper,
    /// &phi;
    Phi,
    /// &Phi;
    PhiUpper,
    /// &chi;
    Chi,
    /// &Chi;
    ChiUpper,
    /// &psi;
    Psi,
    /// &Psi;
    PsiUpper,
    /// &omega;
    Omega,
    /// &Omega;
    OmegaUpper,
    /// "
    Quote,
}

impl MathSpecial {
    /// Whether this special is an alphabetic one
    #[must_use]
    pub fn is_alphabetic(self) -> bool {
        self != Self::Quote
    }

    /// If `char_code` is a valid special math character, returns it in a `Some`. `None` otherwise.
    ///
    /// # Panics
    /// Any panic in this function are a bug.
    #[must_use]
    pub fn parse(char_code: &str) -> Option<Self> {
        SPECIAL_MATH
            .iter()
            .enumerate()
            .find(|x| *x.1 == char_code)
            .map(|x| MathSpecial::from_usize(x.0).unwrap())
    }
}

impl Display for MathSpecial {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", SPECIAL_MATH[*self as usize])
    }
}

/// A series of math characters.
#[derive(Debug, Clone, Default)]
pub struct MathString(Vec<MathChar>);

impl MathString {
    /// Creates a new, empty math string
    #[must_use]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Checks if the math string is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates a new string from a `&str`, treating each character literally
    #[must_use]
    pub fn raw(source: &str) -> Self {
        Self(source.chars().map(MathChar::Ascii).collect())
    }
}

impl Deref for MathString {
    type Target = Vec<MathChar>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MathString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for MathString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for c in &self.0 {
            write!(f, "{c}")?;
        }

        Ok(())
    }
}

impl FromStr for MathString {
    type Err = ParseError;

    fn from_str(content: &str) -> Result<Self, Self::Err> {
        let mut ignore_next = false;
        let mut math_string = Vec::new();
        let mut indexed = false;
        let mut collect_special = false;
        let mut special = String::new();
        let mut index_delimited = false;

        let mut special_start = 0;

        for (char_count, c) in content.chars().enumerate() {
            if collect_special {
                if ignore_next || (c != ']' && c != '\\') {
                    special.push(c);
                    ignore_next = false;
                } else if c == ']' {
                    math_string.push(MathChar::Special(MathSpecial::parse(&special).ok_or_else(
                        || ParseError {
                            span: StringSpan {
                                start: special_start + 1,
                                end: char_count,
                            },
                            kind: ParseErrorKind::SpecialNotRecognised(mem::take(&mut special)),
                        },
                    )?));

                    // special.clear();
                    collect_special = false;
                } else {
                    ignore_next = true;
                }
            } else if ignore_next {
                math_string.push(MathChar::Ascii(c));
                ignore_next = false;
            } else if c == '\\' {
                ignore_next = true;
            } else if c == '_' {
                if indexed {
                    return Err(ParseError {
                        span: StringSpan {
                            start: 0,
                            end: content.len(),
                        },
                        kind: ParseErrorKind::NestedIndex,
                    });
                }

                math_string.push(MathChar::SetIndex(MathIndex::Lower));
                indexed = true;
            } else if c == '[' {
                special_start = char_count;
                collect_special = true;
            } else if c == ' ' {
                if indexed && !index_delimited {
                    indexed = false;
                    math_string.push(MathChar::SetIndex(MathIndex::Normal));
                }

                math_string.push(MathChar::Ascii(c));
            } else if c == '{'
                && math_string.last().copied() == Some(MathChar::SetIndex(MathIndex::Lower))
            {
                index_delimited = true;
            } else if c == '}' && index_delimited {
                indexed = false;
                index_delimited = false;
                math_string.push(MathChar::SetIndex(MathIndex::Normal));
            } else if c == '\'' {
                math_string.push(MathChar::Prime);
            } else {
                math_string.push(MathChar::Ascii(c));
            }
        }

        if indexed {
            math_string.push(MathChar::SetIndex(MathIndex::Normal));
        }

        if collect_special {
            // Special tag was not closed
            return Err(ParseError {
                span: StringSpan {
                    start: special_start,
                    end: content.len(),
                },
                kind: ParseErrorKind::UnclosedSpecialTag(special),
            });
        }

        Ok(Self(math_string))
    }
}

impl Serialize for MathString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = String::new();

        for c in &self.0 {
            match c {
                MathChar::Prime => s.push('c'),
                MathChar::SetIndex(MathIndex::Normal) => s.push('}'),
                MathChar::SetIndex(MathIndex::Lower) => s += "_{",
                MathChar::Ascii(c) => s.extend(['\\', *c]),
                MathChar::Special(c) => {
                    s.push('[');
                    s += SPECIAL_MATH[*c as usize];
                    s.push(']');
                }
            }
        }

        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for MathString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(MathStringVisitor)
    }
}

struct MathStringVisitor;

impl Visitor<'_> for MathStringVisitor {
    type Value = MathString;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("a valid math string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        MathString::from_str(v).map_err(|err| E::custom(err))
    }
}
