use std::{fmt::Display, str::FromStr};

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use serde::Serialize;
use crate::geometry::ValueEnum;
use crate::script::math::{EntityKind, IndexMap, Reconstruct, ReconstructCtx, Reindex, VarIndex};

use crate::span;

use super::math::Entity;
use super::{parser::{FromProperty, Parse, PropertyValue}, token::{Ident, PointCollectionItem, Span}, unroll::most_similar, Error, math};

pub const SPECIAL_MATH: [&str; 49] = [
    "alpha", "Alpha", "beta", "Beta", "gamma", "Gamma", "delta", "Delta", "epsilon", "Epsilon",
    "zeta", "Zeta", "eta", "Eta", "theta", "Theta", "iota", "Iota", "kappa", "Kappa", "lambda",
    "Lambda", "mu", "Mu", "nu", "Nu", "xi", "Xi", "omicron", "Omicron", "phi", "Phi", "rho", "Rho",
    "sigma", "Sigma", "tau", "Tau", "upsilon", "Upsilon", "phi", "Phi", "chi", "Chi", "psi", "Psi",
    "omega", "Omega", "quote",
];

/// The display mode of the expression.
#[derive(Debug, Clone, Serialize, Copy, Default)]
pub enum Style {
    Dotted,
    Dashed,
    Bold,
    #[default]
    Solid,
}

#[derive(Debug, Clone, Serialize)]
pub struct PointItem {
    pub id: VarIndex,
    pub label: MathString,
    pub display_dot: bool
}

impl Reindex for PointItem {
    fn reindex(&mut self, map: &IndexMap) {
        self.id.reindex(map);
    }
}

impl Reconstruct for PointItem {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        Self {
            id: self.id.reconstruct(ctx),
            ..self
        }
    }
}

impl From<PointItem> for Item {
    fn from(value: PointItem) -> Self {
        Self::Point(value)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct CircleItem {
    pub id: VarIndex,
    pub label: MathString,
    pub style: Style
}

impl Reindex for CircleItem {
    fn reindex(&mut self, map: &IndexMap) {
        self.id.reindex(map);
    }
}

impl Reconstruct for CircleItem {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        Self {
            id: self.id.reconstruct(ctx),
            ..self
        }
    }
}

impl From<CircleItem> for Item {
    fn from(value: CircleItem) -> Self {
        Self::Circle(value)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct LineItem {
    pub id: VarIndex,
    pub label: MathString,
    pub style: Style
}

impl Reindex for LineItem {
    fn reindex(&mut self, map: &IndexMap) {
        self.id.reindex(map);
    }
}

impl Reconstruct for LineItem {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        Self {
            id: self.id.reconstruct(ctx),
            ..self
        }
    }
}

impl From<LineItem> for Item {
    fn from(value: LineItem) -> Self {
        Self::Line(value)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct RayItem {
    pub p_id: VarIndex,
    pub q_id: VarIndex,
    pub label: MathString,
    pub style: Style
}

impl Reindex for RayItem {
    fn reindex(&mut self, map: &IndexMap) {
        self.p_id.reindex(map);
        self.q_id.reindex(map);
    }
}

impl Reconstruct for RayItem {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        Self {
            p_id: self.p_id.reconstruct(ctx),
            q_id: self.q_id.reconstruct(ctx),
            ..self
        }
    }
}

impl From<RayItem> for Item {
    fn from(value: RayItem) -> Self {
        Self::Ray(value)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SegmentItem {
    pub p_id: VarIndex,
    pub q_id: VarIndex,
    pub label: MathString,
    pub style: Style
}

impl From<SegmentItem> for Item {
    fn from(value: SegmentItem) -> Self {
        Self::Segment(value)
    }
}

impl Reindex for SegmentItem {
    fn reindex(&mut self, map: &IndexMap) {
        self.p_id.reindex(map);
        self.q_id.reindex(map);
    }
}

impl Reconstruct for SegmentItem {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        Self {
            p_id: self.p_id.reconstruct(ctx),
            q_id: self.q_id.reconstruct(ctx),
            ..self
        }
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Point(PointItem),
    Circle(CircleItem),
    Line(LineItem),
    Ray(RayItem),
    Segment(SegmentItem)
}

impl Reindex for Item {
    fn reindex(&mut self, map: &IndexMap) {
        match self {
            Self::Point(v) => v.reindex(map),
            Self::Circle(v) => v.reindex(map),
            Self::Line(v) => v.reindex(map),
            Self::Ray(v) => v.reindex(map),
            Self::Segment(v) => v.reindex(map),
        }
    }
}

impl Reconstruct for Item {
    fn reconstruct(self, ctx: &mut ReconstructCtx) -> Self {
        match self {
            Self::Point(v) => Self::Point(v.reconstruct(ctx)),
            Self::Circle(v) => Self::Circle(v.reconstruct(ctx)),
            Self::Line(v) => Self::Line(v.reconstruct(ctx)),
            Self::Ray(v) => Self::Ray(v.reconstruct(ctx)),
            Self::Segment(v) => Self::Segment(v.reconstruct(ctx))
        }
    }
}

/// Defines the visual data of the figure.
#[derive(Debug, Default)]
pub struct Figure {
    /// Entities used by the figure
    pub entities: Vec<EntityKind>,
    /// Variables used by the figure
    pub variables: Vec<math::Expr<()>>,
    /// Drawn items with meta
    pub items: Vec<Item>
}

/// Generated figure, created by the engine
#[derive(Debug, Clone)]
pub struct Generated {
    /// Entities used by the figure
    pub entities: Vec<Entity<ValueEnum>>,
    /// Variables used by the figure
    pub variables: Vec<math::Expr<ValueEnum>>,
    /// Drawn items with meta
    pub items: Vec<Item>
}

/// Normal/lower index in math text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum MathIndex {
    Normal,
    Lower,
}

impl Display for MathIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ascii(c) => write!(f, "{c}"),
            Self::Special(c) => write!(f, "[{c}]"),
            Self::SetIndex(x) => write!(f, "{x}"),
            Self::Prime => write!(f, "'"),
        }
    }
}

/// A special character.
#[derive(Debug, Clone, Copy, FromPrimitive, ToPrimitive, PartialEq, Eq, Serialize)]
pub enum MathSpecial {
    Alpha,
    AlphaUpper,
    Beta,
    BetaUpper,
    Gamma,
    GammaUpper,
    Delta,
    DeltaUpper,
    Epsilon,
    EpsilonUpper,
    Zeta,
    ZetaUpper,
    Eta,
    EtaUpper,
    Theta,
    ThetaUpper,
    Iota,
    IotaUpper,
    Kappa,
    KappaUpper,
    Lambda,
    LambdaUpper,
    Mu,
    MuUpper,
    Nu,
    NuUpper,
    Xi,
    XiUpper,
    Omicron,
    OmicronUpper,
    Pi,
    PiUpper,
    Rho,
    RhoUpper,
    Sigma,
    SigmaUpper,
    Tau,
    TauUpper,
    Upsilon,
    UpsilonUpper,
    Phi,
    PhiUpper,
    Chi,
    ChiUpper,
    Psi,
    PsiUpper,
    Omega,
    OmegaUpper,
    Quote,
}

impl MathSpecial {
    #[must_use]
    pub fn is_alphabetic(self) -> bool {
        self != Self::Quote
    }

    /// # Errors
    /// Returns an error on parsing error.
    ///
    /// # Panics
    /// Any panic in this function are a bug.
    pub fn parse(char_code: &str, content_span: Span) -> Result<Self, Error> {
        SPECIAL_MATH
            .iter()
            .enumerate()
            .find(|x| *x.1 == char_code)
            .map(|x| MathSpecial::from_usize(x.0).unwrap())
            .ok_or_else(|| {
                let best = most_similar(SPECIAL_MATH, char_code);

                Error::SpecialNotRecognised {
                    error_span: content_span,
                    code: char_code.to_string(),
                    suggested: best,
                }
            })
    }
}

impl Display for MathSpecial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", SPECIAL_MATH[*self as usize])
    }
}

/// A series of math characters.
#[derive(Debug, Clone, Serialize)]
pub struct MathString {
    pub chars: Vec<MathChar>,
    pub span: Span,
}

impl MathString {
    #[must_use]
    pub fn new(span: Span) -> Self {
        Self {
            chars: Vec::new(),
            span,
        }
    }

    /// # Panics
    /// Any panic here is a bug.
    #[must_use]
    pub fn displayed_by_default(&self) -> Option<Self> {
        let mut result = Vec::new();

        // The first set of characters must be either a single character or a special code.
        let mut letter = String::new();

        let mut chars = self.chars.iter().copied().peekable();

        while let Some(MathChar::Ascii(c)) = chars.peek().copied() {
            chars.next();
            letter.push(c);
        }

        if let Ok(special) = MathSpecial::parse(&letter, span!(0, 0, 0, 0)) {
            if special.is_alphabetic() {
                result.push(MathChar::Special(special));
            } else {
                return None;
            }
        } else if letter.len() == 1 {
            result.push(MathChar::Ascii(letter.chars().next().unwrap()));
        } else {
            return None;
        }

        while Some(MathChar::Prime) == chars.peek().copied() {
            chars.next();
            result.push(MathChar::Prime);
        }

        if chars.next() == Some(MathChar::SetIndex(MathIndex::Lower)) {
            result.push(MathChar::SetIndex(MathIndex::Lower));
            for c in chars.by_ref() {
                if c == MathChar::SetIndex(MathIndex::Normal) {
                    break;
                }

                result.push(c);
            }
            result.push(MathChar::SetIndex(MathIndex::Normal));
        }

        if chars.next().is_none() {
            Some(Self {
                chars: result,
                span: self.span,
            })
        } else {
            None
        }
    }

    /// # Errors
    /// Returns an error on parsing errors.
    pub fn parse(content: &str, content_span: Span) -> Result<Self, Error> {
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
                    math_string.push(MathChar::Special(MathSpecial::parse(
                        &special,
                        span!(
                            content_span.start.line,
                            content_span.start.column + special_start + 1,
                            content_span.start.line,
                            content_span.start.column + char_count
                        ),
                    )?));

                    special.clear();
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
                    return Err(Error::LabelIndexInsideIndex {
                        error_span: content_span,
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
            return Err(Error::UnclosedSpecial {
                error_span: span!(
                    content_span.start.line,
                    content_span.start.column + special_start,
                    content_span.start.line,
                    content_span.end.column
                ),
                parsed_special: special,
            });
        }

        Ok(Self {
            chars: math_string,
            span: content_span,
        })
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    #[must_use]
    pub fn get_span(&self) -> Span {
        self.span
    }
}

impl FromProperty for MathString {
    fn from_property(property: PropertyValue) -> Result<MathString, Error> {
        match property {
            PropertyValue::Number(n) => Err(Error::StringOrIdentExpected {
                error_span: n.get_span(),
            }),
            PropertyValue::Ident(i) => match i {
                Ident::Collection(mut c) => {
                    if c.len() == 1 {
                        Ok(MathString::from(c.collection.swap_remove(0)))
                    } else {
                        Err(Error::InvalidIdentMathString { error_span: c.span })
                    }
                }
                Ident::Named(n) => MathString::parse(&n.ident, n.span)?
                    .displayed_by_default()
                    .ok_or(Error::InvalidIdentMathString { error_span: n.span }),
            },
            PropertyValue::String(s) => MathString::parse(
                &s.content,
                span!(
                    s.span.start.line,
                    s.span.start.column + 1,
                    s.span.end.line,
                    s.span.end.column - 1
                ),
            ),
            PropertyValue::RawString(s) => Ok(MathString {
                chars: s.lit.content.chars().map(MathChar::Ascii).collect(),
                span: s.get_span(),
            }),
        }
    }
}

impl From<PointCollectionItem> for MathString {
    fn from(value: PointCollectionItem) -> Self {
        let mut math_string = vec![MathChar::Ascii(value.letter)];

        math_string.extend(
            value
                .index
                .iter()
                .flat_map(|v| v.chars().map(MathChar::Ascii)),
        );

        math_string.extend([MathChar::Prime].repeat(value.primes.into()));

        Self {
            chars: math_string,
            span: value.span,
        }
    }
}

impl Display for MathString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.chars {
            write!(f, "{c}")?;
        }

        Ok(())
    }
}

impl FromStr for MathString {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s, span!(0, 0, 0, 0))
    }
}
