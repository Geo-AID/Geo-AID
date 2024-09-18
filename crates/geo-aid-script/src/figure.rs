//! Geo-AID's figure Intermediate Representation and all definitions related to it.
//! Note that a part of it is also located in `geo-aid-figure`

use std::{fmt::Display, str::FromStr};

use crate::geometry::ValueEnum;
use crate::math::{EntityKind, IndexMap, Reconstruct, ReconstructCtx, Reindex};
use geo_aid_figure::math_string::{
    MathChar, MathIndex, MathSpecial, MathString, ParseErrorKind, SPECIAL_MATH,
};
use geo_aid_figure::{Style, VarIndex};

use crate::span;

use super::math::Entity;
use super::{
    math,
    parser::{FromProperty, Parse, PropertyValue},
    token::{Ident, PointCollectionItem, Span},
    unroll::most_similar,
    Error,
};

/// A drawn point
#[derive(Debug, Clone)]
pub struct PointItem {
    /// Index of the defining expression
    pub id: VarIndex,
    /// Label of this point
    pub label: MathString,
    /// Whether to display a small circle in its place
    pub display_dot: bool,
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

/// A drawn circle
#[derive(Debug, Clone)]
pub struct CircleItem {
    /// Index of the defining expression
    pub id: VarIndex,
    /// Label of this circle
    pub label: MathString,
    /// How to draw the circle (brush)
    pub style: Style,
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

/// A drawn line
#[derive(Debug, Clone)]
pub struct LineItem {
    /// Index of the defining expression
    pub id: VarIndex,
    /// Label of this line
    pub label: MathString,
    /// How to draw the line (brush)
    pub style: Style,
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

/// A drawn ray (half-line)
#[derive(Debug, Clone)]
pub struct RayItem {
    /// Index of the expression defining the ray's origin (end point).
    pub p_id: VarIndex,
    /// Index of the expression defining the ray's guiding point
    pub q_id: VarIndex,
    /// The ray's label
    pub label: MathString,
    /// How to draw the ray (brush)
    pub style: Style,
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

/// A drawn segment
#[derive(Debug, Clone)]
pub struct SegmentItem {
    /// Index of the expression defining the first endpoint
    pub p_id: VarIndex,
    /// Index of the expression defining the second endpoint
    pub q_id: VarIndex,
    /// The segment's label
    pub label: MathString,
    /// How to draw the segment (brush)
    pub style: Style,
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

/// A type-erased drawn item of the figure
#[derive(Debug, Clone)]
pub enum Item {
    Point(PointItem),
    Circle(CircleItem),
    Line(LineItem),
    Ray(RayItem),
    Segment(SegmentItem),
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
            Self::Segment(v) => Self::Segment(v.reconstruct(ctx)),
        }
    }
}

/// Defines the visual data of the figure.
#[derive(Debug, Default, Clone)]
pub struct Figure {
    /// Entities used by the figure
    pub entities: Vec<EntityKind>,
    /// Variables used by the figure
    pub variables: Vec<math::Expr<()>>,
    /// Drawn items
    pub items: Vec<Item>,
}

/// Generated figure, created by the engine
#[derive(Debug, Clone, Default)]
pub struct Generated {
    /// Entities used by the figure
    pub entities: Vec<Entity<ValueEnum>>,
    /// Variables used by the figure
    pub variables: Vec<math::Expr<ValueEnum>>,
    /// Drawn items with meta
    pub items: Vec<Item>,
}

/// A [`MathString`] with a [`Span`].
#[derive(Debug, Clone)]
pub struct SpannedMathString {
    pub string: MathString,
    pub span: Span,
}

impl SpannedMathString {
    /// Create an empty math string with a span.
    #[must_use]
    pub fn new(span: Span) -> Self {
        Self {
            string: MathString::new(),
            span,
        }
    }

    /// Return `Some` with `self` if the string should be displayed by default.
    /// A string should be displayed by default if it consists of one alphabetical
    /// (either special or literal) and is possibly followed by primes (') and an index
    /// of any length.
    ///
    /// # Panics
    /// Any panic here is a bug.
    #[must_use]
    pub fn displayed_by_default(&self) -> Option<Self> {
        let mut result = MathString::new();

        // The first set of characters must be either a single character or a special code.
        let mut letter = String::new();

        let mut chars = self.string.iter().copied().peekable();

        while let Some(MathChar::Ascii(c)) = chars.peek().copied() {
            chars.next();
            letter.push(c);
        }

        if let Some(special) = MathSpecial::parse(&letter) {
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
                string: result,
                span: self.span,
            })
        } else {
            None
        }
    }

    /// Try to parse the given string as a math string.
    ///
    /// # Errors
    /// Returns an error on parsing errors.
    pub fn parse(content: &str, content_span: Span) -> Result<Self, Error> {
        let string = MathString::from_str(content).map_err(|err| {
            let error_span = span!(
                content_span.start.line,
                content_span.start.column + err.span.start,
                content_span.end.line,
                content_span.end.column + err.span.end
            );

            match err.kind {
                ParseErrorKind::SpecialNotRecognised(special) => {
                    let suggested = most_similar(&SPECIAL_MATH, &special);

                    Error::SpecialNotRecognised {
                        error_span,
                        code: special,
                        suggested,
                    }
                }
                ParseErrorKind::NestedIndex => Error::LabelIndexInsideIndex { error_span },
                ParseErrorKind::UnclosedSpecialTag(special) => Error::UnclosedSpecial {
                    error_span,
                    parsed_special: special,
                },
            }
        })?;

        Ok(Self {
            string,
            span: content_span,
        })
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.string.is_empty()
    }

    #[must_use]
    pub fn get_span(&self) -> Span {
        self.span
    }
}

impl FromProperty for SpannedMathString {
    fn from_property(property: PropertyValue) -> Result<Self, Error> {
        match property {
            PropertyValue::Number(n) => Err(Error::StringOrIdentExpected {
                error_span: n.get_span(),
            }),
            PropertyValue::Ident(i) => match i {
                Ident::Collection(mut c) => {
                    if c.len() == 1 {
                        Ok(Self::from(c.collection.swap_remove(0)))
                    } else {
                        Err(Error::InvalidIdentMathString { error_span: c.span })
                    }
                }
                Ident::Named(n) => Self::parse(&n.ident, n.span)?
                    .displayed_by_default()
                    .ok_or(Error::InvalidIdentMathString { error_span: n.span }),
            },
            PropertyValue::String(s) => Self::parse(
                &s.content,
                span!(
                    s.span.start.line,
                    s.span.start.column + 1,
                    s.span.end.line,
                    s.span.end.column - 1
                ),
            ),
            PropertyValue::RawString(s) => Ok(Self {
                string: MathString::raw(&s.lit.content),
                span: s.get_span(),
            }),
        }
    }
}

impl From<PointCollectionItem> for SpannedMathString {
    fn from(value: PointCollectionItem) -> Self {
        let mut string = MathString::new();
        string.push(MathChar::Ascii(value.letter));

        if let Some(index) = value.index {
            string.extend(index.chars().map(MathChar::Ascii));
        }

        string.extend([MathChar::Prime].repeat(value.primes.into()));

        Self {
            string,
            span: value.span,
        }
    }
}

impl Display for SpannedMathString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.string)
    }
}
