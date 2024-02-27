use std::cell::OnceCell;
use std::collections::HashMap;
use std::mem;
use num_traits::Zero;

use crate::generator::AdjustableTemplate;
use crate::script::figure::Item;
use crate::script::token::number::{CompExponent, ProcNum};

use super::{figure::Figure, unroll::{self, Displayed, Expr as Unrolled, UnrolledRule, UnrolledRuleKind,
                                     Point as UnrolledPoint, Line as UnrolledLine, Circle as UnrolledCircle, ScalarData as UnrolledScalar}, Error, ComplexUnit, SimpleUnit};

trait Var {
    fn var(id: usize) -> Self;
}

trait HasMeta {
    type Meta;
}

impl<T: HasMeta> HasMeta for Vec<T> {
    type Meta = T::Meta;
}

impl<T: HasMeta> HasMeta for Box<T> {
    type Meta = T::Meta;
}

trait MapMeta<Dst>: HasMeta {
    type Output;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, f: F) -> Self::Output;
}

impl<Dst, T: MapMeta<Dst>> MapMeta<Dst> for Vec<T> {
    type Output = Vec<T::Output>;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, f: F) -> Self::Output {
        self.into_iter().map(|x| x.map_meta(f)).collect()
    }
}

impl<Dst, T: MapMeta<Dst>> MapMeta<Dst> for Box<T> {
    type Output = Box<T::Output>;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, f: F) -> Self::Output {
        Box::new((*self).map_meta(f))
    }
}

trait FromUnrolled<T: Displayed> {
    fn load(expr: &Unrolled<T>, math: &mut Expand) -> Self;
}

trait Normalize {
    fn normalize(&mut self);
}

pub trait LoadsTo {
    type Output;
}

impl LoadsTo for UnrolledPoint {
    type Output = PointExpr<()>;
}

impl LoadsTo for UnrolledLine {
    type Output = LineExpr<()>;
}

impl LoadsTo for UnrolledCircle {
    type Output = CircleExpr<()>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Point<M> {
    Var {
        id: usize
    },
    LineLineIntersection {
        k: LineExpr<M>,
        l: LineExpr<M>
    },
    Average {
        items: Vec<PointExpr<M>>
    },
    CircleCenter {
        circle: CircleExpr<M>
    },
    Entity {
        id: usize
    }
}

pub type PointExpr<M> = Expr<Point<M>, M>;

impl FromUnrolled<UnrolledPoint> for PointExpr<()> {
    fn load(expr: &Unrolled<UnrolledPoint>, math: &mut Expand) -> Self {
        let kind = match expr.get_data() {
            UnrolledPoint::LineLineIntersection(a, b) => Point::LineLineIntersection {
                k: math.load(a),
                l: math.load(b)
            },
            UnrolledPoint::Average(exprs) => Point::Average {
                items: exprs.iter().map(|x| math.load(x)).collect()
            },
            UnrolledPoint::CircleCenter(circle) => {
                match circle.get_data() {
                    UnrolledCircle::Circle(center, _) => return math.load(center),
                    _ => unreachable!()
                }
            },
            UnrolledPoint::Free => Point::Entity { id: math.add_point() },
            _ => unreachable!()
        };

        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

impl<M> Normalize for Point<M> {
    fn normalize(&mut self) {
        match self {
            Point::Entity { .. }
            | Point::Var { .. } => (),
            Point::LineLineIntersection { k, l } => {
                k.normalize();
                l.normalize();

                if k > l {
                    mem::swap(k, l);
                }
            }
            Point::Average { items } => {
                for item in &mut items {
                    item.normalize();
                }

                items.sort();
            }
            Point::CircleCenter { circle } => circle.normalize(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Number<M> {
    Var {
        id: usize
    },
    Average {
        items: Vec<NumberExpr<M>>
    },
    Entity {
        id: usize
    },
    Sum {
        plus: Vec<NumberExpr<M>>,
        minus: Vec<NumberExpr<M>>
    },
    Product {
        times: Vec<NumberExpr<M>>,
        by: Vec<NumberExpr<M>>
    },
    Const {
        value: ProcNum
    },
    Power {
        value: NumberExpr<M>,
        exponent: CompExponent
    },
    PointPointDistance {
        p: NumberExpr<M>,
        q: NumberExpr<M>
    },
    PointLineDistance {
        p: NumberExpr<M>,
        k: LineExpr<M>
    },
    ThreePointAngle {
        p: NumberExpr<M>,
        q: NumberExpr<M>,
        r: NumberExpr<M>
    },
    ThreePointAngleDir {
        p: NumberExpr<M>,
        q: NumberExpr<M>,
        r: NumberExpr<M>
    },
    TwoLineAngle {
        k: LineExpr<M>,
        l: LineExpr<M>
    },
    PointX {
        point: NumberExpr<M>
    },
    PointY {
        point: NumberExpr<M>
    }
}

pub type NumberExpr<M> = Expr<Number<M>, M>;

impl<M> Var for Number<M> {
    fn var(id: usize) -> Self {
        Self::Var { id }
    }
}

fn fix_dst(expr: NumberExpr<()>, unit: Option<ComplexUnit>, math: &mut Expand) -> NumberExpr<()> {
    match unit {
        None => expr,
        Some(unit) => {
            if unit.0[SimpleUnit::Distance as usize].is_zero() {
                expr
            } else {
                Expr::new(Number::Product {
                    times: vec![expr, Expr::new(Number::Power {
                        value: math.get_dst_var(),
                        exponent: unit.0[SimpleUnit::Distance as usize]
                    })],
                    by: Vec::new()
                })
            }
        }
    }
}

impl FromUnrolled<unroll::Scalar> for NumberExpr<()> {
    fn load(expr: &Unrolled<unroll::Scalar>, math: &mut Expand) -> Self {
        let mut kind = match expr.get_data() {
            UnrolledScalar::Add(a, b) => Number::Sum {
                plus: vec![math.load(a), math.load(b)],
                minus: Vec::new()
            },
            UnrolledScalar::Subtract(a, b) => Number::Sum {
                plus: vec![math.load(a)],
                minus: vec![math.load(b)]
            },
            UnrolledScalar::Multiply(a, b) => Number::Product {
                times: vec![math.load(a), math.load(b)],
                by: Vec::new()
            },
            UnrolledScalar::Divide(a, b) => Number::Product {
                times: vec![math.load(a)],
                by: vec![math.load(b)]
            },
            UnrolledScalar::Average(exprs) => Number::Average {
                items: exprs.iter().map(|x| math.load(x)).collect()
            },
            UnrolledScalar::CircleRadius(circle) => {
                match circle.get_data() {
                    UnrolledCircle::Circle(_, radius) => return math.load(radius),
                    _ => unreachable!()
                }
            }
            UnrolledScalar::Free => Number::Entity { id: math.add_real() },
            UnrolledScalar::Number(x) => return fix_dst(Expr::new(Number::Const { value: x.clone() }), expr.data.unit, math),
            UnrolledScalar::DstLiteral(x) => Number::Const { value: x.clone() },
            UnrolledScalar::SetUnit(x, unit) => return fix_dst(math.load(x), Some(*unit), math),
            UnrolledScalar::PointPointDistance(p, q) => Number::PointPointDistance {
                p: math.load(p),
                q: math.load(q)
            },
            UnrolledScalar::PointLineDistance(p, k) => Number::PointLineDistance {
                p: math.load(p),
                k: math.load(k)
            },
            UnrolledScalar::Negate(x) => Number::Sum {
                plus: Vec::new(),
                minus: vec![math.load(x)]
            },
            UnrolledScalar::ThreePointAngle(p, q, r) => Number::ThreePointAngle {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r)
            },
            UnrolledScalar::ThreePointAngleDir(p, q, r) => Number::ThreePointAngleDir {
                p: math.load(p),
                q: math.load(q),
                r: math.load(r)
            },
            UnrolledScalar::TwoLineAngle(k, l) => Number::TwoLineAngle {
                k: math.load(k),
                l: math.load(l)
            },
            UnrolledScalar::Pow(base, exponent) => Number::Power {
                value: math.load(base),
                exponent: exponent.clone()
            },
            UnrolledScalar::PointX(point) => Number::PointX {
                point: math.load(point)
            },
            UnrolledScalar::PointY(point) => Number::PointY {
                point: math.load(point)
            },
            _ => unreachable!()
        };

        match &mut kind {
            Number::Var { .. }
            | Number::CircleCenter { .. }
            | Number::Entity { .. } => (),
            Number::LineLineIntersection { k, l } => {
                if k > l {
                    mem::swap(k, l);
                }
            }
            Number::Average { items } => items.sort(),
            Number::Sum { .. } => {}
            Number::Product { .. } => {}
            Number::Const { .. } => {}
            Number::Power { .. } => {}
            Number::PointPointDistance { .. } => {}
            Number::PointLineDistance { .. } => {}
            Number::ThreePointAngle { .. } => {}
            Number::ThreePointAngleDir { .. } => {}
            Number::TwoLineAngle { .. } => {}
            Number::PointX { .. } => {}
            Number::PointY { .. } => {}
        }

        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

/// Beyond their specific conditions, all variants are
/// only normalized if their operands are normalized.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Line<M> {
    /// Always normalized
    Var {
        id: usize
    },
    /// Normalized iff `p` and `q` are in ascending order
    PointPoint {
        p: NumberExpr<M>,
        q: NumberExpr<M>
    },
    /// Normalized iff `a` and `c` are in ascending order (`b` must stay in the middle)
    AngleBisector {
        a: NumberExpr<M>,
        b: NumberExpr<M>,
        c: NumberExpr<M>
    },
    /// Always normalized
    ParallelThrough {
        point: NumberExpr<M>,
        line: LineExpr<M>
    },
    /// Always normalized
    PerpendicularThrough {
        point: NumberExpr<M>,
        line: LineExpr<M>
    }
}

pub type LineExpr<M> = Expr<Line<M>, M>;

impl<M> Var for Line<M> {
    fn var(id: usize) -> Self {
        Self::Var { id }
    }
}

impl FromUnrolled<UnrolledLine> for LineExpr<()> {
    fn load(expr: &Unrolled<UnrolledLine>, math: &mut Expand) -> Self {
        let mut kind = match expr.get_data() {
            UnrolledLine::LineFromPoints(a, b) => Line::PointPoint {
                p: math.load(a),
                q: math.load(b)
            },
            UnrolledLine::AngleBisector(a, b, c) => Line::AngleBisector {
                a: math.load(a),
                b: math.load(b),
                c: math.load(c),
            },
            UnrolledLine::PerpendicularThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => {
                        Line::ParallelThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    UnrolledLine::ParallelThrough(l, _) => {
                        Line::PerpendicularThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    _ => Line::PerpendicularThrough {
                        point: math.load(p),
                        line: math.load(k)
                    }
                }
            }
            UnrolledLine::ParallelThrough(k, p) => {
                // Remove unnecessary intermediates
                match k.get_data() {
                    UnrolledLine::PerpendicularThrough(l, _) => {
                        Line::PerpendicularThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    UnrolledLine::ParallelThrough(l, _) => {
                        Line::ParallelThrough {
                            point: math.load(p),
                            line: math.load(l)
                        }
                    },
                    _ => Line::ParallelThrough {
                        point: math.load(p),
                        line: math.load(k)
                    }
                }
            },
            _ => unreachable!()
        };

        // Normalize
        match &mut kind {
            // Normalized at the point of construction
            Line::ParallelThrough { .. }
            | Line::PerpendicularThrough { .. }
            | Line::Var { .. } => (),
            // Reorder if necessary
            Line::PointPoint { p, q }
            | Line::AngleBisector { a: p, b: _, c: q } => {
                if p > q {
                    mem::swap(p, q);
                }
            }
        }

        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Circle<M> {
    Var {
        id: usize
    },
    Construct {
        center: NumberExpr<M>,
        radius: NumberExpr<M>
    }
}

pub type CircleExpr<M> = Expr<Circle<M>, M>;

#[derive(Debug, Clone)]
pub enum Any<M> {
    Point(Point<M>),
    Number(Number<M>),
    Line(Line<M>)
}

pub type AnyExpr<M> = Expr<Any<M>, M>;

impl<M> From<Number<M>> for Any<M> {
    fn from(value: Number<M>) -> Self {
        Self::Number(value)
    }
}

impl<M> From<Line<M>> for Any<M> {
    fn from(value: Line<M>) -> Self {
        Self::Line(value)
    }
}

impl<M> From<Point<M>> for Any<M> {
    fn from(value: Point<M>) -> Self {
        Self::Point(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr<T, M> {
    pub kind: Box<T>,
    pub meta: M
}

impl<T: Ord, M: PartialEq> PartialOrd for Expr<T, M> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord, M: PartialEq> Ord for Expr<T, M> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.kind.cmp(&other.kind)
    }
}

impl<T: Var> Var for Expr<T, ()> {
    fn var(id: usize) -> Self {
        Self {
            kind: Box::new(T::var(id)),
            meta: ()
        }
    }
}

impl<T: Normalize, M> Normalize for Expr<T, M> {
    fn normalize(&mut self) {
        self.kind.normalize();
    }
}

impl<T, M> HasMeta for Expr<T, M> {
    type Meta = M;
}

impl<M, Dst, T: MapMeta<Dst> + HasMeta<Meta = M>> MapMeta<Dst> for Expr<T, M> {
    type Output = Expr<T::Output, Dst>;

    fn map_meta<F: FnMut(Self::Meta) -> Dst>(self, mut f: F) -> Self::Output {
        Expr {
            kind: self.kind.map_meta(f),
            meta: f(self.meta)
        }
    }
}

impl<T> Expr<T, ()> {
    #[must_use]
    pub const fn new(kind: T) -> Self {
        Self {
            kind: Box::new(kind),
            meta: ()
        }
    }
}

impl<T, M> Expr<T, M> where Any<M>: From<T> {
    #[must_use]
    pub fn into_any(self) -> AnyExpr<M> {
        AnyExpr {
            kind: Box::new((*self.kind).into()),
            meta: self.meta
        }
    }
}

/// Represents a rule of the figure.
/// Rules are normalized iff:
/// * their operands are normalized
/// * their operands are sorted in ascending order
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rule<M> {
    Eq(NumberExpr<M>, NumberExpr<M>),
    Lt(NumberExpr<M>, NumberExpr<M>),
    Gt(NumberExpr<M>, NumberExpr<M>),
    Alternative(Vec<Rule<M>>),
    Invert(Box<Rule<M>>),
    Bias
}

impl Rule<()> {
    /// # Returns
    /// A normalized rule.
    fn load(rule: &UnrolledRule, math: &mut Expand) -> Self {
        let mut mathed = match &rule.kind {
            UnrolledRuleKind::PointEq(a, b) => Self::Eq(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::ScalarEq(a, b) => Self::Eq(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::Gt(a, b) => Self::Gt(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::Lt(a, b) => Self::Lt(
                math.load(a),
                math.load(b)
            ),
            UnrolledRuleKind::Alternative(rules) => Self::Alternative(
                rules.iter().map(|x| Self::load(x, math)).collect()
            ),
            UnrolledRuleKind::Bias(_) => Self::Bias
        };

        // Normalize
        match &mut mathed {
            | Self::Eq(a, b)
            | Self::Gt(a, b)
            | Self::Lt(a, b) => {
                if a > b {
                    mem::swap(a, b);
                }
            }
            Self::Alternative(v) => v.sort(),
            Self::Bias => (),
            Self::Invert(_) => unreachable!()
        }

        if rule.inverted {
            Self::Invert(Box::new(mathed))
        } else {
            mathed
        }
    }
}

#[derive(Debug)]
pub struct Adjusted {
    pub template: Vec<AdjustableTemplate>,
    pub items: Vec<AnyExpr<()>>,
    pub rules: Vec<Rule<()>>
}

#[derive(Debug)]
pub struct Intermediate {
    pub figure: Figure,
    /// Ready for generation
    pub adjusted: Adjusted
}

#[derive(Debug, Clone)]
pub struct Entry {
    pub expr: AnyExpr<()>,
    pub uses: usize
}

#[derive(Debug)]
pub enum Entity {
    FreePoint,
    FreeReal
}

#[derive(Debug, Clone, Default)]
pub struct Expand {
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, AnyExpr<()>>,
    /// All found entities
    pub entities: Vec<Entity>,
    /// Dst variable
    pub dst_var: OnceCell<NumberExpr<()>>
}

impl Expand {
    pub fn load<T: Displayed, U>(&mut self, expr: &Unrolled<T>) -> Expr<U, ()>
        where Any<()>: From<U>, Expr<U, ()>: FromUnrolled<T> {
        let key = (expr.data.as_ref() as *const _) as usize;
        let loaded = self.expr_map.get_mut(&key).cloned();

        if let Some(loaded) = loaded {
            loaded.try_into().unwrap()
        } else {
            // If expression has not been mathed yet, math it and put it into the record.
            let loaded = Expr::load(expr, self);
            self.expr_map.insert( key, loaded.clone().into_any());
            loaded
        }
    }

    #[must_use]
    pub fn get_dst_var(&mut self) -> NumberExpr<()> {
        self.dst_var.get_or_init(|| Expr::new(Number::Entity { id: self.add_real() })).clone()
    }

    fn add_entity(&mut self, entity: Entity) -> usize {
        self.entities.push(entity);
        self.entities.len() - 1
    }

    pub fn add_point(&mut self) -> usize {
        self.add_entity(Entity::FreePoint)
    }

    pub fn add_real(&mut self) -> usize {
        self.add_entity(Entity::FreeReal)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Build {
    expand: Expand,
    id_map: HashMap<usize, usize>,
    loaded: Vec<AnyExpr<()>>,
    items: Vec<Item>
}

impl Build {
    pub fn load<T: Displayed + LoadsTo>(&mut self, expr: &Unrolled<T>) -> usize {
        let key = (expr.data.as_ref() as *const _) as usize;
        *self.id_map.entry(key).or_insert_with(|| {
            self.loaded.push(self.expand.load(expr));
            self.loaded.len() - 1
        })
    }

    pub fn add<I: Into<Item>>(&mut self, item: I) {
        self.items.push(item.into());
    }
}

pub fn load_script(input: &str) -> Result<(Adjusted, Figure), Vec<Error>> {
    // Unroll script
    // Expand rules & figure maximally (normalize at the point of expansion)
    // ---
    // Optimize rules and entities
    // Reduce entities
    // --- repeat
    // Turn entities into adjustables
    // Ultimately reduce entities
    // Split rules & figure
    // Fold rules & figure separately
    // Assign reserved registers to figure expressions
    // Return

    // Unroll script
    let (mut unrolled, nodes) = unroll::unroll(input)?;

    // Expand figure
    let mut build = Build::default();
    nodes.build(&mut build);

    // Move expand base
    let mut expand = build.expand;

    // Expand rules
    let mut rules = Vec::new();

    for rule in unrolled.take_rules() {
        rules.push(Rule::load(&rule, &mut expand));
    }

    // Fold & normalize rules


    Ok((
        todo!()
    ))
}