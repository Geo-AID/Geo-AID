use std::collections::HashMap;

use crate::generator::AdjustableTemplate;

use super::{
    figure::Figure,
    unroll::{self, Displayed, Expr as Unrolled, context::CompileContext, UnrolledRule, UnrolledRuleKind, Point as UnrolledPoint, Line as UnrolledLine, Circle as UnrolledCircle, Generic},
    Error
};

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
    fn load(expr: &Unrolled<T>, math: &mut Expand<()>) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Number<M> {
    Var {
        id: usize
    },
    LineLineIntersection {
        k: LineExpr<M>,
        l: LineExpr<M>
    },
    Average {
        items: NumberExpr<M>
    },
    CircleCenter {
        circle: CircleExpr<M>
    }
}

pub type NumberExpr<M> = Expr<Number<M>, M>;

impl<M> Var for Number<M> {
    fn var(id: usize) -> Self {
        Self::Var(id)
    }
}

impl FromUnrolled<UnrolledPoint> for NumberExpr<()> {
    fn load(expr: &Unrolled<UnrolledPoint>, math: &mut Expand<()>) -> Self {
        let kind = match expr.data.as_ref() {
            UnrolledPoint::LineLineIntersection(a, b) => Number::LineLineIntersection(
                math.load(a),
                math.load(b)
            ),
            UnrolledPoint::Average(exprs) => Number::Average(
                exprs.iter().map(|x| math.load(x)).collect()
            ),
            UnrolledPoint::CircleCenter(circle) => {
                match circle.data.as_ref() {
                    UnrolledCircle::Generic(generic) => match generic {
                        Generic::Boxed(v) => return math.load(v),
                        Generic::VariableAccess(v) => return math.load(&v.borrow().definition)
                    },
                    UnrolledCircle::Circle(center, _) => return math.load(center),
                }
            }
        };

        Self {
            kind,
            meta: ()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Line<M> {
    Var {
        id: usize
    },
    PointPoint {
        p: NumberExpr<M>,
        q: NumberExpr<M>
    }
}

pub type LineExpr<M> = Expr<Line<M>, M>;

impl<M> Var for Line<M> {
    fn var(id: usize) -> Self {
        Self::Var(id)
    }
}

impl FromUnrolled<UnrolledLine> for Line<()> {
    fn load(expr: &Unrolled<UnrolledLine>, math: &mut Expand<()>) -> Self {
        match expr.data.as_ref() {
            UnrolledLine::LineFromPoints(a, b) => Self::PointPoint(
                math.load(a),
                math.load(b)
            ),
            _ => todo!()
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expr<T, M> {
    pub kind: Box<T>,
    pub meta: M
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rule<M> {
    Eq(NumberExpr<M>, NumberExpr<M>),
    Invert(Box<Rule<M>>)
}

impl Rule<()> {
    fn load(rule: &UnrolledRule, math: &mut Expand<()>) -> Self {
        let mathed = match &rule.kind {
            UnrolledRuleKind::PointEq(a, b) => Rule::Eq(
                math.load(a),
                math.load(b)
            ),
            _ => todo!()
        };

        if rule.inverted {
            Self::Invert(Box::new(mathed))
        } else {
            mathed
        }
    }
}

/// A binding to an entity (templated).
pub type EntityBinding = Option<usize>;

#[derive(Debug)]
pub struct Adjusted {
    pub template: Vec<AdjustableTemplate>,
    pub items: Vec<AnyExpr<EntityBinding>>,
    pub rules: Vec<Rule<EntityBinding>>
}

#[derive(Debug)]
pub struct Intermediate {
    pub figure: Figure,
    /// Ready for generation
    pub adjusted: Adjusted
}

#[derive(Debug, Clone)]
pub struct Entry<M> {
    pub expr: AnyExpr<M>,
    pub uses: usize
}

#[derive(Debug)]
pub struct Expand<M> {
    /// All mathed expressions are stored here.
    pub record: Vec<Entry<M>>,
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, usize>
}

impl Expand<()> {
    pub fn load<T: Displayed, U: Var + FromUnrolled<T>>(&mut self, expr: &Unrolled<T>) -> Expr<U, ()> where Any<()>: From<U> {
        let key = (expr.data.as_ref() as *const _) as usize;
        let l = self.expr_map.len();
        let id = self.expr_map.get_mut(&key).copied();

        let id = if let Some(id) = id {
            self.record[id].uses += 1;
            id
        } else {
            // If expression has not been mathed yet, math it and put it into the record.
            self.record.push(Entry {
                expr: Expr::new(Any::from(U::load(expr, self))),
                uses: 1
            });

            let id = self.record.len() - 1;
            self.expr_map.insert(key, id);

            id
        };

        Expr::new(U::var(id))
    }
}

fn load_adjusted(mut unrolled: CompileContext) -> Adjusted {
    // First, all expressions are expanded: mapped by Rc addresses and split into atoms.
    let mut expansion: Expand<()> = Expand {
        record: Vec::new(),
        expr_map: HashMap::new()
    };

    let mut rules = Vec::new();

    for rule in unrolled.take_rules() {
        rules.push(Rule::load(&rule, &mut expansion));
    }

    // Give entity indices.
    // let exprs = expansion.record.map_meta(|_| {
        
    // });

    Adjusted {
        template: Vec::new(),
        items: Vec::new(),
        rules: Vec::new()
    }
}

pub fn load_script(input: &str, canvas_size: (usize, usize)) -> Result<Intermediate, Vec<Error>> {
    let (unrolled, nodes) = unroll::unroll(input)?;

    let adjusted = load_adjusted(unrolled);

    Ok(Intermediate {
        figure: Figure {
            items: Vec::new(),
            canvas_size
        },
        adjusted: Adjusted {
            template: Vec::new(),
            items: Vec::new(),
            rules: Vec::new()
        },
    })
}