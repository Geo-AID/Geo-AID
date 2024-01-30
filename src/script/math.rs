use std::collections::HashMap;

use crate::generator::AdjustableTemplate;

use super::{figure::Figure, unroll::{self, Displayed, Expr as Unrolled}, Error, HashableRc};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Number<M> {
    LineLineIntersection(LineExpr<M>, LineExpr<M>)
}

pub type NumberExpr<M> = Expr<Number<M>, M>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Line<M> {
    PointPoint(NumberExpr<M>, NumberExpr<M>)
}

pub type LineExpr<M> = Expr<Line<M>, M>;

#[derive(Debug, Clone)]
pub enum Any<M> {
    Number(Number<M>),
    Line(Line<M>)
}

pub type AnyExpr<M> = Expr<Any<M>, M>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expr<T, M> {
    pub kind: Box<T>,
    pub meta: M
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rule<M> {
    Eq(NumberExpr<M>, NumberExpr<M>)
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

#[derive(Debug)]
pub struct MathContext<M> {
    /// All mathed expressions are stored here.
    pub record: Vec<AnyExpr<M>>,
    /// Expressions are mapped to the record entries.
    pub expr_map: HashMap<usize, usize>
}

trait FromUnrolled {
    type Kind: Displayed;

    fn load(expr: &Unrolled<Self::Kind>, context: &mut MathContext<()>) -> Self;
}

impl MathContext<()> {
    pub fn load<T>(&mut self, expr: &Unrolled<T::Kind>) -> usize where T: FromUnrolled {
        let key = 
    }
}

pub fn load_script(input: &str, canvas_size: (usize, usize)) -> Result<Intermediate, Vec<Error>> {
    let (unrolled, nodes) = unroll::unroll(input)?;

    let mut context: MathContext<()> = MathContext {
        record: Vec::new(),
        expr_map: HashMap::new()
    };

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