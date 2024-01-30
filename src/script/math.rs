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