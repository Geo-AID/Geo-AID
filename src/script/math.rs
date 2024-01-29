#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Point<M> {
    LineLineIntersection(LineExpr<M>, LineExpr<M>)
}

pub type PointExpr<M> = Expr<Point<M>, M>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Line<M> {
    PointPoint(PointExpr<M>, PointExpr<M>)
}

pub type LineExpr<M> = Expr<Line<M>, M>;

#[derive(Debug, Clone)]
pub enum Any<M> {
    Point(Point<M>),
    Line(Line<M>)
}

pub type AnyExpr<M> = Expr<Any<M>, M>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Expr<T, M> {
    pub kind: Box<T>,
    pub meta: M
}