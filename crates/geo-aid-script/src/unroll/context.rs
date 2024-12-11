//! General functionality contained in the unroll context.

use num_rational::Ratio;
use num_traits::{One, Zero};
use paste::paste;
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use std::{collections::HashMap, fmt::Debug};

use super::library::macros::number;
use crate::span;
use crate::token::number::ProcNum;
use crate::unroll::{AnyExpr, CloneWithNode};
use crate::{unit, ComplexUnit, Error};

use super::figure::FromExpr;
use super::flags::FlagSet;
use super::{
    Circle, CollectionNode, Displayed, Expr, HierarchyNode, Line, Node, Number, NumberData, Point,
    Properties, UnrolledRule, UnrolledRuleKind,
};

/// The context of unroll process.
#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct CompileContext {
    /// Variables. `AnyExpr` is expected to be the variable variant.
    pub variables: HashMap<String, AnyExpr>, // We have to store variables in this form to prevent type errors.
    /// Flags
    pub flags: FlagSet,
    /// Unrolled rules
    pub rules: RefCell<Vec<UnrolledRule>>,
    /// Errors collected.
    errors: RefCell<Vec<Error>>,
}

impl Default for CompileContext {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileContext {
    /// Create a new context.
    #[must_use]
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            flags: FlagSet::default(),
            rules: RefCell::new(Vec::new()),
            errors: RefCell::new(Vec::new()),
        }
    }

    /// Push an error.
    pub fn push_error(&self, err: Error) {
        self.errors.borrow_mut().push(err);
    }

    /// Turn a `Result` into an `Option` while pushing the error if present.
    pub fn ok<T>(&self, res: Result<T, Error>) -> Option<T> {
        match res {
            Ok(v) => Some(v),
            Err(err) => {
                self.push_error(err);
                None
            }
        }
    }

    /// Take the context's errors.
    pub fn take_errors(&mut self) -> Vec<Error> {
        mem::take(&mut self.errors.borrow_mut())
    }

    /// Extend the context's errors
    pub fn extend_errors<I: IntoIterator<Item = Error>>(&self, iter: I) {
        self.errors.borrow_mut().extend(iter);
    }

    /// Whether the unroll step finished with no errors.
    pub fn valid(&self) -> bool {
        self.errors.borrow().is_empty()
    }

    /// Push a rule.
    pub fn push_rule(&self, rule: UnrolledRule) {
        self.rules.borrow_mut().push(rule);
    }

    /// Take the context's rules.
    pub fn take_rules(&mut self) -> Vec<UnrolledRule> {
        mem::take(&mut self.rules.borrow_mut())
    }
}

/// Everything related to circles.
impl CompileContext {
    /// A point lies on circle rule.
    pub fn point_on_circle(&mut self, lhs: &Expr<Point>, rhs: &Expr<Circle>, weight: ProcNum) {
        self.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::NumberEq(
                self.distance_pp(
                    lhs.clone_without_node(),
                    self.circle_center(rhs.clone_without_node()),
                ),
                self.circle_radius(rhs.clone_without_node()),
            ),
            inverted: false,
            weight,
        });
    }

    /// A point lies on line rule.
    pub fn point_on_line(&mut self, lhs: &Expr<Point>, rhs: &Expr<Line>, weight: ProcNum) {
        self.push_rule(UnrolledRule {
            kind: UnrolledRuleKind::NumberEq(
                self.distance_pl(lhs.clone_without_node(), rhs.clone_without_node()),
                number!(=ProcNum::zero()),
            ),
            inverted: false,
            weight,
        });
    }
}

/// Helper macro for taking nodes out of multiple expressions.
#[macro_export]
macro_rules! take_nodes {
    () => {
        Vec::new()
    };
    ($($x:ident),* $(,)?) => {
        {
            let mut nodes = Vec::new();
            take_nodes!{nodes << $($x),*}
            nodes
        }
    };
    ($nodes:ident << $v:ident, $($x:ident),*) => {
        $nodes.extend($v.take_node().map(|node| Box::new(node) as Box<dyn $crate::unroll::Node>));
        take_nodes!{$nodes << $($x),*}
    };
    ($nodes:ident << $v:ident) => {
        $nodes.extend($v.take_node().map(|node| Box::new(node) as Box<dyn $crate::unroll::Node>));
    }
}

/// Helper macro for a generic expression function.
macro_rules! generic_expr {
    {$f:ident($v0:ident : $t0:ident $(, $v:ident : $t:ident)* $(,)?) -> Number[inferred] :: $k:ident} => {
        paste! {
            pub fn [<$f _display>](&self, mut $v0: Expr<super::$t0> $(, mut $v: Expr<super::$t>)*, display: Properties) -> Expr<super::Number> {
                let nodes = take_nodes!($v0, $($v),*);
                self.expr_with(super::Number {
                    unit: $v0.data.unit,
                    data: super::NumberData::$k($v0, $($v),*)
                }, display, nodes)
            }

            pub fn $f(&self, $v0: Expr<super::$t0>, $($v: Expr<super::$t>),*) -> Expr<super::Number> {
                self.[<$f _display>]($v0 $(, $v)*, Properties::default())
            }
        }
    };
    {$f:ident($($v:ident : $t:ident),* $(,)?) -> Number[$unit:expr] :: $k:ident} => {
        paste! {
            pub fn [<$f _display>](&self, $(mut $v: Expr<super::$t>),*, display: Properties) -> Expr<super::Number> {
                let nodes = take_nodes!($($v),*);
                self.expr_with(super::Number {
                    unit: Some($unit),
                    data: super::NumberData::$k($($v),*)
                }, display, nodes)
            }

            pub fn $f(&self, $($v: Expr<super::$t>),*) -> Expr<super::Number> {
                self.[<$f _display>]($($v,)* Properties::default())
            }
        }
    };
    {$f:ident($($v:ident : $t:ident),* $(,)?) -> $r:ident :: $k:ident} => {
        paste! {
            pub fn [<$f _display>](&self, $(mut $v: Expr<super::$t>,)* display: Properties) -> Expr<super::$r> {
                let nodes = take_nodes!($($v),*);
                self.expr_with(super::$r::$k($($v),*), display, nodes)
            }

            pub fn $f(&self, $($v: Expr<super::$t>),*) -> Expr<super::$r> {
                self.[<$f _display>]($($v,)* Properties::default())
            }
        }
    }
}

// Expression constructors
impl CompileContext {
    /// Create an expression with properties and nodes.
    pub fn expr_with<T: Displayed>(
        &self,
        content: T,
        display: Properties,
        nodes: Vec<Box<dyn Node>>,
    ) -> Expr<T>
    where
        T::Node: FromExpr<T>,
    {
        let mut expr = Expr {
            span: span!(0, 0, 0, 0),
            data: Rc::new(content),
            node: None,
        };

        let mut node = HierarchyNode::new(T::Node::from_expr(&expr, display, self));

        node.extend_boxed(nodes);

        expr.node = Some(node);
        expr
    }

    /// An average pointe expression with display options.
    pub fn average_p_display(
        &self,
        mut points: Vec<Expr<Point>>,
        display: Properties,
    ) -> Expr<Point> {
        let nodes = points
            .iter_mut()
            .filter_map(|v| v.take_node().map(|v| Box::new(v) as Box<dyn Node>))
            .collect();

        self.expr_with(Point::Average(points.into()), display, nodes)
    }

    /// Average point expression with no properties.
    pub fn average_p(&self, points: Vec<Expr<Point>>) -> Expr<Point> {
        self.average_p_display(points, Properties::from(None))
    }

    /// Average scalar expression with display options.
    pub fn average_s_display(
        &self,
        mut values: Vec<Expr<Number>>,
        display: Properties,
    ) -> Expr<Number> {
        let nodes = values
            .iter_mut()
            .filter_map(|v| v.take_node().map(|v| Box::new(v) as Box<dyn Node>))
            .collect();

        self.expr_with(
            Number {
                unit: values[0].data.unit,
                data: NumberData::Average(values.into()),
            },
            display,
            nodes,
        )
    }

    /// Average scalar expression with no properties.
    pub fn average_s(&self, points: Vec<Expr<Number>>) -> Expr<Number> {
        self.average_s_display(points, Properties::from(None))
    }

    /// Set unit expression with properties.
    pub fn set_unit_display(
        &self,
        mut v: Expr<Number>,
        unit: ComplexUnit,
        display: Properties,
    ) -> Expr<Number> {
        let node = v.take_node();
        self.expr_with(
            Number {
                unit: Some(unit),
                data: NumberData::SetUnit(v, unit),
            },
            display,
            node.into_iter()
                .map(|x| Box::new(x) as Box<dyn Node>)
                .collect(),
        )
    }

    /// Set unit expression with no properties.
    pub fn set_unit(&self, v: Expr<Number>, unit: ComplexUnit) -> Expr<Number> {
        self.set_unit_display(v, unit, Properties::default())
    }

    /// Free point expression with properties.
    pub fn free_point_display(&self, display: Properties) -> Expr<Point> {
        self.expr_with(Point::Free, display, Vec::new())
    }

    /// Free point expression with no properties.
    pub fn free_point(&self) -> Expr<Point> {
        self.free_point_display(Properties::default())
    }

    /// Free scalar expression with properties.
    pub fn free_scalar_display(&self, display: Properties) -> Expr<Number> {
        self.expr_with(
            Number {
                unit: Some(unit::SCALAR),
                data: NumberData::Free,
            },
            display,
            Vec::new(),
        )
    }

    /// Free scalar expression with no properties.
    pub fn free_scalar(&self) -> Expr<Number> {
        self.free_scalar_display(Properties::default())
    }

    generic_expr! {intersection(k: Line, l: Line) -> Point::LineLineIntersection}
    generic_expr! {distance_pp(p: Point, q: Point) -> Number[unit::DISTANCE]::PointPointDistance}
    generic_expr! {distance_pl(p: Point, k: Line) -> Number[unit::DISTANCE]::PointLineDistance}
    generic_expr! {circle_center(c: Circle) -> Point::CircleCenter}
    generic_expr! {circle_radius(c: Circle) -> Number[unit::DISTANCE]::CircleRadius}
    generic_expr! {line(a: Point, b: Point) -> Line::LineFromPoints}
    generic_expr! {angle_ppp(a: Point, b: Point, c: Point) -> Number[unit::ANGLE]::ThreePointAngle}
    generic_expr! {angle_dir(a: Point, b: Point, c: Point) -> Number[unit::ANGLE]::ThreePointAngleDir}
    generic_expr! {angle_ll(k: Line, l: Line) -> Number[unit::ANGLE]::TwoLineAngle}
    generic_expr! {bisector_ppp(a: Point, b: Point, c: Point) -> Line::AngleBisector}
    generic_expr! {perpendicular_through(line: Line, point: Point) -> Line::PerpendicularThrough}
    generic_expr! {parallel_through(line: Line, point: Point) -> Line::ParallelThrough}
    generic_expr! {circle(center: Point, radius: Number) -> Circle::Circle}
    generic_expr! {add(a: Number, b: Number) -> Number[inferred]::Add}
    generic_expr! {sub(a: Number, b: Number) -> Number[inferred]::Subtract}
    generic_expr! {real(v: Number) -> Number[inferred]::Real}
    generic_expr! {imaginary(v: Number) -> Number[inferred]::Imaginary}
    generic_expr! {to_complex(p: Point) -> Number[unit::DISTANCE]::FromPoint}
    generic_expr! {to_point(v: Number) -> Point::FromComplex}
    generic_expr! {sin(v: Number) -> Number[unit::SCALAR]::Sin}
    generic_expr! {cos(v: Number) -> Number[unit::SCALAR]::Cos}
    generic_expr! {log(v: Number) -> Number[unit::SCALAR]::Log}
    generic_expr! {direction(k: Line) -> Number[unit::SCALAR]::Direction}
    generic_expr! {neg(v: Number) -> Number[inferred]::Negate}
    generic_expr! {point_vector(p: Point, v: Number) -> Line::PointVector}
    generic_expr! {point_x(p: Point) -> Number[unit::DISTANCE]::PointX}
    generic_expr! {point_y(p: Point) -> Number[unit::DISTANCE]::PointY}

    pub fn mult_display(
        &self,
        mut a: Expr<Number>,
        mut b: Expr<Number>,
        display: Properties,
    ) -> Expr<Number> {
        let nodes = take_nodes!(a, b);
        self.expr_with(
            Number {
                unit: a.data.unit.zip(b.data.unit).map(|(a, b)| a * &b),
                data: NumberData::Multiply(a, b),
            },
            display,
            nodes,
        )
    }

    pub fn mult(&self, a: Expr<Number>, b: Expr<Number>) -> Expr<Number> {
        self.mult_display(a, b, Properties::default())
    }

    pub fn pow_display(
        &self,
        mut v: Expr<Number>,
        exponent: Ratio<i64>,
        display: Properties,
    ) -> Expr<Number> {
        let nodes = take_nodes!(v);
        self.expr_with(
            Number {
                unit: v.data.unit.map(|u| u.pow(exponent)),
                data: NumberData::Pow(v, exponent),
            },
            display,
            nodes,
        )
    }

    pub fn pow(&self, v: Expr<Number>, exponent: Ratio<i64>) -> Expr<Number> {
        self.pow_display(v, exponent, Properties::default())
    }

    pub fn div_display(
        &self,
        mut a: Expr<Number>,
        mut b: Expr<Number>,
        display: Properties,
    ) -> Expr<Number> {
        let nodes = take_nodes!(a, b);
        self.expr_with(
            Number {
                unit: a.data.unit.zip(b.data.unit).map(|(a, b)| a / &b),
                data: NumberData::Divide(a, b),
            },
            display,
            nodes,
        )
    }

    pub fn div(&self, a: Expr<Number>, b: Expr<Number>) -> Expr<Number> {
        self.div_display(a, b, Properties::default())
    }
}

/// Helper macro for general rule functions.
macro_rules! generic_rule {
    ($f:ident($lhs:ident, $rhs:ident) -> $r:ident) => {
        paste! {
            pub fn [<$f _display>](
                &mut self,
                mut lhs: Expr<super::$lhs>,
                mut rhs: Expr<super::$rhs>,
                inverted: bool,
                display: Properties
            ) -> Box<dyn Node> {
                let (lhs_node, rhs_node) = (lhs.take_node(), rhs.take_node());
                self.rule_with(UnrolledRuleKind::$r(lhs, rhs), lhs_node, rhs_node, inverted, display, ProcNum::one())
            }

            pub fn $f(
                &mut self,
                lhs: Expr<super::$lhs>,
                rhs: Expr<super::$rhs>,
                inverted: bool
            ) -> Box<dyn Node> {
                self.[<$f _display>](lhs, rhs, inverted, Properties::default())
            }
        }

    };
}

// Rule constructors
impl CompileContext {
    /// Make a rule with properties and nodes.
    fn rule_with<N: Node + 'static, M: Node + 'static>(
        &mut self,
        kind: UnrolledRuleKind,
        lhs: Option<N>,
        rhs: Option<M>,
        inverted: bool,
        mut display: Properties,
        def_weight: ProcNum,
    ) -> Box<dyn Node> {
        let weight = display.get("weight").get_or(def_weight);
        let mut node = CollectionNode::from_display(display, self);

        node.extend(lhs);
        node.extend(rhs);

        self.push_rule(UnrolledRule {
            kind,
            inverted,
            weight,
        });

        Box::new(node)
    }

    generic_rule! {scalar_eq(Number, Number) -> NumberEq}
    generic_rule! {point_eq(Point, Point) -> PointEq}
    generic_rule! {gt(Number, Number) -> Gt}
}
