use std::{cell::RefCell, collections::HashMap, sync::Arc};

use crate::{
    generator::geometry,
    script::{
        unit, ComplexUnit, Criteria, CriteriaKind, Expression, HashableWeakArc, SimpleUnit, Weighed,
    },
};

use super::{Complex, EvaluationError, ExprCache, Flags, Logger};

type PointVec = Vec<(Complex, f64)>;

/// That's the infamous sigma function. It packs a [0, +inf) range into [0, 1).
fn smooth_0_inf(x: f64) -> f64 {
    1.0 - 1.0 / (x + 1.0)
}

/// That's the infamous sigma function. It packs a (-inf, +inf) range into (0, 1).
fn smooth_inf_inf(x: f64) -> f64 {
    if x >= 0.0 {
        (1.0 + smooth_0_inf(x)) / 2.0
    } else {
        (1.0 - smooth_0_inf(-x)) / 2.0
    }
}

fn weighed_mean<I: Iterator<Item = (f64, f64)>>(it: I) -> f64 {
    let mut sum = 0.0;
    let mut weight_sum = 0.0;

    for (value, weight) in it {
        sum += value * weight;
        weight_sum += weight;
    }

    sum / weight_sum
}

#[derive(Debug, Clone, Copy)]
enum Quality {
    /// Absolute zero, cannot be inverted. Used with evaluation errors.
    Zero,
    /// An actual value that can be manipulated.
    Some(f64),
}

impl PartialEq for Quality {
    fn eq(&self, other: &Self) -> bool {
        self.as_f64().eq(&other.as_f64())
    }
}

impl Eq for Quality {}

impl PartialOrd for Quality {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_f64().partial_cmp(&other.as_f64())
    }
}

impl Quality {
    #[must_use]
    pub fn as_f64(&self) -> f64 {
        match self {
            Self::Zero => 0.0,
            Self::Some(v) => *v,
        }
    }
}

/// Inverts the quality. As simple as 1 - q
#[inline]
fn invert(q: Quality) -> Quality {
    match q {
        Quality::Zero => Quality::Zero,
        Quality::Some(v) => Quality::Some(1.0 - v),
    }
}

pub struct EvaluationArgs<'r> {
    logger: &'r mut Logger,
    points: &'r PointVec,
    weights: RefCell<Vec<f64>>,
    generation: u64,
    flags: &'r Arc<Flags>,
    record: &'r RefCell<HashMap<HashableWeakArc<Weighed<Expression>>, ExprCache>>,
}

fn evaluate_point_point_distance(
    p1: &Arc<Weighed<Expression>>,
    p2: &Arc<Weighed<Expression>>,
    weight_mult: f64,
    args: &EvaluationArgs,
) -> Result<(Complex, ComplexUnit), EvaluationError> {
    // Evaluate the two points
    let p1 = evaluate_expression(p1, weight_mult * p1.weight, args)?;
    let p2 = evaluate_expression(p2, weight_mult * p2.weight, args)?;

    assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
    assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));

    // Pythagorean theorem
    let distance = (p1.0 - p2.0).mangitude();
    Ok((
        Complex::new(distance, 0.0),
        ComplexUnit::new(SimpleUnit::Distance),
    ))
}

fn evaluate_point_line_distance(
    point: &Arc<Weighed<Expression>>,
    line: &Arc<Weighed<Expression>>,
    weight_mult: f64,
    args: &EvaluationArgs,
) -> Result<(Complex, ComplexUnit), EvaluationError> {
    // Evaluate the line and the point
    let point = evaluate_expression(point, weight_mult * point.weight, args)?;
    let line = evaluate_expression(line, weight_mult * line.weight, args)?;

    assert_eq!(point.1, ComplexUnit::new(SimpleUnit::Point));
    assert_eq!(line.1, ComplexUnit::new(SimpleUnit::Line));

    // If the line's vertical (slope is NaN), the question's easy
    let distance = if line.0.real.is_infinite() {
        f64::abs(point.0.real - line.0.imaginary)
    } else {
        // We use the formula
        // Get the general form

        // A is the slope
        let a = line.0.real;
        // B is -1
        let b = -1.0f64;
        // C is the intercept
        let c = line.0.imaginary;

        // And the magical formula
        f64::abs(a * point.0.real + b * point.0.imaginary + c) / f64::sqrt(a.powi(2) + b.powi(2))
    };

    Ok((
        Complex::new(distance, 0.0),
        ComplexUnit::new(SimpleUnit::Distance),
    ))
}

#[allow(clippy::too_many_lines)]
fn evaluate_expression(
    expr: &Arc<Weighed<Expression>>,
    weight_mult: f64,
    args: &EvaluationArgs,
) -> Result<(Complex, ComplexUnit), EvaluationError> {
    // If the flag is on, check if this expression has already been calculated in this generation. If so, return that value.
    if args.flags.optimizations.identical_expressions && Arc::strong_count(expr) > 1 {
        let key = HashableWeakArc::new(Arc::downgrade(expr));
        let mut borrowed = args.record.borrow_mut();
        let v = borrowed.get_mut(&key).unwrap();

        if v.generation == args.generation {
            return Ok((v.value, v.unit.clone()));
        }
    }

    // If no flag is on or no value is cached yet, compute.
    let computed = match &expr.object {
        Expression::PointPointDistance(p1, p2) => {
            evaluate_point_point_distance(p1, p2, weight_mult, args)?
        }
        Expression::PointLineDistance(point, line) => {
            evaluate_point_line_distance(point, line, weight_mult, args)?
        }
        Expression::AnglePoint(p1, p2, p3) => {
            // Evaluate the three points
            let p1 = evaluate_expression(p1, weight_mult * p1.weight, args)?;
            let p2 = evaluate_expression(p2, weight_mult * p2.weight, args)?;
            let p3 = evaluate_expression(p3, weight_mult * p3.weight, args)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p3.1, ComplexUnit::new(SimpleUnit::Point));

            let (arm1, origin, arm2) = (p1.0, p2.0, p3.0);
            (
                Complex::new(geometry::get_angle(arm1, origin, arm2), 0.0),
                ComplexUnit::new(SimpleUnit::Angle),
            )
        }
        Expression::Literal(v, unit) => (Complex::new(*v, 0.0), unit.clone()),
        Expression::FreePoint(p) => {
            args.weights.borrow_mut()[*p] += expr.weight * weight_mult;

            (args.points[*p].0, ComplexUnit::new(SimpleUnit::Point))
        }
        Expression::Line(p1, p2) => {
            // Evaluate the two points
            let p1 = evaluate_expression(p1, weight_mult * p1.weight, args)?;
            let p2 = evaluate_expression(p2, weight_mult * p2.weight, args)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));

            (
                geometry::get_line(p1.0, p2.0),
                ComplexUnit::new(SimpleUnit::Line),
            )
        }
        Expression::LineLineIntersection(l1, l2) => {
            // Evaluate the two lines
            let l1 = evaluate_expression(l1, weight_mult * l1.weight, args)?;
            let l2 = evaluate_expression(l2, weight_mult * l2.weight, args)?;

            assert_eq!(l1.1, ComplexUnit::new(SimpleUnit::Line));
            assert_eq!(l2.1, ComplexUnit::new(SimpleUnit::Line));

            (
                geometry::get_crossing(l1.0, l2.0)?,
                ComplexUnit::new(SimpleUnit::Point),
            )
        }
        Expression::SetUnit(e, unit) => {
            // Evaluate e
            let e = evaluate_expression(e, weight_mult * e.weight, args)?;

            (e.0, unit.clone())
        }
        Expression::Sum(e1, e2) => {
            let v1 = evaluate_expression(e1, weight_mult * e1.weight, args)?;
            let v2 = evaluate_expression(e2, weight_mult * e2.weight, args)?;

            assert_eq!(v1.1, v2.1);

            (v1.0 + v2.0, v1.1)
        }
        Expression::Difference(e1, e2) => {
            let v1 = evaluate_expression(e1, weight_mult * e1.weight, args)?;
            let v2 = evaluate_expression(e2, weight_mult * e2.weight, args)?;

            assert_eq!(v1.1, v2.1);

            (v1.0 - v2.0, v1.1)
        }
        Expression::Product(e1, e2) => {
            let v1 = evaluate_expression(e1, weight_mult * e1.weight, args)?;
            let v2 = evaluate_expression(e2, weight_mult * e2.weight, args)?;

            (v1.0 * v2.0, v1.1 * v2.1)
        }
        Expression::Quotient(e1, e2) => {
            let v1 = evaluate_expression(e1, weight_mult * e1.weight, args)?;
            let v2 = evaluate_expression(e2, weight_mult * e2.weight, args)?;

            (v1.0 / v2.0, v1.1 / v2.1)
        }
        Expression::Negation(expr) => {
            let v = evaluate_expression(expr, weight_mult * expr.weight, args)?;

            (-v.0, v.1)
        }
        Expression::AngleLine(l1, l2) => {
            // Evaluate the two lines
            let l1 = evaluate_expression(l1, weight_mult * l1.weight, args)?;
            let l2 = evaluate_expression(l2, weight_mult * l2.weight, args)?;

            assert_eq!(l1.1, ComplexUnit::new(SimpleUnit::Line));
            assert_eq!(l2.1, ComplexUnit::new(SimpleUnit::Line));

            let arm1 = if l1.0.real.is_infinite() {
                Complex::new(0.0, 1.0) // (0, 1) - vertical
            } else {
                Complex::new(1.0, l1.0.real)
            };

            let arm2 = if l2.0.real.is_infinite() {
                Complex::new(0.0, 1.0) // (0, 1) - vertical
            } else {
                Complex::new(1.0, l2.0.real)
            };

            let origin = Complex::new(0.0, 0.0);

            (
                Complex::new(geometry::get_angle(arm1, origin, arm2), 0.0),
                ComplexUnit::new(SimpleUnit::Angle),
            )
        }
        Expression::AngleBisector(p1, p2, p3) => {
            // Evaluate the three points
            let p1 = evaluate_expression(p1, weight_mult * p1.weight, args)?;
            let p2 = evaluate_expression(p2, weight_mult * p2.weight, args)?;
            let p3 = evaluate_expression(p3, weight_mult * p3.weight, args)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p3.1, ComplexUnit::new(SimpleUnit::Point));

            let angle = geometry::get_angle(p1.0, p2.0, p3.0) / 2.0;

            (geometry::rotate_around(p1.0, p2.0, angle), unit::POINT)
        }
        Expression::Average(exprs) => {
            // Evaluate all
            let exprs = exprs
                .iter()
                .map(|expr| evaluate_expression(expr, weight_mult * expr.weight, args))
                .collect::<Result<Vec<(Complex, ComplexUnit)>, EvaluationError>>()?;

            // Assume all types are valid. Typechecking should have already been done.

            let mut sum = Complex::new(0.0, 0.0);

            for expr in &exprs {
                sum += expr.0;
            }

            #[allow(clippy::cast_precision_loss)]
            (sum / exprs.len() as f64, exprs[0].1.clone())
        }
        Expression::PerpendicularThrough(l, p) => {
            let l = evaluate_expression(l, weight_mult * l.weight, args)?;
            let p = evaluate_expression(p, weight_mult * p.weight, args)?;

            // Find the right a coefficient.
            let a = if l.0.real.is_infinite() {
                0.0 // vertical -> horizontal
            } else if l.0.real.abs() <= 0.000_000_1 {
                f64::INFINITY // horizontal -> vertical
            } else {
                -1.0 / l.0.real
            };

            let b = if a.is_infinite() {
                p.0.real // If vertical, simply adjust the x.
            } else {
                p.0.imaginary - a * p.0.real
            };

            (Complex::new(a, b), unit::LINE)
        }
        Expression::ParallelThrough(l, p) => {
            let l = evaluate_expression(l, weight_mult * l.weight, args)?;
            let p = evaluate_expression(p, weight_mult * p.weight, args)?;

            // Find the right a coefficient.
            let a = l.0.real;
            let b = if a.is_infinite() {
                p.0.real // If vertical, simply adjust the x.
            } else {
                p.0.imaginary - a * p.0.real
            };

            (Complex::new(a, b), unit::LINE)
        }
    };

    // If the flag is on, and the expr has not been cached, cache it.
    if args.flags.optimizations.identical_expressions && Arc::strong_count(expr) > 1 {
        let key = HashableWeakArc::new(Arc::downgrade(expr));
        let mut borrowed = args.record.borrow_mut();
        let v = borrowed.get_mut(&key).unwrap();

        v.generation = args.generation;
        v.value = computed.0;
        v.unit = computed.1.clone();
    }

    Ok(computed)
}

///# Panics
///# Errors

#[allow(clippy::too_many_lines)]
pub fn evaluate_expression_simple(
    expr: &Arc<Weighed<Expression>>,
    generated_points: &[Complex],
) -> Result<(Complex, ComplexUnit), EvaluationError> {
    Ok(match &expr.object {
        Expression::PointPointDistance(p1, p2) => {
            let p1 = evaluate_expression_simple(p1, generated_points)?;
            let p2 = evaluate_expression_simple(p2, generated_points)?;

            // Pythagorean theorem
            let distance = (p1.0 - p2.0).mangitude();
            (
                Complex::new(distance, 0.0),
                ComplexUnit::new(SimpleUnit::Distance),
            )
        }
        Expression::PointLineDistance(point, line) => {
            // Evaluate the line and the point
            let point = evaluate_expression_simple(point, generated_points)?;
            let line = evaluate_expression_simple(line, generated_points)?;

            // If the line's vertical (slope is NaN), the question's easy
            let distance = if line.0.real.is_infinite() {
                f64::abs(point.0.real - line.0.imaginary)
            } else {
                // We use the formula
                // Get the general form

                // A is the slope
                let a = line.0.real;
                // B is -1
                let b = -1.0f64;
                // C is the intercept
                let c = line.0.imaginary;

                // And the magical formula
                f64::abs(a * point.0.real + b * point.0.imaginary + c)
                    / f64::sqrt(a.powi(2) + b.powi(2))
            };

            (
                Complex::new(distance, 0.0),
                ComplexUnit::new(SimpleUnit::Distance),
            )
        }
        Expression::AnglePoint(p1, p2, p3) => {
            // Evaluate the three points
            let p1 = evaluate_expression_simple(p1, generated_points)?;
            let p2 = evaluate_expression_simple(p2, generated_points)?;
            let p3 = evaluate_expression_simple(p3, generated_points)?;

            let (arm1, origin, arm2) = (p1.0, p2.0, p3.0);
            (
                Complex::new(geometry::get_angle(arm1, origin, arm2), 0.0),
                ComplexUnit::new(SimpleUnit::Angle),
            )
        }
        Expression::Literal(v, unit) => (Complex::new(*v, 0.0), unit.clone()),
        Expression::FreePoint(p) => (generated_points[*p], ComplexUnit::new(SimpleUnit::Point)),
        Expression::Line(p1, p2) => {
            // Evaluate the two points
            let p1 = evaluate_expression_simple(p1, generated_points)?;
            let p2 = evaluate_expression_simple(p2, generated_points)?;

            (
                geometry::get_line(p1.0, p2.0),
                ComplexUnit::new(SimpleUnit::Line),
            )
        }
        Expression::LineLineIntersection(l1, l2) => {
            // Evaluate the two lines
            let l1 = evaluate_expression_simple(l1, generated_points)?;
            let l2 = evaluate_expression_simple(l2, generated_points)?;

            (
                geometry::get_crossing(l1.0, l2.0)?,
                ComplexUnit::new(SimpleUnit::Point),
            )
        }
        Expression::SetUnit(e, unit) => {
            // Evaluate e
            let e = evaluate_expression_simple(e, generated_points)?;

            (e.0, unit.clone())
        }
        Expression::Sum(e1, e2) => {
            let v1 = evaluate_expression_simple(e1, generated_points)?;
            let v2 = evaluate_expression_simple(e2, generated_points)?;

            assert_eq!(v1.1, v2.1);

            (v1.0 + v2.0, v1.1)
        }
        Expression::Difference(e1, e2) => {
            let v1 = evaluate_expression_simple(e1, generated_points)?;
            let v2 = evaluate_expression_simple(e2, generated_points)?;

            assert_eq!(v1.1, v2.1);

            (v1.0 - v2.0, v1.1)
        }
        Expression::Product(e1, e2) => {
            let v1 = evaluate_expression_simple(e1, generated_points)?;
            let v2 = evaluate_expression_simple(e2, generated_points)?;

            (v1.0 * v2.0, v1.1 * v2.1)
        }
        Expression::Quotient(e1, e2) => {
            let v1 = evaluate_expression_simple(e1, generated_points)?;
            let v2 = evaluate_expression_simple(e2, generated_points)?;

            (v1.0 / v2.0, v1.1 / v2.1)
        }
        Expression::Negation(expr) => {
            let v = evaluate_expression_simple(expr, generated_points)?;

            (-v.0, v.1)
        }
        Expression::AngleLine(l1, l2) => {
            // Evaluate the two lines
            let l1 = evaluate_expression_simple(l1, generated_points)?;
            let l2 = evaluate_expression_simple(l2, generated_points)?;

            let arm1 = if l1.0.real.is_infinite() {
                Complex::new(0.0, 1.0) // (0, 1) - vertical
            } else {
                Complex::new(1.0, l1.0.real)
            };

            let arm2 = if l2.0.real.is_infinite() {
                Complex::new(0.0, 1.0) // (0, 1) - vertical
            } else {
                Complex::new(1.0, l2.0.real)
            };

            let origin = Complex::new(0.0, 0.0);

            (
                Complex::new(geometry::get_angle(arm1, origin, arm2), 0.0),
                ComplexUnit::new(SimpleUnit::Angle),
            )
        }
        Expression::AngleBisector(p1, p2, p3) => {
            // Evaluate the three points
            let p1 = evaluate_expression_simple(p1, generated_points)?;
            let p2 = evaluate_expression_simple(p2, generated_points)?;
            let p3 = evaluate_expression_simple(p3, generated_points)?;

            let angle = geometry::get_angle(p1.0, p2.0, p3.0) / 2.0;

            (geometry::rotate_around(p1.0, p2.0, angle), unit::POINT)
        }
        Expression::Average(exprs) => {
            // Evaluate all
            let exprs = exprs
                .iter()
                .map(|expr| evaluate_expression_simple(expr, generated_points))
                .collect::<Result<Vec<(Complex, ComplexUnit)>, EvaluationError>>()?;

            // Assume all types are valid. Typechecking should have already been done.

            let mut sum = Complex::new(0.0, 0.0);

            for expr in &exprs {
                sum += expr.0;
            }

            #[allow(clippy::cast_precision_loss)]
            (sum / exprs.len() as f64, exprs[0].1.clone())
        }
        Expression::PerpendicularThrough(l, p) => {
            let l = evaluate_expression_simple(l, generated_points)?;
            let p = evaluate_expression_simple(p, generated_points)?;

            // Find the right a coefficient.
            let a = if l.0.real.is_infinite() {
                0.0 // vertical -> horizontal
            } else if l.0.real.abs() <= 0.000_000_1 {
                f64::INFINITY // horizontal -> vertical
            } else {
                -1.0 / l.0.real
            };

            let b = if a.is_infinite() {
                p.0.real // If vertical, simply adjust the x.
            } else {
                p.0.imaginary - a * p.0.real
            };

            (Complex::new(a, b), unit::LINE)
        }
        Expression::ParallelThrough(l, p) => {
            let l = evaluate_expression_simple(l, generated_points)?;
            let p = evaluate_expression_simple(p, generated_points)?;

            // Find the right a coefficient.
            let a = l.0.real;
            let b = if a.is_infinite() {
                p.0.real // If vertical, simply adjust the x.
            } else {
                p.0.imaginary - a * p.0.real
            };

            (Complex::new(a, b), unit::LINE)
        }
    })
}

/// Evaluates a single rule in terms of quality.
fn evaluate_single(
    crit: &CriteriaKind,
    points: &PointVec,
    logger: &mut Logger,
    generation: u64,
    flags: &Arc<Flags>,
    record: &RefCell<HashMap<HashableWeakArc<Weighed<Expression>>, ExprCache>>,
) -> (Quality, Vec<f64>) {
    let mut weights = Vec::new();
    weights.resize(points.len(), 0.0);

    let mut args = EvaluationArgs {
        logger,
        points,
        weights: RefCell::new(weights),
        generation,
        flags,
        record,
    };

    let quality = match crit {
        CriteriaKind::Equal(e1, e2) => {
            let v1 = evaluate_expression(e1, 1.0, &args);
            let v2 = evaluate_expression(e2, 1.0, &args);

            if let (Ok(v1), Ok(v2)) = (v1, v2) {
                assert_eq!(v1.1, v2.1);

                let diff = (v1.0 - v2.0).mangitude();
                // Interestingly, it's easier to calculate the quality function for != and then invert it.
                invert(Quality::Some(smooth_0_inf(1130.0 * diff.powi(2))))
            } else {
                // An evaluation error means the criteria is surely not met
                Quality::Zero
            }
        }
        CriteriaKind::Less(e1, e2) => {
            let v1 = evaluate_expression(e1, 1.0, &args);
            let v2 = evaluate_expression(e2, 1.0, &args);

            if let (Ok(v1), Ok(v2)) = (v1, v2) {
                assert_eq!(v1.1, v2.1);

                assert_eq!(v1.1[SimpleUnit::Point as usize], 0);
                assert_eq!(v1.1[SimpleUnit::Line as usize], 0);

                // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
                let diff = (v1.0.real - v2.0.real) / v1.0.real;
                // logger.push(format!("Distance is {}", v1.0.real));
                // logger.push(format!("Diff's {diff}"));
                // Interestingly, it's easier to calculate the quality function for != and then invert it.
                Quality::Some(smooth_inf_inf(-54.0 * f64::cbrt(diff + 0.001)))
            } else {
                // An evaluation error means the criteria is surely not met
                Quality::Zero
            }
        }
        CriteriaKind::Greater(e1, e2) => {
            let v1 = evaluate_expression(e1, 1.0, &args);
            let v2 = evaluate_expression(e2, 1.0, &args);

            if let (Ok(v1), Ok(v2)) = (v1, v2) {
                assert_eq!(v1.1, v2.1);

                assert_eq!(v1.1[SimpleUnit::Point as usize], 0);
                assert_eq!(v1.1[SimpleUnit::Line as usize], 0);

                // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
                let diff = (v1.0.real - v2.0.real) / v1.0.real;
                // Interestingly, it's easier to calculate the quality function for != and then invert it.
                Quality::Some(smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001)))
            } else {
                // An evaluation error means the criteria is surely not met
                Quality::Zero
            }
        }
        // There's a problem here: if there is an evaluation error, the inverse is treated as 100% met, which might lead to some problems in some edge cases.
        CriteriaKind::Inverse(kind) => {
            let (quality, ws) =
                evaluate_single(kind, points, args.logger, generation, flags, record);
            args.weights = RefCell::new(ws);
            invert(quality)
        }
    };

    (quality, args.weights.take())
}

/// Evaluates all rules in terms of quality
pub fn evaluate(
    points: &PointVec,
    criteria: &Arc<Vec<Criteria>>,
    logger: &mut Logger,
    generation: u64,
    flags: &Arc<Flags>,
    record: &RefCell<HashMap<HashableWeakArc<Weighed<Expression>>, ExprCache>>,
) -> PointVec {
    let mut point_evaluation = Vec::new();
    point_evaluation.resize(points.len(), Vec::new());

    for crit in criteria.iter() {
        // println!("Evaluating criteria {:#?}", crit);
        let (quality, weights) =
            evaluate_single(&crit.object, points, logger, generation, flags, record);

        // println!("Evaluation result: {quality}, {:?}", weights);

        // Normalize weights (squeeze to [0, 1]) and add (quality, weight) pairs to the points
        let weight_sum = weights.iter().sum::<f64>();
        for (i, weight) in weights.into_iter().enumerate() {
            if weight != 0.0 {
                point_evaluation[i].push((quality, crit.weight * weight / weight_sum));
            }
        }
    }

    // Find the final evaluation
    point_evaluation
        .into_iter()
        .enumerate()
        .map(|(i, eval)| {
            (
                points[i].0,
                if eval.is_empty() {
                    1.0
                } else {
                    weighed_mean(eval.into_iter().map(|v| (v.0.as_f64(), v.1)))
                },
            )
        })
        .collect()
}
