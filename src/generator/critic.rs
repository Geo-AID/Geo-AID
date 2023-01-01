use std::sync::Arc;

use crate::{
    generator::geometry,
    script::{ComplexUnit, Criteria, CriteriaKind, Expression, SimpleUnit, Weighed, unit},
};

use super::{Complex, EvaluationError, Logger};

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

fn evaluate_point_point_distance(
    p1: &Weighed<Expression>,
    p2: &Weighed<Expression>,
    weights: &mut Vec<f64>,
    points: &PointVec,
    logger: &mut Logger,
    weight_mult: f64,
) -> Result<(Complex, ComplexUnit), EvaluationError> {
    // Evaluate the two points
    let p1 = evaluate_expression(p1, weights, points, logger, weight_mult)?;
    let p2 = evaluate_expression(p2, weights, points, logger, weight_mult)?;

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
    point: &Weighed<Expression>,
    line: &Weighed<Expression>,
    weights: &mut Vec<f64>,
    points: &PointVec,
    logger: &mut Logger,
    weight_mult: f64,
) -> Result<(Complex, ComplexUnit), EvaluationError> {
    // Evaluate the line and the point
    let point = evaluate_expression(point, weights, points, logger, weight_mult)?;
    let line = evaluate_expression(line, weights, points, logger, weight_mult)?;

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
    expr: &Weighed<Expression>,
    weights: &mut Vec<f64>,
    points: &PointVec,
    logger: &mut Logger,
    weight_mult: f64,
) -> Result<(Complex, ComplexUnit), EvaluationError> {
    Ok(match &expr.object {
        Expression::PointPointDistance(p1, p2) => evaluate_point_point_distance(
            p1,
            p2,
            weights,
            points,
            logger,
            weight_mult * expr.weight,
        )?,
        Expression::PointLineDistance(point, line) => evaluate_point_line_distance(
            point,
            line,
            weights,
            points,
            logger,
            weight_mult * expr.weight,
        )?,
        Expression::AnglePoint(p1, p2, p3) => {
            // Evaluate the three points
            let p1 = evaluate_expression(p1, weights, points, logger, weight_mult * expr.weight)?;
            let p2 = evaluate_expression(p2, weights, points, logger, weight_mult * expr.weight)?;
            let p3 = evaluate_expression(p3, weights, points, logger, weight_mult * expr.weight)?;

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
            weights[*p] += expr.weight * weight_mult;

            (points[*p].0, ComplexUnit::new(SimpleUnit::Point))
        }
        Expression::Line(p1, p2) => {
            // Evaluate the two points
            let p1 = evaluate_expression(p1, weights, points, logger, weight_mult * expr.weight)?;
            let p2 = evaluate_expression(p2, weights, points, logger, weight_mult * expr.weight)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));

            (
                geometry::get_line(p1.0, p2.0),
                ComplexUnit::new(SimpleUnit::Line),
            )
        }
        Expression::LineLineIntersection(l1, l2) => {
            // Evaluate the two lines
            let l1 = evaluate_expression(l1, weights, points, logger, weight_mult * expr.weight)?;
            let l2 = evaluate_expression(l2, weights, points, logger, weight_mult * expr.weight)?;

            assert_eq!(l1.1, ComplexUnit::new(SimpleUnit::Line));
            assert_eq!(l2.1, ComplexUnit::new(SimpleUnit::Line));

            (
                geometry::get_crossing(l1.0, l2.0)?,
                ComplexUnit::new(SimpleUnit::Point),
            )
        }
        Expression::SetUnit(e, unit) => {
            // Evaluate e
            let e = evaluate_expression(e, weights, points, logger, weight_mult * e.weight)?;

            (e.0, unit.clone())
        }
        Expression::Sum(e1, e2) => {
            let v1 = evaluate_expression(e1, weights, points, logger, weight_mult * e1.weight)?;
            let v2 = evaluate_expression(e2, weights, points, logger, weight_mult * e2.weight)?;

            assert_eq!(v1.1, v2.1);

            (v1.0 + v2.0, v1.1)
        }
        Expression::Difference(e1, e2) => {
            let v1 = evaluate_expression(e1, weights, points, logger, weight_mult * e1.weight)?;
            let v2 = evaluate_expression(e2, weights, points, logger, weight_mult * e2.weight)?;

            assert_eq!(v1.1, v2.1);

            (v1.0 - v2.0, v1.1)
        }
        Expression::Product(e1, e2) => {
            let v1 = evaluate_expression(e1, weights, points, logger, weight_mult * e1.weight)?;
            let v2 = evaluate_expression(e2, weights, points, logger, weight_mult * e2.weight)?;

            (v1.0 * v2.0, v1.1 * v2.1)
        }
        Expression::Quotient(e1, e2) => {
            let v1 = evaluate_expression(e1, weights, points, logger, weight_mult * e1.weight)?;
            let v2 = evaluate_expression(e2, weights, points, logger, weight_mult * e2.weight)?;

            (v1.0 / v2.0, v1.1 / v2.1)
        }
        Expression::Negation(expr) => {
            let v = evaluate_expression(expr, weights, points, logger, weight_mult * expr.weight)?;

            (-v.0, v.1)
        }
        Expression::AngleLine(l1, l2) => {
            // Evaluate the two lines
            let l1 = evaluate_expression(l1, weights, points, logger, weight_mult * expr.weight)?;
            let l2 = evaluate_expression(l2, weights, points, logger, weight_mult * expr.weight)?;

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
            let p1 = evaluate_expression(p1, weights, points, logger, weight_mult * expr.weight)?;
            let p2 = evaluate_expression(p2, weights, points, logger, weight_mult * expr.weight)?;
            let p3 = evaluate_expression(p3, weights, points, logger, weight_mult * expr.weight)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p3.1, ComplexUnit::new(SimpleUnit::Point));

            let angle = geometry::get_angle(p1.0, p2.0, p3.0) / 2.0;

            (
                geometry::rotate_around(p1.0, p2.0, angle),
                unit::POINT
            )
        },
        Expression::Average(exprs) => {
            // Evaluate all
            let exprs = exprs.iter().map(
                |expr| evaluate_expression(expr, weights, points, logger, weight_mult * expr.weight)
            ).collect::<Result<Vec<(Complex, ComplexUnit)>, EvaluationError>>()?;
            
            // Assume all types are valid. Typechecking should have already been done.

            let mut sum = Complex::new(0.0, 0.0);

            for expr in &exprs {
                sum += expr.0;
            }

            #[allow(clippy::cast_precision_loss)]
            (
                sum / exprs.len() as f64,
                exprs[0].1.clone()
            )
        }
        Expression::PerpendicularThrough(l, p) => {
            let l = evaluate_expression(l, weights, points, logger, weight_mult * l.weight)?;
            let p = evaluate_expression(p, weights, points, logger, weight_mult * p.weight)?;

            // Find the right a coefficient.
            let a = if l.0.real.is_infinite() {
                0.0 // vertical -> horizontal
            } else if l.0.real.abs() <= 0.000_000_1 {
                f64::INFINITY // horizontal -> vertical
            } else {
                - 1.0 / l.0.real
            };

            let b = if a.is_infinite() {
                p.0.real // If vertical, simply adjust the x.
            } else {
                p.0.imaginary - a * p.0.real
            };

            (
                Complex::new(a, b),
                unit::LINE
            )
        },
        Expression::ParallelThrough(l, p) => {
            let l = evaluate_expression(l, weights, points, logger, weight_mult * l.weight)?;
            let p = evaluate_expression(p, weights, points, logger, weight_mult * p.weight)?;

            // Find the right a coefficient.
            let a = l.0.real;
            let b = if a.is_infinite() {
                p.0.real // If vertical, simply adjust the x.
            } else {
                p.0.imaginary - a * p.0.real
            };

            (
                Complex::new(a, b),
                unit::LINE
            )
        },
    })
}

/// Evaluates a single rule in terms of quality.
fn evaluate_single(
    crit: &CriteriaKind,
    points: &PointVec,
    logger: &mut Logger,
) -> (Quality, Vec<f64>) {
    let mut weights = Vec::new();
    weights.resize(points.len(), 0.0);

    let quality = match crit {
        CriteriaKind::Equal(e1, e2) => {
            let v1 = evaluate_expression(e1, &mut weights, points, logger, 1.0);
            let v2 = evaluate_expression(e2, &mut weights, points, logger, 1.0);

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
            let v1 = evaluate_expression(e1, &mut weights, points, logger, 1.0);
            let v2 = evaluate_expression(e2, &mut weights, points, logger, 1.0);

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
            let v1 = evaluate_expression(e1, &mut weights, points, logger, 1.0);
            let v2 = evaluate_expression(e2, &mut weights, points, logger, 1.0);

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
            let (quality, ws) = evaluate_single(kind, points, logger);
            weights = ws;
            invert(quality)
        }
    };

    (quality, weights)
}

/// Evaluates all rules in terms of quality
pub fn evaluate(points: &PointVec, criteria: &Arc<Vec<Criteria>>, logger: &mut Logger) -> PointVec {
    let mut point_evaluation = Vec::new();
    point_evaluation.resize(points.len(), Vec::new());

    for crit in criteria.iter() {
        // println!("Evaluating criteria {:#?}", crit);
        let (quality, weights) = evaluate_single(&crit.object, points, logger);

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
