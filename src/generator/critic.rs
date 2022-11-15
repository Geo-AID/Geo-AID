use std::sync::Arc;

use crate::{script::{Criteria, CriteriaKind, Expression, ComplexUnit, SimpleUnit, Weighed}, generator::geometry};

use super::{Complex, Logger, EvaluationError};

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

/// Inverts the quality. As simple as 1 - q
#[inline]
fn invert(q: f64) -> f64 {
    1.0 - q
}

fn evaluate_expression(expr: &Weighed<Expression>, weights: &mut Vec<f64>, points: &PointVec, _logger: &mut Logger, weight_mult: f64)
    -> Result<(Complex, ComplexUnit), EvaluationError> {
    Ok(match &expr.object {
        Expression::PointPointDistance(p1, p2) => {
            // Evaluate the two points
            let p1 = evaluate_expression(p1, weights, points, _logger, weight_mult * expr.weight)?;
            let p2 = evaluate_expression(p2, weights, points, _logger, weight_mult * expr.weight)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));

            // Pythagorean theorem
            let distance = (p1.0 - p2.0).mangitude();
            (Complex::new(distance, 0.0), ComplexUnit::new(SimpleUnit::Distance))
        },
        Expression::PointLineDistance(point, line) => {
            // Evaluate the line and the point
            let point = evaluate_expression(point, weights, points, _logger, weight_mult * expr.weight)?;
            let line = evaluate_expression(line, weights, points, _logger, weight_mult * expr.weight)?;

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

                // logger.push(format!("A = {a}, B = {b}, C = {c}"));
                // logger.push(format!("A^2 = {}, B^2 = {}", a.powi(2), b.powi(2)));
                // logger.push(format!("(x0, y0) = {}", point.0));
                // logger.push(format!("Ax0 + By0 + C = {}", a * point.0.real + b * point.0.imaginary + c));
                // logger.push(format!("|Ax0 + By0 + C|/sqrt(A^2 + B^2) = {}", f64::abs(a * point.0.real + b * point.0.imaginary + c) / f64::sqrt(a.powi(2) + b.powi(2))));

                f64::abs(a * point.0.real + b * point.0.imaginary + c) / f64::sqrt(a.powi(2) + b.powi(2))
            };

            (Complex::new(distance, 0.0), ComplexUnit::new(SimpleUnit::Distance))
        },
        Expression::AnglePoint(p1, p2, p3) => {
            // Evaluate the two points
            let p1 = evaluate_expression(p1, weights, points, _logger, weight_mult * expr.weight)?;
            let p2 = evaluate_expression(p2, weights, points, _logger, weight_mult * expr.weight)?;
            let p3 = evaluate_expression(p3, weights, points, _logger, weight_mult * expr.weight)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p3.1, ComplexUnit::new(SimpleUnit::Point));

            let (arm1, origin, arm2) = (p1.0, p2.0, p3.0);

            // Get the vectors to calculate the angle between them.
            let arm1_vec = arm1 - origin;
            let arm2_vec = arm2 - origin;

            // Get the dot product
            let dot_product = arm1_vec.real * arm2_vec.real + arm1_vec.imaginary * arm2_vec.imaginary;
            // Get the angle
            let angle = f64::acos(dot_product / (arm1_vec.mangitude() * arm2_vec.mangitude()));

            (Complex::new(angle, 0.0), ComplexUnit::new(SimpleUnit::Angle))
        },
        Expression::Literal(v, unit) => (Complex::new(*v, 0.0), unit.clone()),
        Expression::FreePoint(p) => {
            weights[*p] += expr.weight * weight_mult;

            (points[*p].0, ComplexUnit::new(SimpleUnit::Point))
        },
        Expression::Line(p1, p2) => {
            // Evaluate the two points
            let p1 = evaluate_expression(p1, weights, points, _logger, weight_mult * expr.weight)?;
            let p2 = evaluate_expression(p2, weights, points, _logger, weight_mult * expr.weight)?;

            assert_eq!(p1.1, ComplexUnit::new(SimpleUnit::Point));
            assert_eq!(p2.1, ComplexUnit::new(SimpleUnit::Point));

            (geometry::get_line(p1.0, p2.0), ComplexUnit::new(SimpleUnit::Line))
        },
        Expression::LineCrossing(l1, l2) => {
            // Evaluate the two lines
            let l1 = evaluate_expression(l1, weights, points, _logger, weight_mult * expr.weight)?;
            let l2 = evaluate_expression(l2, weights, points, _logger, weight_mult * expr.weight)?;

            assert_eq!(l1.1, ComplexUnit::new(SimpleUnit::Line));
            assert_eq!(l2.1, ComplexUnit::new(SimpleUnit::Line));

            (geometry::get_crossing(l1.0, l2.0)?, ComplexUnit::new(SimpleUnit::Point))
        },
    })
}

fn evaluate_single(crit: &CriteriaKind, points: &PointVec, logger: &mut Logger) -> (f64, Vec<f64>) {
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
                invert(smooth_0_inf(1130.0 * diff.powi(2)))
            } else {
                // An evaluation error means the criteria is surely not met
                0.0
            }
        },
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
                smooth_inf_inf(-54.0 * f64::cbrt(diff + 0.001))
            } else {
                // An evaluation error means the criteria is surely not met
                0.0
            }
        },
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
                smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001))
            } else {
                // An evaluation error means the criteria is surely not met
                0.0
            }
        },
        // There's a problem here: if there is an evaluation error, the inverse is treated as 100% met, which might lead to some problems in some edge cases.
        CriteriaKind::Inverse(kind) => {
            let (quality, ws) = evaluate_single(kind, points, logger);
            weights = ws;
            invert(quality)
        },
    };

    (quality, weights)
}

pub fn evaluate(points: PointVec, criteria: &Arc<Vec<Criteria>>, logger: &mut Logger) -> PointVec {
   let mut point_evaluation = Vec::new();
   point_evaluation.resize(points.len(), Vec::new());

    for crit in criteria.iter() {
        // println!("Evaluating criteria {:#?}", crit);
        let (quality, weights) = evaluate_single(&crit.object, &points, logger);

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
    point_evaluation.into_iter().enumerate().map(
        |(i, eval)| (points[i].0, if !eval.is_empty() {weighed_mean(eval.into_iter())} else {1.0})
    ).collect()
}