use std::{cell::RefCell, collections::HashMap, sync::Arc};

use crate::script::{Criteria, CriteriaKind};

use super::{expression::ExprCache, Adjustable, Flags, Logger};

type AdjustableVec = Vec<(Adjustable, f64)>;

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
pub enum Quality {
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

pub type Cache = HashMap<usize, ExprCache>;

pub struct EvaluationArgs<'r> {
    pub logger: &'r mut Logger,
    pub adjustables: &'r [(Adjustable, f64)],
    pub generation: u64,
    pub flags: &'r Arc<Flags>,
    pub cache: Option<&'r RefCell<Cache>>,
}

/// Evaluates a single rule in terms of quality.
fn evaluate_single(
    crit: &CriteriaKind,
    adjustables: &AdjustableVec,
    logger: &mut Logger,
    generation: u64,
    flags: &Arc<Flags>,
    cache: Option<&RefCell<Cache>>,
) -> (Quality, Vec<f64>) {
    let args = EvaluationArgs {
        logger,
        adjustables,
        generation,
        flags,
        cache,
    };

    match crit {
        CriteriaKind::EqualScalar(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let weigths = e1.weights.clone() + &e2.weights;

            (
                if let (Ok(v1), Ok(v2)) = (v1, v2) {
                    let diff = v1 - v2;
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    invert(Quality::Some(smooth_0_inf(1130.0 * diff.powi(2))))
                } else {
                    // An evaluation error means the criteria is surely not met
                    Quality::Zero
                },
                weigths.0,
            )
        }
        CriteriaKind::EqualPoint(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let weigths = e1.weights.clone() + &e2.weights;

            (
                if let (Ok(v1), Ok(v2)) = (v1, v2) {
                    let diff = (v1 - v2).mangitude();
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    invert(Quality::Some(smooth_0_inf(1130.0 * diff.powi(2))))
                } else {
                    // An evaluation error means the criteria is surely not met
                    Quality::Zero
                },
                weigths.0,
            )
        }
        CriteriaKind::Less(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let weigths = e1.weights.clone() + &e2.weights;

            (
                if let (Ok(v1), Ok(v2)) = (v1, v2) {
                    // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
                    let (v1, v2) = (v1, v2);
                    let diff = 1.0 - v2 / v1;
                    // logger.push(format!("Distance is {}", v1.0.real));
                    // logger.push(format!("Diff's {diff}"));
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    Quality::Some(smooth_inf_inf(-54.0 * f64::cbrt(diff + 0.001)))
                } else {
                    // An evaluation error means the criteria is surely not met
                    Quality::Zero
                },
                weigths.0,
            )
        }
        CriteriaKind::Greater(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let weigths = e1.weights.clone() + &e2.weights;

            (
                if let (Ok(v1), Ok(v2)) = (v1, v2) {
                    // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
                    let (v1, v2) = (v1, v2);
                    let diff = 1.0 - v2 / v1;
                    println!("{v1} > {v2} Satisfied with {}", smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001)));
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    Quality::Some(smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001)))
                } else {
                    // An evaluation error means the criteria is surely not met
                    Quality::Zero
                },
                weigths.0,
            )
        }
        // Note: Inversed zero quality is still zero.
        CriteriaKind::Inverse(kind) => {
            let (quality, ws) =
                evaluate_single(kind, adjustables, args.logger, generation, flags, cache);
            (invert(quality), ws)
        }
        CriteriaKind::Bias(expr) => (Quality::Some(1.0), expr.weights.clone().0),
    }
}

#[allow(clippy::implicit_hasher)]
/// Evaluates all rules in terms of quality
pub fn evaluate(
    points: &AdjustableVec,
    criteria: &Arc<Vec<Criteria>>,
    logger: &mut Logger,
    generation: u64,
    flags: &Arc<Flags>,
    cache: Option<&RefCell<Cache>>,
    point_evaluation: &mut [Vec<(Quality, f64)>],
) -> AdjustableVec {
    for pt in point_evaluation.iter_mut() {
        pt.clear();
    }

    for crit in criteria.iter() {
        // println!("Evaluating criteria {:#?}", crit);
        let (quality, weights) =
            evaluate_single(&crit.object, points, logger, generation, flags, cache);

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
        .iter()
        .enumerate()
        .map(|(i, eval)| {
            (
                points[i].0.clone(),
                if eval.is_empty() {
                    1.0
                } else {
                    weighed_mean(eval.iter().map(|v| (v.0.as_f64(), v.1)))
                },
            )
        })
        .collect()
}
