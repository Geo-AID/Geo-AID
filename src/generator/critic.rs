/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use crate::generator::fast_float::FastFloat;
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

/// Inverts the quality. As simple as 1 - q
#[inline]
fn invert(q: f64) -> f64 {
    1.0 - q
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
) -> (f64, Vec<FastFloat>) {
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

            let weights = e1.weights.clone() + &e2.weights;

            (
                {
                    let diff = v1 - v2;
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    invert(smooth_0_inf(1130.0 * diff.powi(2)))
                },
                weights.0,
            )
        }
        CriteriaKind::EqualPoint(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let weights = e1.weights.clone() + &e2.weights;

            (
                {
                    let diff = (v1 - v2).mangitude();
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    invert(smooth_0_inf(1130.0 * diff.powi(2)))
                },
                weights.0,
            )
        }
        CriteriaKind::Less(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let weights = e1.weights.clone() + &e2.weights;

            (
                {
                    // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
                    let (v1, v2) = (v1, v2);
                    let diff = (v1 - v2) / v1.abs();
                    // logger.push(format!("Distance is {}", v1.0.real));
                    // logger.push(format!("Diff's {diff}"));
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    smooth_inf_inf(-54.0 * f64::cbrt(diff + 0.001))
                },
                weights.0,
            )
        }
        CriteriaKind::Greater(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let weights = e1.weights.clone() + &e2.weights;

            (
                {
                    // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
                    let (v1, v2) = (v1, v2);
                    let diff = (v1 - v2) / v1.abs();
                    // println!("{v1} > {v2} Satisfied with {}", smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001)));
                    // Interestingly, it's easier to calculate the quality function for != and then invert it.
                    smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001))
                },
                weights.0,
            )
        }
        // Note: Inverted zero quality is still zero.
        CriteriaKind::Inverse(kind) => {
            let (quality, ws) =
                evaluate_single(kind, adjustables, args.logger, generation, flags, cache);
            (invert(quality), ws)
        }
        CriteriaKind::Bias(expr) => (1.0, expr.weights.clone().0),
        CriteriaKind::Alternative(rules) => {
            let mut evaluated = rules
                .iter()
                .map(|x| evaluate_single(&x.object, adjustables, logger, generation, flags, cache));

            let mut max = evaluated.next().unwrap();

            for pair in evaluated {
                if pair.0 > max.0 {
                    max = pair;
                }
            }

            max
        }
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
    point_evaluation: &mut [Vec<(f64, FastFloat)>],
) -> AdjustableVec {
    for pt in &mut *point_evaluation {
        pt.clear();
    }

    for crit in criteria.iter() {
        // println!("Evaluating criteria {:#?}", crit);
        let (quality, weights) =
            evaluate_single(&crit.object, points, logger, generation, flags, cache);

        // println!("Evaluation result: {quality}, {:?}", weights);

        // Normalize weights (squeeze to [0, 1]) and add (quality, weight) pairs to the points
        let weight_sum = weights.iter().copied().sum::<FastFloat>();
        for (i, weight) in weights.into_iter().enumerate() {
            if !weight.is_zero() {
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
                    weighed_mean(eval.iter().map(|v| (v.0, v.1.to_f64())))
                },
            )
        })
        .collect()
}
