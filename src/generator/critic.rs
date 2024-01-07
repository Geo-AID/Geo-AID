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

use std::{cell::RefCell, collections::HashMap, sync::Arc};

use crate::script::{Criteria, CriteriaKind};

use super::{expression::ExprCache, Adjustable, Flags, fast_float::FastFloat};

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

// fn weighed_mean<I: Iterator<Item = (f64, f64)>>(it: I) -> f64 {
//     let mut sum = 0.0;
//     let mut weight_sum = 0.0;

//     for (value, weight) in it {
//         sum += value * weight;
//         weight_sum += weight;
//     }

//     sum / weight_sum
// }

/// Inverts the quality. As simple as 1 - q
#[inline]
fn invert(q: f64) -> f64 {
    1.0 - q
}

pub type Cache = HashMap<usize, ExprCache>;

pub struct EvaluationArgs<'r> {
    pub adjustables: &'r [(Adjustable, f64)],
    pub generation: u64,
    pub flags: &'r Arc<Flags>,
    pub cache: Option<&'r RefCell<Cache>>,
}

/// Evaluates a single rule in terms of quality.
fn evaluate_single(
    crit: &CriteriaKind,
    adjustables: &AdjustableVec,
    generation: u64,
    flags: &Arc<Flags>,
    cache: Option<&RefCell<Cache>>,
) -> f64 {
    let args = EvaluationArgs {
        adjustables,
        generation,
        flags,
        cache,
    };

    match crit {
        CriteriaKind::EqualScalar(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            let diff = v1 - v2;
            // Interestingly, it's easier to calculate the quality function for != and then invert it.
            invert(smooth_0_inf(1130.0 * diff.powi(2)))
        }
        CriteriaKind::EqualPoint(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

                let diff = (v1 - v2).mangitude();
                // Interestingly, it's easier to calculate the quality function for != and then invert it.
                invert(smooth_0_inf(1130.0 * diff.powi(2)))
        }
        CriteriaKind::Less(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
            let (v1, v2) = (v1, v2);
            let diff = (v1 - v2) / v1.abs();
            // logger.push(format!("Distance is {}", v1.0.real));
            // logger.push(format!("Diff's {diff}"));
            smooth_inf_inf(-54.0 * f64::cbrt(diff + 0.001))
        }
        CriteriaKind::Greater(e1, e2) => {
            let v1 = e1.evaluate(&args);
            let v2 = e2.evaluate(&args);

            // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
            let (v1, v2) = (v1, v2);
            let diff = (v1 - v2) / v1.abs();
            // println!("{v1} > {v2} Satisfied with {}", smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001)));
            smooth_inf_inf(54.0 * f64::cbrt(diff - 0.001))
        }
        CriteriaKind::Inverse(kind) => {
            invert(evaluate_single(kind, adjustables, generation, flags, cache))
        }
        CriteriaKind::Bias(_) => 1.0,
        CriteriaKind::Alternative(rules) => {
            rules
                .iter()
                .map(|crit| evaluate_single(crit, adjustables, generation, flags, cache))
                .reduce(f64::max)
                .unwrap()
        }
    }
}

#[allow(clippy::implicit_hasher)]
/// Evaluates all rules in terms of quality
pub fn evaluate(
    points: &AdjustableVec,
    criteria: &Arc<Vec<Criteria>>,
    generation: u64,
    flags: &Arc<Flags>,
    cache: Option<&RefCell<Cache>>,
    // Rule-related weights for each adjustable
    weights: &[Vec<FastFloat>],
    evaluation: &mut [Vec<f64>],
) -> AdjustableVec {
    for pt in &mut *evaluation {
        pt.clear();
    }

    for (i, crit) in criteria.iter().enumerate() {
        // println!("Evaluating criteria {:#?}", crit);
        let quality =
            evaluate_single(crit.get_kind(), points, generation, flags, cache);

        for (adj, weights) in evaluation.iter_mut().zip(weights) {
            let weight = weights[i];

            if !weight.is_zero() {
                adj.push(quality * weight.to_f64());
            }
        }
    }

    // Find the final evaluation
    evaluation
        .iter()
        .enumerate()
        .map(|(i, eval)| {
            (
                points[i].0,
                if eval.is_empty() {
                    1.0
                } else {
                    eval.iter().copied().sum()
                },
            )
        })
        .collect()
}
