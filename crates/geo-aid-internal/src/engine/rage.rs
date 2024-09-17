//! # Random Adjustment Generation Engine.
//!
//! In a nutshell, it improves the figure by randomly adjusting all inputs
//! and checking if the new figure error is smaller.

pub use self::generator::Generator;
use crate::engine::compiler::{Compiled, FigureFn};
use crate::engine::rage::generator::AdjustableTemplate;
use crate::script::figure::Generated;
use crate::script::math::Intermediate;
#[allow(unused_imports)]
use geo_aid_math::Func;
use std::time::Duration;

mod generator;

/// The Random Adjustment Generation Enginei runtime.
pub struct Rage {
    /// The underlying generation
    generator: Generator,
    /// The figure function
    figure_fn: FigureFn,
    // rule_fn: Func,
    // rule_count: usize,
}

/// The engine's generation params
#[derive(Clone, Copy)]
pub struct Params {
    /// How strictly the rules are applied.
    pub strictness: f64,
    /// How many samples to use per generation steps
    /// (how many different adjustments to make from a single base)
    pub samples: usize,
    /// How many threads to use
    pub worker_count: usize,
}

impl Rage {
    #[must_use]
    pub fn new(params: Params, intermediate: &Intermediate) -> Self {
        let Compiled {
            context,
            errors,
            figure_fn,
            input_count,
            ..
        } = super::compiler::compile(intermediate);

        let error_fn = context.compute(errors.iter().copied());
        let adjustables: Vec<_> = intermediate
            .adjusted
            .entities
            .iter()
            .map(AdjustableTemplate::from)
            .collect();

        // let rule_count = rule_errors.len();
        // let rule_fn = context.compute(rule_errors.into_iter());

        // let mut dst = [1.0, 1.0];
        // error_fn.call(&[0.0, 0.0, 2.0, 2.0], &mut dst);
        // println!("Works now: {dst:?}");

        Self {
            generator: Generator::new(params, input_count, error_fn, &adjustables.into()),
            figure_fn,
            // rule_fn,
            // rule_count,
        }
    }

    /// Generate with last deltas mean as a stop condition.
    pub fn generate_mean_delta(&mut self, params: GenParams) -> Duration {
        self.generator.cycle_until_mean_delta(
            params.max_adjustment,
            params.mean_count,
            params.delta_max_mean,
            params.progress_update,
        )
    }

    /// Returns the underlying generator instance (separated mostly for legacy reasons)
    #[must_use]
    pub fn gen(&self) -> &Generator {
        &self.generator
    }

    /// Returns the underlying generator instance (separated mostly for legacy reasons)
    #[must_use]
    pub fn gen_mut(&mut self) -> &mut Generator {
        &mut self.generator
    }

    /// Get the figure based on the current best state.
    pub fn get_figure(&mut self) -> Generated {
        let inputs = self.generator.get_state();
        // let mut rule_qs = vec![0.0; self.rule_count];
        // self.rule_fn.call(&inputs.inputs, &mut rule_qs);
        // println!("Rule qualities: {:?}", rule_qs);
        (self.figure_fn)(&inputs.inputs)
    }
}

/// The generation params
pub struct GenParams {
    /// The maximal adjustment of figure inputs.
    pub max_adjustment: f64,
    /// The amount of last quality deltas to include in
    /// mean calculation
    pub mean_count: usize,
    /// If the arithemtic mean of the last `mean_count` quality deltas ever
    /// goes below this, generation is stopped
    pub delta_max_mean: f64,
    /// Ran every generation step.
    pub progress_update: Box<dyn FnMut(f64)>,
}
