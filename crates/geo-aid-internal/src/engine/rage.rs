use crate::engine::rage::generator::AdjustableTemplate;
use crate::script::figure::Generated;
use crate::script::math::Intermediate;
use std::time::Duration;
#[allow(unused_imports)]
use geo_aid_math::Func;
use crate::engine::compiler::{Compiled, FigureFn};
pub use self::generator::Generator;

mod generator;

/// The Random Adjustment Generation Engine.
pub struct Rage {
    generator: Generator,
    figure_fn: FigureFn,
    // rule_fn: Func,
    // rule_count: usize,
}

impl Rage {
    #[must_use]
    pub fn new(worker_count: usize, strictness: f64, intermediate: &Intermediate) -> Self {
        let Compiled {
            context,
            errors,
            figure_fn,
            input_count,
            ..
        } = super::compiler::compile(intermediate);

        let error_fn = context.compute(errors.iter().copied());
        let adjustables: Vec<_> = intermediate.adjusted.entities
            .iter().map(AdjustableTemplate::from).collect();

        // let rule_count = rule_errors.len();
        // let rule_fn = context.compute(rule_errors.into_iter());

        // let mut dst = [1.0, 1.0];
        // error_fn.call(&[0.0, 0.0, 2.0, 2.0], &mut dst);
        // println!("Works now: {dst:?}");

        Self {
            generator: unsafe {
                Generator::new(
                    worker_count,
                    input_count,
                    error_fn,
                    strictness,
                    &adjustables.into(),
                )
            },
            figure_fn,
            // rule_fn,
            // rule_count,
        }
    }

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

    pub fn get_figure(&mut self) -> Generated {
        let inputs = self.generator.get_state();
        // let mut rule_qs = vec![0.0; self.rule_count];
        // self.rule_fn.call(&inputs.inputs, &mut rule_qs);
        // println!("Rule qualities: {:?}", rule_qs);
        (self.figure_fn)(&inputs.inputs)
    }
}

pub struct GenParams {
    pub max_adjustment: f64,
    pub mean_count: usize,
    pub delta_max_mean: f64,
    pub progress_update: Box<dyn FnMut(f64)>,
}
