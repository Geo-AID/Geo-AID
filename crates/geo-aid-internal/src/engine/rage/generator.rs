//! The generator of Rage, contains all underlying logic
//! of the engine.

use std::{
    collections::VecDeque,
    sync::Arc,
    time::{Duration, Instant},
};

use crate::engine::thread_pool::ThreadPool;
use crate::geometry::Complex;
use crate::script::math::EntityKind;
use geo_aid_math::Func;

mod magic_box;

/// The generation context
struct GenerateContext {
    /// Current best state
    current_state: State,
    /// How much the inputs should be adjusted.
    adjustment_magnitude: f64,
    /// What adjustables the generator is adjusting.
    adjustable_template: Arc<[AdjustableTemplate]>,
    /// The function for computing entity errors
    error_fn: Func,
    /// The exponent used for a generic mean for computing figure quality.
    mean_exponent: f64,
}

/// An attempt at adjusting the state.
fn adjust_and_check(ctx: &mut GenerateContext) {
    magic_box::adjust(
        &mut ctx.current_state,
        ctx.adjustment_magnitude,
        &ctx.adjustable_template,
    );

    #[allow(clippy::cast_precision_loss)]
    let errors_len = ctx.current_state.qualities.len() as f64;
    ctx.error_fn
        .call(&ctx.current_state.inputs, &mut ctx.current_state.qualities);

    // Convert the errors to qualities
    for err in &mut ctx.current_state.qualities {
        *err = (-*err).exp();
    }
    let total_quality = (ctx
        .current_state
        .qualities
        .iter()
        .copied()
        .map(|x| x.powf(ctx.mean_exponent))
        .sum::<f64>()
        / errors_len)
        .powf(ctx.mean_exponent.recip());
    ctx.current_state.total_quality = total_quality;
}

/// A generator state
#[derive(Debug, Clone)]
pub struct State {
    /// Current input values
    pub inputs: Vec<f64>,
    /// Current entity qualities
    pub qualities: Vec<f64>,
    /// Total figure quality
    pub total_quality: f64,
}

/// A structure responsible for generating a figure based on criteria and given points.
pub struct Generator {
    /// The executing thread pool.
    pool: ThreadPool<GenerateContext>,
    /// Current values of all adjustables.
    current_state: State,
    /// A delta of the error in comparison to previous generation.
    delta: f64,
    /// Input count
    input_count: usize,
}

/// The kind of an adjustable (entity)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AdjustableTemplate {
    /// A free point
    Point,
    /// A point described with one real value.
    Clip1d,
    /// A free real
    Real,
}

impl AdjustableTemplate {
    /// Returns `true` if the adjustable template is [`Point`].
    ///
    /// [`Point`]: AdjustableTemplate::Point
    #[must_use]
    pub fn is_point(self) -> bool {
        matches!(self, Self::Point | Self::Clip1d)
    }
}

impl From<&EntityKind> for AdjustableTemplate {
    fn from(value: &EntityKind) -> Self {
        match value {
            EntityKind::FreePoint => AdjustableTemplate::Point,
            EntityKind::PointOnLine { .. } | EntityKind::PointOnCircle { .. } => {
                AdjustableTemplate::Clip1d
            }
            EntityKind::FreeReal | EntityKind::DistanceUnit => AdjustableTemplate::Real,
            EntityKind::Bind(_) => unreachable!(),
        }
    }
}

impl Generator {
    /// Creates a new generator.
    ///
    /// # Panics
    /// Any panic is a bug.
    #[must_use]
    pub fn new(
        params: super::Params,
        input_count: usize,
        error_fn: Func,
        adjustables: &Arc<[AdjustableTemplate]>,
    ) -> Self {
        // Randomize the first state
        let current_state = State {
            inputs: {
                let mut v = Vec::new();
                v.reserve_exact(input_count);
                v.resize_with(input_count, rand::random);
                v
            },
            qualities: {
                let mut v = Vec::new();
                v.reserve_exact(adjustables.len());
                v.resize(adjustables.len(), 0.0);
                v
            },
            total_quality: 0.0,
        };

        // Create the executing thread pool
        let pool = ThreadPool::new(
            params.worker_count,
            move |v| {
                let (temp, state) = v.unwrap();
                GenerateContext {
                    current_state: state,
                    adjustment_magnitude: 0.0,
                    adjustable_template: temp,
                    error_fn,
                    mean_exponent: -params.strictness,
                }
            },
            std::iter::from_fn(|| Some((Arc::clone(adjustables), current_state.clone()))),
            adjust_and_check,
        );

        Self {
            current_state,
            pool,
            delta: 0.0,
            input_count,
        }
    }

    /// Performs a generation cycle with pre-baked magnitudes (how much a point can get adjusted).
    ///
    /// # Returns
    /// The time it took for this cycle to complete.
    ///
    /// # Panics
    /// If there aren't enough magnitudes given
    pub fn cycle_prebaked(&mut self, magnitudes: &[f64]) -> Duration {
        let now = Instant::now();

        let current_state = self.current_state.clone();

        let mut mags = magnitudes.iter().copied();
        self.pool.execute(
            |ctx| {
                if let Some(mag) = mags.next() {
                    ctx.adjustment_magnitude = mag;
                    ctx.current_state.clone_from(&current_state);
                    true
                } else {
                    false
                }
            },
            |ctx| {
                if ctx.current_state.total_quality > self.current_state.total_quality {
                    self.current_state.clone_from(&ctx.current_state);
                }
            },
        );

        now.elapsed()
    }

    /// Prepares adjustment magnitudes for each sample (adjustment attempt) based on the max magnitude.
    #[must_use]
    pub fn bake_magnitudes(&self, maximum_adjustment: f64) -> Vec<f64> {
        #[allow(clippy::cast_precision_loss)]
        let step = maximum_adjustment / self.input_count as f64;

        let mut magnitudes = Vec::new();
        let mut first = step;
        for _ in 0..self.input_count {
            magnitudes.push(first);
            first += step;
        }

        magnitudes
    }

    /// Performs generation cycles until the mean delta from the last `mean_count` deltas becomes less or equal to `max_mean`.
    /// Executes `cyclic` after the end of each cycle.
    ///
    /// # Returns
    /// The time it took to generate the figure.
    ///
    /// # Panics
    /// If there are multithreading issues (there has been a panic in one of the generation threads).
    pub fn cycle_until_mean_delta<P: FnMut(f64)>(
        &mut self,
        maximum_adjustment: f64,
        mean_count: usize,
        max_mean: f64,
        mut cyclic: P,
    ) -> Duration {
        let magnitudes = self.bake_magnitudes(maximum_adjustment);
        let mut last_deltas = VecDeque::new();

        let mut current_quality = 0.0;
        let mut mean_delta = 1.0;

        last_deltas.resize(mean_count, 1.0);

        #[allow(clippy::cast_precision_loss)]
        let mean_count_f = mean_count as f64;

        let mut duration = Duration::new(0, 0);

        while mean_delta > max_mean {
            duration += self.cycle_prebaked(&magnitudes);

            self.delta = self.get_total_quality() - current_quality;
            current_quality = self.get_total_quality();
            let dropped_delta = last_deltas.pop_front().unwrap();
            mean_delta = (mean_delta * mean_count_f - dropped_delta + self.delta) / mean_count_f;
            last_deltas.push_back(self.delta);

            cyclic(current_quality);
        }

        duration
    }

    /// Get the current best state.
    #[must_use]
    pub fn get_state(&self) -> &State {
        &self.current_state
    }

    /// Get the current state's quality.
    #[must_use]
    pub fn get_total_quality(&self) -> f64 {
        self.current_state.total_quality
    }
}
