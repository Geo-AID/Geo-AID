use crate::engine::rage::generator::program::{Loc, ValueType};
use crate::geometry::Complex;
use serde::Serialize;

use super::{
    program::{Execute, Program, Value},
    AdjustableTemplate,
};

/// Program used for evaluation
///
/// # Safety
/// The program is considered safe iff:
/// * the `base` is a safe program.
/// * After the 'constants' block there are `rule_count` registers holding respective rule qualities and the end of execution.
#[derive(Debug, Clone, Serialize)]
pub struct EvaluateProgram {
    /// Program base.
    pub base: Program,
    /// Templated adjustables adjusted by the generator.
    pub adjustables: Vec<AdjustableTemplate>,
    /// How many rules are to be used in the generation process.
    pub rule_count: usize,
    /// A vec of biases to fill when preparing.
    pub biases: Vec<Loc>,
    /// How much each rule quality influences each adjustable.
    /// Constants take up the first registers in memory. Weights should be normalized.
    /// Do note that the first constants are always the adjustables.
    /// `rule_count` registers after that are expected to contain rule qualities.
    /// Everything beyond that point is active program memory with no layout guarantees.
    /// Weight of rule `i` on adjustable `j` is at index `i * adjustables.len() + j`.
    pub weights: Vec<f64>,
}

impl EvaluateProgram {
    /// Prepares the memory layout for execution.
    pub fn setup(&self) -> Vec<Value> {
        let mut memory = self.base.setup();

        for bias in &self.biases {
            memory[*bias].complex = Complex::real(1.0);
        }

        memory
    }

    /// Evaluates the qualities of adjustables.
    ///
    /// # Safety
    /// The program must be valid.
    pub unsafe fn evaluate(&self, memory: &mut [Value], evaluation: &mut [f64]) {
        // Clear the current evaluation.
        for x in evaluation.iter_mut() {
            *x = 0.0;
        }

        // Create the weight iterator.
        let mut weight_it = self.weights.iter().copied();

        // Execute the base program.
        self.base.execute(memory);

        // Calculate adjustable qualities (weighed mean). Qualities are expected to be normalized.
        for q in memory
            .iter()
            .skip(self.base.constants.len())
            .take(self.rule_count)
        {
            let quality = q.complex.real;

            for ev in evaluation.iter_mut().take(self.adjustables.len()) {
                *ev += quality * weight_it.next().unwrap();
            }
        }
    }
}

/// Program used for figure preparation
///
/// # Safety
/// The program is considered safe iff:
/// * the `base` is a safe program.
#[derive(Debug, Clone, Serialize)]
pub struct FigureProgram {
    /// Program base.
    pub base: Program,
    /// Expression values.
    pub variables: Vec<(ValueType, Loc)>,
    /// Entity values.
    pub entities: Vec<(ValueType, Loc)>,
}

impl FigureProgram {
    /// Prepares the memory layout for execution.
    pub fn setup(&self) -> Vec<Value> {
        self.base.setup()
    }

    /// Calculates values of the right expressions.
    ///
    /// # Safety
    /// The program must be valid.
    pub unsafe fn calculate(&self, memory: &mut [Value]) {
        // Execute the base program.
        self.base.execute(memory);
    }
}
