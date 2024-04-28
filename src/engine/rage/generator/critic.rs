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

use serde::Serialize;
use crate::engine::rage::generator::program::{Loc, ValueType};
use crate::geometry::Complex;

use super::{program::{Program, Execute, Value}, AdjustableTemplate};

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
    pub weights: Vec<f64>
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
        for x in evaluation {
            *x = 0.0;
        }

        // Create the weight iterator.
        let mut weight_it = self.weights.iter().copied();

        // Execute the base program.
        self.base.execute(memory);

        // Calculate adjustable qualities (weighed mean). Qualities are expected to be normalized.
        for rule in 0..self.rule_count {
            let quality = memory[rule].complex.real;

            for adj in 0..self.adjustables.len() {
                evaluation[adj] += quality * weight_it.next().unwrap();
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
    pub variables: Vec<(ValueType, Loc)>
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
