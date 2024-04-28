/*
 Copyright (c) 2024 Michał Wilczek, Michał Margos
 
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

use crate::engine::rage::compiler::Compiler;
use crate::engine::rage::generator::Adjustable;
use crate::engine::rage::generator::critic::{EvaluateProgram, FigureProgram};
use crate::engine::rage::generator::program::ValueType;
use crate::geometry::{Complex, ValueEnum};
use crate::script::math::Intermediate;

use self::generator::Generator;

use super::{Engine, GenerateResult};

mod compiler;
mod generator;

/// The Random Adjustment Generation Engine.
#[derive(Debug)]
pub struct Rage {
    worker_count: usize
}

impl Rage {
    #[must_use]
    pub fn new(worker_count: usize) -> Self {
        Self {
            worker_count
        }
    }
}

pub struct GenParams {
    pub max_adjustment: f64,
    pub mean_count: usize,
    pub delta_max_mean: f64,
    pub progress_update: Box<dyn FnMut(f64)>
}

impl Engine for Rage {
    type Compiled = (EvaluateProgram, FigureProgram);
    type CompileParams = ();
    type GenerateParams = GenParams;

    fn compile(&self, intermediate: &Intermediate, _params: Self::CompileParams) -> Self::Compiled {
        Compiler::new(intermediate).compile_programs()
    }

    fn generate(&self, compiled: Self::Compiled, params: Self::GenerateParams) -> GenerateResult {
        let mut gen = unsafe {
            Generator::new(self.worker_count, compiled.0)
        };

        let time = gen.cycle_until_mean_delta(
            params.max_adjustment,
            params.mean_count,
            params.delta_max_mean,
            params.progress_update,
        );

        let mut figure = compiled.1;
        for  (c, adj) in figure.base.constants.iter_mut().zip(&gen.get_state().adjustables) {
            *c = match adj {
                Adjustable::Point(point) => ValueEnum::Complex(*point),
                Adjustable::Real(x)
                | Adjustable::Clip1D(x) => ValueEnum::Complex(Complex::real(*x))
            };
        }

        let mut memory = figure.setup();
        unsafe {
            figure.calculate(&mut memory);
        }

        let mut values = Vec::new();
        for (ty, loc) in figure.variables {
            let v = memory[loc];
            let v = unsafe {
                match ty {
                    ValueType::Complex => ValueEnum::Complex(v.complex),
                    ValueType::Line => ValueEnum::Line(v.line),
                    ValueType::Circle => ValueEnum::Circle(v.circle)
                }
            };
            values.push(v);
        }

        GenerateResult {
            values,
            time,
            total_quality: gen.get_total_quality()
        }
    }
}