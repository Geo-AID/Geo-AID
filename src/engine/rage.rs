use crate::engine::rage::compiler::Compiler;
use crate::engine::rage::generator::Adjustable;
use crate::engine::rage::generator::critic::{EvaluateProgram, FigureProgram};
use crate::engine::rage::generator::program::ValueType;
use crate::geometry::{Complex, ValueEnum};
use crate::script::figure::{Figure, Generated};
use crate::script::math::{Entity, Expr, Intermediate};

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

    fn generate(&self, compiled: Self::Compiled, figure: Figure, params: Self::GenerateParams) -> GenerateResult {
        let mut gen = unsafe {
            Generator::new(self.worker_count, compiled.0)
        };

        let time = gen.cycle_until_mean_delta(
            params.max_adjustment,
            params.mean_count,
            params.delta_max_mean,
            params.progress_update,
        );

        let mut figure_prog = compiled.1;
        for  (c, adj) in figure_prog.base.constants.iter_mut().zip(&gen.get_state().adjustables) {
            *c = match adj {
                Adjustable::Point(point) => ValueEnum::Complex(*point),
                Adjustable::Real(x)
                | Adjustable::Clip1D(x) => ValueEnum::Complex(Complex::real(*x))
            };
        }

        let mut memory = figure_prog.setup();
        unsafe {
            figure_prog.calculate(&mut memory);
        }

        let mut variables = Vec::new();
        for ((ty, loc), expr) in figure_prog.variables.into_iter().zip(figure.variables) {
            let v = memory[loc];
            let v = unsafe {
                match ty {
                    ValueType::Complex => ValueEnum::Complex(v.complex),
                    ValueType::Line => ValueEnum::Line(v.line),
                    ValueType::Circle => ValueEnum::Circle(v.circle)
                }
            };
            variables.push(Expr {
                ty: expr.ty,
                kind: expr.kind,
                meta: v
            });
        }

        let mut entities = Vec::new();
        for ((ty, loc), ent) in figure_prog.entities.into_iter().zip(figure.entities) {
            let v = memory[loc];
            let v = unsafe {
                match ty {
                    ValueType::Complex => ValueEnum::Complex(v.complex),
                    ValueType::Line => ValueEnum::Line(v.line),
                    ValueType::Circle => ValueEnum::Circle(v.circle)
                }
            };
            entities.push(Entity {
                kind: ent,
                meta: v
            });
        }

        GenerateResult {
            generated: Generated {
                entities,
                variables,
                items: figure.items,
            },
            time,
            total_quality: gen.get_total_quality()
        }
    }
}