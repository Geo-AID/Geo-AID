use crate::engine::rage::compiler::Compiler;
use crate::engine::rage::generator::critic::FigureProgram;
use crate::engine::rage::generator::program::ValueType;
use crate::engine::rage::generator::Adjustable;
use crate::geometry::{Complex, ValueEnum};
use crate::script::figure::{Figure, Generated};
use crate::script::math::{Entity, Expr, Intermediate};
use std::time::Duration;

pub use self::generator::Generator;

mod compiler;
mod generator;

/// The Random Adjustment Generation Engine.
#[derive(Debug)]
pub struct Rage {
    generator: Generator,
    figure_program: FigureProgram,
}

impl Rage {
    #[must_use]
    pub fn new(worker_count: usize, intermediate: &Intermediate) -> Self {
        let (ev, fig) = Compiler::new(intermediate).compile_programs();

        Self {
            generator: unsafe { Generator::new(worker_count, ev) },
            figure_program: fig,
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

    pub fn get_figure(&mut self, figure: Figure) -> Generated {
        for (c, adj) in self
            .figure_program
            .base
            .constants
            .iter_mut()
            .zip(&self.generator.get_state().adjustables)
        {
            *c = match adj {
                Adjustable::Point(point) => ValueEnum::Complex(*point),
                Adjustable::Real(x) | Adjustable::Clip1D(x) => {
                    ValueEnum::Complex(Complex::real(*x))
                }
            };
        }

        let mut memory = self.figure_program.setup();
        unsafe {
            self.figure_program.calculate(&mut memory);
        }

        let mut variables = Vec::new();
        for ((ty, loc), expr) in self.figure_program.variables.iter().zip(figure.variables) {
            let v = memory[*loc];
            let v = unsafe {
                match ty {
                    ValueType::Complex => ValueEnum::Complex(v.complex),
                    ValueType::Line => ValueEnum::Line(v.line),
                    ValueType::Circle => ValueEnum::Circle(v.circle),
                }
            };
            variables.push(Expr {
                ty: expr.ty,
                kind: expr.kind,
                meta: v,
            });
        }

        let mut entities = Vec::new();
        for ((ty, loc), ent) in self.figure_program.entities.iter().zip(figure.entities) {
            let v = memory[*loc];
            let v = unsafe {
                match ty {
                    ValueType::Complex => ValueEnum::Complex(v.complex),
                    ValueType::Line => ValueEnum::Line(v.line),
                    ValueType::Circle => ValueEnum::Circle(v.circle),
                }
            };
            entities.push(Entity { kind: ent, meta: v });
        }

        Generated {
            variables,
            entities,
            items: figure.items,
        }
    }
}

pub struct GenParams {
    pub max_adjustment: f64,
    pub mean_count: usize,
    pub delta_max_mean: f64,
    pub progress_update: Box<dyn FnMut(f64)>,
}
