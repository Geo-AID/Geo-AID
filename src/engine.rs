use crate::{geometry::ValueEnum, script::math::Intermediate};

/// Random Adjustment Generation Engine - adjustables are randomly adjusted before rating the figure's quality.
pub mod rage;

pub trait Engine {
    type Compiled;
    type CompileParams;
    type GenerateParams;

    fn compile(&self, intermediate: &Intermediate, params: Self::CompileParams) -> Self::Compiled;

    fn generate(&self, compiled: Self::Compiled, params: Self::GenerateParams) -> Vec<ValueEnum>;
}