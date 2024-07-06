use std::time::Duration;
use crate::script::math::Intermediate;
use crate::script::figure::{Figure, Generated};

/// Random Adjustment Generation Engine - adjustables are randomly adjusted before rating the figure's quality.
pub mod rage;

#[derive(Debug, Clone)]
pub struct GenerateResult {
    pub generated: Generated,
    pub time: Duration,
    pub total_quality: f64
}

pub trait Engine {
    type Compiled;
    type CompileParams;
    type GenerateParams;

    fn compile(&self, intermediate: &Intermediate, params: Self::CompileParams) -> Self::Compiled;

    fn generate(&self, compiled: Self::Compiled, figure: Figure, params: Self::GenerateParams) -> GenerateResult;
}