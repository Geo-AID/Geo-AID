use std::time::Duration;
use crate::{geometry::ValueEnum, script::math::Intermediate};

/// Random Adjustment Generation Engine - adjustables are randomly adjusted before rating the figure's quality.
pub mod rage;

#[derive(Debug, Clone)]
pub struct GenerateResult {
    values: Vec<ValueEnum>,
    time: Duration,
    total_quality: f64
}

pub trait Engine {
    type Compiled;
    type CompileParams;
    type GenerateParams;

    fn compile(&self, intermediate: &Intermediate, params: Self::CompileParams) -> Self::Compiled;

    fn generate(&self, compiled: Self::Compiled, params: Self::GenerateParams) -> GenerateResult;
}