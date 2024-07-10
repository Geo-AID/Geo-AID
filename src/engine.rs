use crate::script::figure::{Figure, Generated};
use crate::script::math::Intermediate;
use std::time::Duration;

/// Random Adjustment Generation Engine - adjustables are randomly adjusted before rating the figure's quality.
pub mod rage;

#[derive(Debug, Clone)]
pub struct GenerateResult {
    pub generated: Generated,
    pub time: Duration,
    pub total_quality: f64,
}

pub trait Engine {
    type Compiled;
    type CompileParams;
    type GenerateParams;

    fn compile(&self, intermediate: &Intermediate, params: Self::CompileParams) -> Self::Compiled;

    fn generate(
        &self,
        compiled: Self::Compiled,
        figure: Figure,
        params: Self::GenerateParams,
    ) -> GenerateResult;
}
