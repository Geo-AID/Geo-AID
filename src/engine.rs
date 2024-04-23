use crate::{generator::program::ValueEnum, script::math::Intermediate};

pub trait Engine {
    type Compiled;

    fn compile(&self, intermediate: &Intermediate) -> Self::Compiled;

    fn generate(&self, compiled: Self::Compiled) -> Vec<ValueEnum>;
}