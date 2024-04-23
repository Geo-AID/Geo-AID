use crate::geometry::ValueEnum;

use self::generator::Generator;

use super::Engine;

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

impl Engine for Rage {
    type Compiled = ();
    type CompileParams = ();
    type GenerateParams = ();

    fn compile(&self, intermediate: &crate::script::math::Intermediate, params: Self::CompileParams) -> Self::Compiled {
        todo!()
    }

    fn generate(&self, compiled: Self::Compiled, params: Self::GenerateParams) -> Vec<ValueEnum> {
        let mut gen = unsafe {
            Generator::new(self.worker_count, todo!())
        };

        let duration = gen.cycle_until_mean_delta(
            args.adjustment_max,
            args.mean_count,
            args.delta_max_mean,
            |quality| {
                stdout
                    .queue(terminal::Clear(terminal::ClearType::FromCursorDown))
                    .unwrap();
    
                stdout.queue(cursor::SavePosition).unwrap();
                stdout
                    .write_all(format!("Quality: {:.2}% ", quality * 100.0).as_bytes())
                    .unwrap();
                stdout.queue(cursor::RestorePosition).unwrap();
                stdout.flush().unwrap();
            },
        );
    }
}