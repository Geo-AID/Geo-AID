use std::{
    collections::VecDeque,
    mem,
    pin::Pin,
    sync::{mpsc, Arc},
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use serde::Serialize;
use geo_aid_math::Func;
use crate::geometry::Complex;
use crate::script::math::EntityKind;

pub mod fast_float;
mod magic_box;

pub enum Message {
    Generate(f64),
    Terminate,
}

#[derive(Debug)]
struct SendPtr<T: ?Sized>(*const T);

impl<T: ?Sized> Clone for SendPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for SendPtr<T> {}

unsafe impl<T: ?Sized> Send for SendPtr<T> {}

unsafe fn generation_cycle(
    receiver: &mpsc::Receiver<Message>,
    sender: &mpsc::Sender<CycleState>,
    adjustables: &[AdjustableTemplate],
    input_count: usize,
    current_state: SendPtr<State>,
    mean_exponent: f64,
    error_fn: Func,
) {
    let mut inputs = Vec::new();
    inputs.reserve_exact(input_count);
    inputs.resize(input_count, 0.0);
    let mut errors = Vec::new();
    errors.reserve_exact(adjustables.len());
    errors.resize(adjustables.len(), 0.0);
    #[allow(clippy::cast_precision_loss)]
    let errors_len = errors.len() as f64;

    // A complete cycle is the following.
    // 1. Receive adjustment information.
    // 2. Adjust the current state onto the thread state.
    // 3. Evaluate adjustables' qualities.
    // 4. Send back thread's state.

    loop {
        match receiver.recv().unwrap() {
            Message::Generate(adjustment_magnitude) => {
                {
                    magic_box::adjust(
                        &*current_state.0,
                        &mut inputs,
                        adjustment_magnitude,
                        adjustables,
                    );
                }

                error_fn.call(&inputs, &mut errors);

                // Convert the errors to qualities
                for err in &mut errors {
                    *err = (-*err).exp();
                }
                let total_quality = (
                    errors
                        .iter()
                        .copied()
                        .map(|x| x.powf(mean_exponent)).sum::<f64>() / errors_len
                ).powf(mean_exponent.recip());

                sender
                    .send(CycleState {
                        inputs: SendPtr(inputs.as_slice()),
                        qualities: SendPtr(errors.as_slice()),
                        total_quality,
                    })
                    .unwrap();
            }
            Message::Terminate => return,
        }
    }
}

#[derive(Debug, Clone)]
pub struct State {
    pub inputs: Vec<f64>,
    pub qualities: Vec<f64>,
    pub total_quality: f64,
}

#[derive(Clone, Copy)]
struct CycleState {
    pub inputs: SendPtr<[f64]>,
    pub qualities: SendPtr<[f64]>,
    pub total_quality: f64,
}

#[derive(Debug)]
/// A structure responsible for generating a figure based on criteria and given points.
pub struct Generator {
    /// Current values of all adjustables.
    current_state: Pin<Box<State>>,
    /// All the workers (generation cycles).
    workers: Vec<JoinHandle<()>>,
    /// Senders for the workers
    senders: Vec<mpsc::Sender<Message>>,
    /// The receiver for adjusted points from each generation cycle.
    receiver: mpsc::Receiver<CycleState>,
    /// A delta of the error in comparison to previous generation.
    delta: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum AdjustableTemplate {
    Point,
    Clip1d,
    Real,
}

impl AdjustableTemplate {
    /// Returns `true` if the adjustable template is [`Point`].
    ///
    /// [`Point`]: AdjustableTemplate::Point
    #[must_use]
    pub fn is_point(self) -> bool {
        matches!(self, Self::Point | Self::Clip1d)
    }
}

impl From<&EntityKind> for AdjustableTemplate {
    fn from(value: &EntityKind) -> Self {
        match value {
            EntityKind::FreePoint => AdjustableTemplate::Point,
            EntityKind::PointOnLine { .. }
            | EntityKind::PointOnCircle { .. } => AdjustableTemplate::Clip1d,
            EntityKind::FreeReal | EntityKind::DistanceUnit => AdjustableTemplate::Real,
            EntityKind::Bind(_) => unreachable!(),
        }
    }
}

impl Generator {
    /// # Safety
    /// The `program` MUST be safe.
    #[must_use]
    pub unsafe fn new(workers: usize, input_count: usize, error_fn: Func, strictness: f64, adjustables: &Arc<[AdjustableTemplate]>) -> Self {
        let (input_senders, input_receivers): (
            Vec<mpsc::Sender<Message>>,
            Vec<mpsc::Receiver<Message>>,
        ) = (0..workers).map(|_| mpsc::channel()).unzip();

        let (output_sender, output_receiver) = mpsc::channel();

        let current_state = State {
            inputs: {
                let mut v = Vec::new();
                v.reserve_exact(input_count);
                v.resize_with(input_count, rand::random);
                v
            },
            qualities: {
                let mut v = Vec::new();
                v.reserve_exact(adjustables.len());
                v.resize(adjustables.len(), 0.0);
                v
            },
            total_quality: 0.0,
        };
        let current_state = Box::pin(current_state);
        let state_ptr: &State = &current_state;
        let state_ptr = SendPtr(state_ptr);

        Self {
            current_state,
            workers: input_receivers
                .into_iter()
                .map(|rec| {
                    let sender = mpsc::Sender::clone(&output_sender);
                    let adjustables = Arc::clone(&adjustables);
                    thread::spawn(move || unsafe {
                        generation_cycle(
                            &rec, &sender,
                            &adjustables, input_count,
                            state_ptr, -strictness,
                            error_fn,
                        );
                    })
                })
                .collect(),
            senders: input_senders,
            receiver: output_receiver,
            delta: 0.0,
        }
    }

    /// Performs a generation cycle with pre-baked magnitudes (how much a point can get adjusted).
    ///
    /// # Returns
    /// The time it took for this cycle to complete.
    ///
    /// # Panics
    /// If there aren't enough magnitudes given
    pub fn cycle_prebaked(&mut self, magnitudes: &[f64]) -> Duration {
        let now = Instant::now();

        // Send data to each worker
        for (i, sender) in self.senders.iter().enumerate() {
            sender.send(Message::Generate(magnitudes[i])).unwrap();
        }

        let first_state = self.receiver.recv().unwrap();
        let mut best = first_state;

        // Wait for each handle to finish and consider its outcome
        for _ in 0..(self.workers.len() - 1) {
            // If the total quality is larger than the current total, replace the points.
            let state = self.receiver.recv().unwrap();

            if state.total_quality > best.total_quality {
                best = state;
            }
        }

        if best.total_quality > self.get_total_quality() {
            // println!("Success!");
            unsafe {
                let mut_ref = self.current_state.as_mut().get_unchecked_mut();
                mut_ref.qualities.copy_from_slice(&*best.qualities.0);
                mut_ref.inputs.copy_from_slice(&*best.inputs.0);
                mut_ref.total_quality = best.total_quality;
            }
        }

        now.elapsed()
    }

    #[must_use]
    pub fn bake_magnitudes(&self, maximum_adjustment: f64) -> Vec<f64> {
        #[allow(clippy::cast_precision_loss)]
        let step = maximum_adjustment / self.workers.len() as f64;

        let mut magnitudes = Vec::new();
        let mut first = step;
        for _ in 0..self.workers.len() {
            magnitudes.push(first);
            first += step;
        }

        magnitudes
    }

    // pub fn single_cycle(&mut self, maximum_adjustment: f64) {
    //     self.cycle_prebaked(&self.bake_magnitudes(maximum_adjustment));
    //
    //     self.delta = self.get_total_quality();
    // }

    /// Performs generation cycles until the mean delta from the last `mean_count` deltas becomes less or equal to `max_mean`.
    /// Executes `cyclic` after the end of each cycle.
    ///
    /// # Returns
    /// The time it took to generate the figure.
    ///
    /// # Panics
    /// If there are multithreading issues (there has been a panic in one of the generation threads).
    pub fn cycle_until_mean_delta<P: FnMut(f64)>(
        &mut self,
        maximum_adjustment: f64,
        mean_count: usize,
        max_mean: f64,
        mut cyclic: P,
    ) -> Duration {
        let magnitudes = self.bake_magnitudes(maximum_adjustment);
        let mut last_deltas = VecDeque::new();

        let mut current_quality = 0.0;
        let mut mean_delta = 1.0;

        last_deltas.resize(mean_count, 1.0);

        #[allow(clippy::cast_precision_loss)]
        let mean_count_f = mean_count as f64;

        let mut duration = Duration::new(0, 0);

        while mean_delta > max_mean {
            duration += self.cycle_prebaked(&magnitudes);

            self.delta = self.get_total_quality() - current_quality;
            current_quality = self.get_total_quality();
            let dropped_delta = last_deltas.pop_front().unwrap();
            mean_delta = (mean_delta * mean_count_f - dropped_delta + self.delta) / mean_count_f;
            last_deltas.push_back(self.delta);

            cyclic(current_quality);
        }

        duration
    }

    #[must_use]
    pub fn get_state(&self) -> &State {
        &self.current_state
    }

    // #[must_use]
    // pub fn get_delta(&self) -> f64 {
    //     self.delta
    // }

    #[must_use]
    pub fn get_total_quality(&self) -> f64 {
        self.current_state.total_quality
    }
}

impl Drop for Generator {
    fn drop(&mut self) {
        for sender in &mut self.senders {
            sender.send(Message::Terminate).unwrap();
        }

        let workers = mem::take(&mut self.workers);

        for worker in workers {
            worker.join().unwrap();
        }
    }
}
