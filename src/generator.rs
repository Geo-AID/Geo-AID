use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt::Display,
    mem,
    ops::{Add, AddAssign, Div, Mul, Neg, Sub, SubAssign},
    sync::{mpsc, Arc},
    thread::{self, JoinHandle},
    time::{Duration, Instant}
};

use serde::Serialize;

use crate::script::Criteria;

use self::expression::{ExprCache, Value};

#[derive(Debug)]
pub enum EvaluationError {
    ParallelLines,
}

pub mod critic;
pub mod expression;
pub mod geometry;
mod magic_box;

/// Represents a complex number located on a "unit" plane.
#[derive(Debug, Clone, Copy, Serialize)]
pub struct Complex {
    /// X coordinate located in range [0, 1).
    pub real: f64,
    /// Y coordinate located in range [0, 1).
    pub imaginary: f64,
}

impl Complex {
    #[must_use]
    pub fn new(real: f64, imaginary: f64) -> Self {
        Self { real, imaginary }
    }

    #[must_use]
    pub fn zero() -> Self {
        Self::new(0.0, 0.0)
    }

    #[must_use]
    pub fn i() -> Self {
        Self::new(0.0, 1.0)
    }

    /// Optimized multiplication by the complex unit (i).
    #[must_use]
    pub fn mul_i(self) -> Complex {
        Complex::new(-self.imaginary, self.real)
    }

    #[must_use]
    pub fn mangitude(self) -> f64 {
        f64::sqrt(self.real.powi(2) + self.imaginary.powi(2))
    }

    #[must_use]
    pub fn conjugate(self) -> Complex {
        Complex::new(self.real, -self.imaginary)
    }

    #[must_use]
    pub fn partial_mul(self, other: Complex) -> Complex {
        Complex::new(self.real * other.real, self.imaginary * other.imaginary)
    }

    #[must_use]
    pub fn partial_div(self, other: Complex) -> Complex {
        Complex::new(self.real / other.real, self.imaginary / other.imaginary)
    }

    #[must_use]
    pub fn arg(self) -> f64 {
        f64::atan2(self.imaginary, self.real)
    }

    #[must_use]
    pub fn normalize(self) -> Complex {
        self / self.mangitude()
    }

    #[must_use]
    pub fn sqrt(self) -> Complex {
        // The formula used here doesn't work for negative reals. We can use a trick here to bypass that restriction.
        // If the real part is negative, we simply negate it to get a positive part and then multiply the result by i.
        if self.real > 0.0 {
            // Use the generic formula (https://math.stackexchange.com/questions/44406/how-do-i-get-the-square-root-of-a-complex-number)
            let r = self.mangitude();

            r.sqrt() * (self + r).normalize()
        } else {
            (-self).sqrt().mul_i()
        }
    }

    /// Same as sqrt, but returns a normalized result.
    #[must_use]
    pub fn sqrt_norm(self) -> Complex {
        // The formula used here doesn't work for negative reals. We can use a trick here to bypass that restriction.
        // If the real part is negative, we simply negate it to get a positive part and then multiply the result by i.
        if self.real > 0.0 {
            // Use the generic formula (https://math.stackexchange.com/questions/44406/how-do-i-get-the-square-root-of-a-complex-number)
            let r = self.mangitude();

            // We simply don't multiply by the square root of r.
            (self + r).normalize()
        } else {
            (-self).sqrt_norm().mul_i() // Normalization isn't lost here.
        }
    }
}

impl Mul for Complex {
    type Output = Complex;

    fn mul(self, rhs: Complex) -> Self::Output {
        Complex::new(
            self.real * rhs.real - self.imaginary * rhs.imaginary,
            self.real * rhs.imaginary + rhs.real * self.imaginary,
        )
    }
}

impl Mul<Complex> for f64 {
    type Output = Complex;

    fn mul(self, rhs: Complex) -> Self::Output {
        Complex::new(self * rhs.real, self * rhs.imaginary)
    }
}

impl Mul<f64> for Complex {
    type Output = Complex;

    fn mul(self, rhs: f64) -> Self::Output {
        Complex::new(self.real * rhs, self.imaginary * rhs)
    }
}

impl Add<f64> for Complex {
    type Output = Complex;

    fn add(self, rhs: f64) -> Self::Output {
        Complex::new(self.real + rhs, self.imaginary)
    }
}

impl Add for Complex {
    type Output = Complex;

    fn add(self, rhs: Self) -> Self::Output {
        Complex::new(self.real + rhs.real, self.imaginary + rhs.imaginary)
    }
}

impl Div<f64> for Complex {
    type Output = Complex;

    fn div(self, rhs: f64) -> Self::Output {
        Complex::new(self.real / rhs, self.imaginary / rhs)
    }
}

impl Div for Complex {
    type Output = Complex;

    fn div(self, rhs: Complex) -> Self::Output {
        (self * rhs.conjugate()) / (rhs.real * rhs.real + rhs.imaginary * rhs.imaginary)
    }
}

impl Sub<f64> for Complex {
    type Output = Complex;

    fn sub(self, rhs: f64) -> Self::Output {
        Complex::new(self.real - rhs, self.imaginary)
    }
}

impl Sub for Complex {
    type Output = Complex;

    fn sub(self, rhs: Self) -> Self::Output {
        Complex::new(self.real - rhs.real, self.imaginary - rhs.imaginary)
    }
}

impl SubAssign for Complex {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl AddAssign for Complex {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} + {}i", self.real, self.imaginary)
    }
}

impl Neg for Complex {
    type Output = Complex;

    fn neg(self) -> Self::Output {
        Complex::new(-self.real, -self.imaginary)
    }
}

impl Default for Complex {
    fn default() -> Self {
        Self {
            real: 0.0,
            imaginary: 0.0,
        }
    }
}

pub type Logger = Vec<String>;

/// An object with quality.
#[derive(Debug, Clone)]
pub struct WithQuality<T> {
    pub quality: f64,
    pub value: T
}

#[derive(Debug, Clone)]
pub struct Adjustables {
    /// Free points.
    pub points: Vec<WithQuality<Complex>>,
    /// Free scalars.
    pub scalars: Vec<WithQuality<f64>>,
    /// Circle clips
    pub clips_1d: Vec<WithQuality<f64>>
}

impl Adjustables {
    /// Get all adjustable count.
    pub fn get_count(&self) -> usize {
        self.points.len() + self.scalars.len() + self.clips_1d.len()
    }
}

pub enum Message {
    Generate(f64, Adjustables, Logger),
    Terminate,
}

#[derive(Debug, Clone)]
pub struct GenerationArgs {
    pub criteria: Arc<Vec<Criteria>>,
    pub point_count: usize,
}

fn generation_cycle(
    receiver: &mpsc::Receiver<Message>,
    sender: &mpsc::Sender<(Adjustables, Logger)>,
    args: &GenerationArgs,
    flags: &Arc<Flags>,
) {
    // Create the expression record
    let mut record = HashMap::new();

    if flags.optimizations.identical_expressions {
        for crit in args.criteria.as_ref() {
            let mut exprs = Vec::new();
            crit.object.collect(&mut exprs);

            for expr in exprs {
                // We use weak to not mess with the strong count.
                record.entry(expr).or_insert(ExprCache {
                    value: Value::scalar(0.0),
                    generation: 0,
                });
            }
        }
    }

    let mut generation = 1;
    let record = RefCell::new(record);

    let mut point_evaluation = Vec::new();
    point_evaluation.resize(args.point_count, Vec::new());

    loop {
        match receiver.recv().unwrap() {
            Message::Generate(adjustment, points, mut logger) => {
                let points = magic_box::adjust(points, adjustment);
                let points = critic::evaluate(
                    &points,
                    &args.criteria,
                    &mut logger,
                    generation,
                    flags,
                    Some(&record),
                    &mut point_evaluation,
                );

                // println!("Adjustment + critic = {:#?}", points);

                sender.send((points, logger)).unwrap();
            }
            Message::Terminate => return,
        }

        generation += 1;
    }
}

/// A structure responsible of generating a figure based on criteria and given points.
pub struct Generator {
    /// Current point positions
    current_state: Adjustables,
    /// All the workers (generation cycles).
    workers: Vec<JoinHandle<()>>,
    /// Senders for the workers
    senders: Vec<mpsc::Sender<Message>>,
    /// The receiver for adjusted points from each generation cycle.
    receiver: mpsc::Receiver<(Adjustables, Logger)>,
    /// Total quality of the points - the arithmetic mean of their qualities.
    total_quality: f64,
    /// A delta of the qualities in comparison to previous generation.
    delta: f64,
}

#[derive(Debug, Clone)]
pub struct AdjustablesTemplate {
    pub points: usize,
    pub scalars: usize,
    pub clips_1d: usize
}

impl Generator {
    #[must_use]
    pub fn new(
        template: &AdjustablesTemplate,
        cycles_per_generation: usize,
        args: &GenerationArgs,
        flags: &Arc<Flags>,
    ) -> Self {
        let (input_senders, input_receivers): (
            Vec<mpsc::Sender<Message>>,
            Vec<mpsc::Receiver<Message>>,
        ) = (0..cycles_per_generation).map(|_| mpsc::channel()).unzip();

        let (output_sender, output_receiver) = mpsc::channel();

        Self {
            current_state: Adjustables {
                points: (0..template.points).map(|_| WithQuality {
                    quality: 0.0,
                    value: Complex::new(rand::random(), rand::random())
                }).collect(),
                scalars: (0..template.scalars).map(|_| WithQuality {
                    quality: 0.0,
                    value: rand::random()
                }).collect(),
                clips_1d: (0..template.clips_1d).map(|_| WithQuality {
                    quality: 0.0,
                    value: rand::random()
                }).collect()
            },
            workers: input_receivers
                .into_iter()
                .map(|rec| {
                    let sender = mpsc::Sender::clone(&output_sender);
                    let flags = Arc::clone(flags);
                    let args = args.clone();
                    thread::spawn(move || generation_cycle(&rec, &sender, &args, &flags))
                })
                .collect(),
            senders: input_senders,
            receiver: output_receiver,
            total_quality: 0.0,
            delta: 0.0,
        }
    }

    /// Performs a generation cycle with pre-baked magnitudes (how much a point can get adjusted).
    ///
    /// # Returns
    /// The time it took for this cycle to complete.
    fn cycle_prebaked(&mut self, magnitudes: &[f64]) -> Duration {
        let now = Instant::now();

        // Send data to each worker
        for (i, sender) in self.senders.iter().enumerate() {
            sender
                .send(Message::Generate(
                    magnitudes[i],
                    self.current_state.clone(),
                    Vec::new(),
                ))
                .unwrap();
        }

        let mut final_logger = Vec::new();

        // Wait for each handle to finish and consider its outcome
        for _ in 0..self.workers.len() {
            // If the total quality is larger than the current total, replace the points.
            let (adjustables, logger) = self.receiver.recv().unwrap();
            #[allow(clippy::cast_precision_loss)]
            let total_quality = (
                adjustables.points.iter().map(|x| x.quality).sum::<f64>()
                + adjustables.scalars.iter().map(|x| x.quality).sum::<f64>()
                + adjustables.clips_1d.iter().map(|x| x.quality).sum::<f64>()
            ) / (
                self.current_state.get_count() as f64
            );

            // println!("Total quality: {total_quality}, points: {:#?}", points);

            if total_quality > self.total_quality {
                self.current_state = adjustables;
                self.total_quality = total_quality;
                final_logger = logger;
            }
        }

        let delta = now.elapsed();

        // Show the logger's output
        for line in final_logger {
            println!("{line}");
        }

        delta
    }

    fn bake_magnitudes(&self, maximum_adjustment: f64) -> Vec<f64> {
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

    pub fn single_cycle(&mut self, maximum_adjustment: f64) {
        let current_quality = self.total_quality;

        self.cycle_prebaked(&self.bake_magnitudes(maximum_adjustment));

        self.delta = self.total_quality - current_quality;
    }

    /// Performs generation cycles until the mean delta from the last `mean_count` deltas becomes less or equal to `max_mean`.
    /// Executes `cyclic` after the end of each cycle.
    ///
    /// # Returns
    /// The time it took to generate the figure.
    ///
    /// # Panics
    /// Panics if there are multithreading issues (there has been a panic in one of the generation threads).
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

            self.delta = self.total_quality - current_quality;
            current_quality = self.total_quality;
            let dropped_delta = last_deltas.pop_front().unwrap();
            mean_delta = (mean_delta * mean_count_f - dropped_delta + self.delta) / mean_count_f;
            last_deltas.push_back(self.delta);

            cyclic(current_quality);
        }

        duration
    }

    #[must_use]
    pub fn get_state(&self) -> &Adjustables {
        &self.current_state
    }

    #[must_use]
    pub fn get_delta(&self) -> f64 {
        self.delta
    }

    #[must_use]
    pub fn get_total_quality(&self) -> f64 {
        self.total_quality
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

#[derive(Debug)]
pub struct Optimizations {
    pub identical_expressions: bool,
}

#[derive(Debug)]
pub enum DistanceLiterals {
    Adjust,
    Solve,
    None,
}

#[derive(Debug)]
pub struct Flags {
    pub optimizations: Optimizations,
    pub distance_literals: DistanceLiterals,
    pub point_bounds: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Self {
            optimizations: Optimizations {
                identical_expressions: true,
            },
            distance_literals: DistanceLiterals::None,
            point_bounds: false,
        }
    }
}
