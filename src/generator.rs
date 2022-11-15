use std::{ops::{Mul, Add, AddAssign, Sub, SubAssign, Div, Neg}, thread::{JoinHandle, self}, sync::{mpsc, Arc}, collections::VecDeque, mem, fmt::Display};

use crate::script::Criteria;

#[derive(Debug)]
pub enum EvaluationError {
    ParallelLines
}

mod magic_box;
mod critic;
pub mod geometry;

/// Represents a complex number located on a "unit" plane.
#[derive(Debug, Clone, Copy)]
pub struct Complex {
    /// X coordinate located in range [0, 1).
    pub real: f64,
    /// Y coordinate located in range [0, 1).
    pub imaginary: f64
}

impl Complex {
    pub fn new(real: f64, imaginary: f64) -> Self { Self { real, imaginary } }

    pub fn mangitude(self) -> f64 {
        f64::sqrt(self.real.powi(2) + self.imaginary.powi(2))
    }
}

impl Mul for Complex {
    type Output = Complex;

    fn mul(self, rhs: Complex) -> Self::Output {
        Complex::new(self.real * rhs.real, self.imaginary * rhs.imaginary)
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
        Complex::new(self.real / rhs.real, self.imaginary / rhs.imaginary)
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
        Complex::new(self.real - rhs.real, self.imaginary + rhs.imaginary)
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

pub type Logger = Vec<String>;

pub enum Message {
    Generate(f64, (Vec<(Complex, f64)>, Logger)),
    Terminate
}

fn generation_cycle(receiver: mpsc::Receiver<Message>, sender: mpsc::Sender<(Vec<(Complex, f64)>, Logger)>, criteria: Arc<Vec<Criteria>>) {
    loop {
        match receiver.recv().unwrap() {
            Message::Generate(adjustment, points) => {
                let mut logger = points.1;
                let points = magic_box::adjust(points.0, adjustment);
                let points = critic::evaluate(points, &criteria, &mut logger);

                // println!("Adjustment + critic = {:#?}", points);

                sender.send((points, logger)).unwrap();
            },
            Message::Terminate => return,
        }
    }
}

/// A structure responsible of generating a figure based on criteria and given points.
pub struct Generator {
    /// Current point positions
    current_points: Vec<(Complex, f64)>,
    /// All the workers (generation cycles).
    workers: Vec<JoinHandle<()>>,
    /// Senders for the workers
    senders: Vec<mpsc::Sender<Message>>,
    /// The receiver for adjusted points from each generation cycle.
    receiver: mpsc::Receiver<(Vec<(Complex, f64)>, Logger)>,
    /// Total quality of the points - the arithmetic mean of their qualities.
    total_quality: f64,
    /// A delta of the qualities in comparison to previous generation.
    delta: f64
}

impl Generator {
    pub fn new(point_count: usize, cycles_per_generation: usize, criteria: Arc<Vec<Criteria>>) -> Self {
        let (input_senders, input_receivers):
            (Vec<mpsc::Sender<Message>>, Vec<mpsc::Receiver<Message>>) = (0..cycles_per_generation).map(
            |_| mpsc::channel()
        ).unzip();

        let (output_sender, output_receiver) = mpsc::channel();

        Self {
            current_points: (0..point_count).map(
                |_| (Complex::new(0.5, 0.5), 0.0)
            ).collect(),
            workers: input_receivers.into_iter().map(
                |rec| {
                    let sender = mpsc::Sender::clone(&output_sender);
                    let criteria = Arc::clone(&criteria);
                    thread::spawn(move || generation_cycle(rec, sender, criteria))
                }
            ).collect(),
            senders: input_senders,
            receiver: output_receiver,
            total_quality: 0.0,
            delta: 0.0
        }
    }

    /// Performs a generation cycle with pre-baked magnitudes (how much a point can get adjusted).
    fn cycle_prebaked(&mut self, magnitudes: &[f64]) {
        // Send data to each worker
        for (i, sender) in self.senders.iter().enumerate() {
            sender.send(Message::Generate(magnitudes[i], (self.current_points.clone(), Vec::new()))).unwrap();
        }

        let mut final_logger = Vec::new();

        // Wait for each handle to finish and consider its outcome
        for _ in 0..self.workers.len() {
            // If the total quality is larger than the current total, replace the points.
            let (points, logger) = self.receiver.recv().unwrap();
            let total_quality = points.iter().map(|x| x.1).sum::<f64>() / self.current_points.len() as f64;

            // println!("Total quality: {total_quality}, points: {:#?}", points);

            if total_quality > self.total_quality {
                self.current_points = points;
                self.total_quality = total_quality;
                final_logger = logger;
            }
        }

        // Show the logger's output
        for line in final_logger {
            println!("{line}");
        }
    }

    fn bake_magnitudes(&self, maximum_adjustment: f64) -> Vec<f64> {
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

    pub fn cycle_until_mean_delta(&mut self, maximum_adjustment: f64, mean_count: usize, max_mean: f64) {
        let magnitudes = self.bake_magnitudes(maximum_adjustment);
        let mut last_deltas = VecDeque::new();

        let mut current_quality = 0.0;
        let mut mean_delta = 1.0;

        last_deltas.resize(mean_count, 1.0);

        while mean_delta > max_mean {
            self.cycle_prebaked(&magnitudes);

            self.delta = self.total_quality - current_quality;
            current_quality = self.total_quality;
            let dropped_delta = last_deltas.pop_front().unwrap();
            mean_delta = (mean_delta * mean_count as f64 - dropped_delta + self.delta) / mean_count as f64;
            last_deltas.push_back(self.delta);
        }
    }

    pub fn get_points(&self) -> &Vec<(Complex, f64)> {
        &self.current_points
    }

    pub fn get_delta(&self) -> f64 {
        self.delta
    }

    pub fn get_total_quality(&self) -> f64 {
        self.total_quality
    }
}

impl Drop for Generator {
    fn drop(&mut self) {
        for sender in self.senders.iter_mut() {
            sender.send(Message::Terminate).unwrap();
        }

        let workers = mem::take(&mut self.workers);

        for worker in workers {
            worker.join().unwrap();
        }
    }
}
