/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use std::{
    collections::VecDeque,
    fmt::Display,
    mem,
    ops::{Add, AddAssign, Div, Mul, Neg, Sub, SubAssign, MulAssign},
    sync::{mpsc, Arc},
    thread::{self, JoinHandle},
    time::{Duration, Instant}, iter::{Sum, Product}, pin::Pin,
};

use serde::Serialize;

use self::{program::Value, critic::EvaluateProgram};

pub mod critic;
pub mod fast_float;
pub mod geometry;
pub mod program;
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
    #[inline]
    pub const fn new(real: f64, imaginary: f64) -> Self {
        Self { real, imaginary }
    }

    #[must_use]
    pub const fn real(real: f64) -> Self {
        Self::new(real, 0.0)
    }

    #[must_use]
    #[inline]
    pub const fn zero() -> Self {
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
    pub fn magnitude(self) -> f64 {
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
        self / self.magnitude()
    }

    /// Number-theoretical norm. Simply a^2 + b^2 with self = a + bi
    #[must_use]
    pub fn len_squared(self) -> f64 {
        self.real * self.real + self.imaginary * self.imaginary
    }

    #[must_use]
    pub fn sqrt(self) -> Complex {
        // The formula used here doesn't work for negative reals. We can use a trick here to bypass that restriction.
        // If the real part is negative, we simply negate it to get a positive part and then multiply the result by i.
        if self.real > 0.0 {
            // Use the generic formula (https://math.stackexchange.com/questions/44406/how-do-i-get-the-square-root-of-a-complex-number)
            let r = self.magnitude();

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
            let r = self.magnitude();

            // We simply don't multiply by the square root of r.
            (self + r).normalize()
        } else {
            (-self).sqrt_norm().mul_i() // Normalization isn't lost here.
        }
    }

    /// Inverse of the number.
    #[must_use]
    pub fn inverse(self) -> Self {
        self.conjugate() / self.len_squared()
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

impl MulAssign for Complex {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
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

impl Sum for Complex {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut v = Complex::zero();

        for x in iter {
            v += x;
        }

        v
    }
}

impl Product for Complex {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut v = Complex::zero();

        for x in iter {
            v *= x;
        }

        v
    }
}

pub type Logger = Vec<String>;

#[derive(Debug, Clone, Copy)]
pub enum Adjustable {
    Point(Complex),
    Real(f64),
    Clip1D(f64),
}

impl Adjustable {
    //noinspection DuplicatedCode
    #[must_use]
    pub fn as_point(&self) -> Option<&Complex> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_real(&self) -> Option<&f64> {
        if let Self::Real(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_clip1d(&self) -> Option<&f64> {
        if let Self::Clip1D(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub enum Message {
    Generate(f64),
    Terminate,
}

#[derive(Debug)]
struct SendPtr<T: ?Sized>(*const T);

impl<T: ?Sized> Clone for SendPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T: ?Sized> Copy for SendPtr<T> {}

unsafe impl<T: ?Sized> Send for SendPtr<T> {}

unsafe fn generation_cycle(
    receiver: mpsc::Receiver<Message>,
    sender: mpsc::Sender<CycleState>,
    program: Arc<EvaluateProgram>,
    flags: Arc<Flags>,
    current_state: SendPtr<State>
) {
    let mut memory = program.setup();
    let mut qualities = Vec::new();
    qualities.reserve_exact(program.adjustables.len());
    qualities.resize(program.adjustables.len(), 0.0);

    // A complete cycle is the following.
    // 1. Receive adjustment information.
    // 2. Adjust the current state onto the thread state.
    // 3. Evaluate adjustables' qualities.
    // 4. Send back thread's state.

    loop {
        match receiver.recv().unwrap() {
            Message::Generate(adjustment_magnitude) => {
                {
                    magic_box::adjust(&*current_state.0, &mut memory[0..program.adjustables.len()], adjustment_magnitude);
                }

                program.evaluate(&mut memory, &mut qualities);

                #[allow(clippy::cast_precision_loss)]
                let total_quality = qualities.iter().copied().sum::<f64>() / qualities.len() as f64;

                sender.send(CycleState {
                    adjustables: SendPtr(memory.as_slice()),
                    qualities: SendPtr(qualities.as_slice()),
                    total_quality
                }).unwrap();
            }
            Message::Terminate => return,
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    pub adjustables: Vec<Adjustable>,
    pub qualities: Vec<f64>,
    pub total_quality: f64
}

#[derive(Clone, Copy)]
struct CycleState {
    pub adjustables: SendPtr<[Value]>,
    pub qualities: SendPtr<[f64]>,
    pub total_quality: f64
}

/// A structure responsible of generating a figure based on criteria and given points.
pub struct Generator {
    /// Current values of all adjustables.
    current_state: Pin<Box<State>>,
    /// All the workers (generation cycles).
    workers: Vec<JoinHandle<()>>,
    /// Senders for the workers
    senders: Vec<mpsc::Sender<Message>>,
    /// The receiver for adjusted points from each generation cycle.
    receiver: mpsc::Receiver<CycleState>,
    /// A delta of the qualities in comparison to previous generation.
    delta: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum AdjustableTemplate {
    Point,
    PointOnCircle,
    PointOnLine,
    Real,
}

impl AdjustableTemplate {
    /// Returns `true` if the adjustable template is [`Point`].
    ///
    /// [`Point`]: AdjustableTemplate::Point
    #[must_use]
    pub fn is_point(&self) -> bool {
        matches!(self, Self::Point | Self::PointOnLine | Self::PointOnCircle)
    }
}

impl Generator {
    /// # Safety
    /// The `program` MUST be safe.
    #[must_use]
    pub unsafe fn new(
        workers: usize,
        program: EvaluateProgram,
        flags: &Arc<Flags>,
    ) -> Self {
        let (input_senders, input_receivers): (
            Vec<mpsc::Sender<Message>>,
            Vec<mpsc::Receiver<Message>>,
        ) = (0..workers).map(|_| mpsc::channel()).unzip();

        let (output_sender, output_receiver) = mpsc::channel();

        let current_state = State {
            adjustables: program.adjustables
                .iter()
                .map(|temp| {
                    match temp {
                        AdjustableTemplate::Point => {
                            Adjustable::Point(Complex::new(rand::random(), rand::random()))
                        }
                        AdjustableTemplate::PointOnCircle | AdjustableTemplate::PointOnLine => {
                            Adjustable::Clip1D(rand::random())
                        }
                        AdjustableTemplate::Real => Adjustable::Real(rand::random()),
                    }
                })
                .collect(),
            qualities: {
                let mut v = Vec::new();
                v.reserve_exact(program.adjustables.len());
                v.resize(program.adjustables.len(), 0.0);
                v
            },
            total_quality: 0.0
        };
        let current_state = Box::pin(current_state);
        let state_ptr: &State = &current_state;
        let state_ptr = SendPtr(state_ptr);

        let program = Arc::new(program);

        Self {
            current_state,
            workers: input_receivers
                .into_iter()
                .map(|rec| {
                    let sender = mpsc::Sender::clone(&output_sender);
                    let flags = Arc::clone(flags);
                    let program = Arc::clone(&program);
                    thread::spawn(move || unsafe { generation_cycle(rec, sender, program, flags, state_ptr) })
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
    fn cycle_prebaked(&mut self, magnitudes: &[f64]) -> Duration {
        let now = Instant::now();

        // Send data to each worker
        for (i, sender) in self.senders.iter().enumerate() {
            sender
                .send(Message::Generate(magnitudes[i]))
                .unwrap();
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
            unsafe {
                let mut_ref = self.current_state.as_mut().get_unchecked_mut();
                mut_ref.qualities.clone_from_slice(&*best.qualities.0);

                for (adj, value) in mut_ref.adjustables.iter_mut().zip(&*best.adjustables.0) {
                    match adj {
                        Adjustable::Point(x) => *x = value.complex,
                        Adjustable::Real(x) => *x = value.complex.real,
                        Adjustable::Clip1D(x) => *x = value.complex.real,
                    }
                }

                mut_ref.total_quality = best.total_quality;
            }
        }

        now.elapsed()
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
        let current_quality = self.get_total_quality();

        self.cycle_prebaked(&self.bake_magnitudes(maximum_adjustment));

        self.delta = self.get_total_quality();
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

    #[must_use]
    pub fn get_delta(&self) -> f64 {
        self.delta
    }

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

#[derive(Debug)]
pub struct Optimizations {}

#[derive(Debug)]
pub struct Flags {
    pub optimizations: Optimizations,
    pub point_bounds: bool,
}

impl Default for Flags {
    fn default() -> Self {
        Self {
            optimizations: Optimizations {},
            point_bounds: false,
        }
    }
}
