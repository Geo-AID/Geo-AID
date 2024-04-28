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

use geo_aid_derive::Execute;
use serde::Serialize;

use crate::geometry::{Circle, Line, ValueEnum};

use self::expr::*;

use super::Complex;

/// Value type used by the machine.
#[derive(Clone, Copy)]
pub union Value {
    pub complex: Complex,
    pub line: Line,
    pub circle: Circle
}

impl From<ValueEnum> for Value {
    fn from(value: ValueEnum) -> Self {
        match value {
            ValueEnum::Complex(complex) => Self { complex },
            ValueEnum::Line(line) => Self { line },
            ValueEnum::Circle(circle) => Self { circle },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    Complex,
    Line,
    Circle
}

/// # Safety
/// The program is considered safe iff:
/// * `req_memory_size` is larger than any register used by any instruction.
/// * `req_memory_size` is larger than `constants.len()`.
/// * all register accesses are valid (under unions).
#[derive(Debug, Clone, Serialize)]
pub struct Program {
    /// Total memory size required (size of the memory vec).
    pub req_memory_size: usize,
    /// Program instructions.
    pub instructions: Vec<Instruction>,
    /// Precomputed constants used by the program.
    pub constants: Vec<ValueEnum>,
}

impl Execute for Program {
    unsafe fn execute(&self, memory: &mut [Value]) {
        self.instructions.execute(memory);
    }
}

impl Program {
    /// Prepares memory for program execution.
    pub fn setup(&self) -> Vec<Value> {
        let mut memory = Vec::new();
        memory.reserve_exact(self.req_memory_size);
        memory.resize(self.req_memory_size, Value { complex: Complex::default() });

        for (reg, constant) in memory.iter_mut().zip(self.constants.iter().copied()) {
            *reg = constant.into();
        }

        memory
    }
}

/// Represents a location in execution memory.
pub type Loc = usize;

/// Marks everything that can be evaluated.
pub trait Execute {
    /// Evaluates the thing.
    ///
    /// # Errors
    /// Any errors related to evaluation.
    /// 
    /// # Safety
    /// Can only be considered safe if all registers used by it are allocated, all registers are of the right type.
    unsafe fn execute(&self, memory: &mut [Value]);
}

impl<T: Execute> Execute for Vec<T> {
    unsafe fn execute(&self, args: &mut [Value]) {
        for item in self {
            item.execute(args);
        }
    }
}

/// All possible expressions.
pub mod expr {
    use std::f64::consts::PI;

    use geo_aid_derive::instructions;
    use serde::Serialize;

    use crate::geometry::{self, Complex};

    use super::{
        Execute, Line, Circle,
        Value, Loc
    };

    #[derive(Debug, Clone, Copy, Serialize)]
    /// A point on a circle.
    pub struct PointOnCircle {
        pub circle: Loc,
        pub clip: Loc,
        pub target: Loc
    }

    impl Execute for PointOnCircle {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let circle = memory.get_unchecked(self.circle).circle;
            let theta = memory.get_unchecked(self.clip).complex.real * 2.0 * PI;

            let point_rel = Complex::new(theta.cos(), theta.sin());
            memory.get_unchecked_mut(self.target).complex = circle.center + point_rel * circle.radius;
        }
    }

    #[derive(Debug, Clone, Copy, Serialize)]
    /// A point on a aine.
    pub struct PointOnLine {
        pub line: Loc,
        pub clip: Loc,
        pub target: Loc
    }

    impl Execute for PointOnLine {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let line = memory.get_unchecked(self.line).line;
            let mag = memory.get_unchecked(self.clip).complex.real;

            memory.get_unchecked_mut(self.target).complex =line.origin + line.direction * mag;
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// sums all values.
    pub struct Sum {
        pub params: Vec<Loc>,
        pub target: Loc
    }

    impl Execute for Sum {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let sum = self.params.iter().map(|loc| memory.get_unchecked(*loc).complex).sum();
            memory.get_unchecked_mut(self.target).complex = sum;
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// multiplies all values.
    pub struct Product {
        pub params: Vec<Loc>,
        pub target: Loc
    }

    impl Execute for Product {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let product = self.params.iter().map(|loc| memory.get_unchecked(*loc).complex).product();
            memory.get_unchecked_mut(self.target).complex = product;
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// multiplies all values.
    pub struct PartialProduct {
        pub params: Vec<Loc>,
        pub target: Loc
    }

    impl Execute for PartialProduct {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let product = self.params
                .iter()
                .map(|loc| memory.get_unchecked(*loc).complex)
                .fold(Complex::zero(),Complex::partial_mul);
            memory.get_unchecked_mut(self.target).complex = product;
        }
    }

    #[derive(Debug, Clone, Serialize)]
    pub struct Max {
        pub params: Vec<Loc>,
        pub target: Loc
    }

    impl Execute for Max {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let max = self.params.iter().map(|loc| memory.get_unchecked(*loc).complex.real).fold(0.0, f64::max);

            memory.get_unchecked_mut(self.target).complex = Complex::real(max);
        }
    }

    #[derive(Debug, Clone, Serialize)]
    /// calculates the average value.
    pub struct Average {
        pub params: Vec<Loc>,
        pub target: Loc
    }

    impl Execute for Average {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let sum: Complex = self.params.iter().map(|loc| memory.get_unchecked(*loc).complex).sum();

            #[allow(clippy::cast_precision_loss)]
            let x = sum / self.params.len() as f64;
            memory.get_unchecked_mut(self.target).complex = x;
        }
    }

    #[derive(Debug, Clone, Copy, Serialize)]
    /// v^exp.
    pub struct PartialPow {
        pub value: Loc,
        pub exponent: f64,
        pub target: Loc
    }

    impl Execute for PartialPow {
        unsafe fn execute(&self, memory: &mut [Value]){
            let v = memory.get_unchecked(self.value).complex;

            memory.get_unchecked_mut(self.target).complex = Complex::new(v.real.powf(self.exponent), v.imaginary.powf(self.exponent));
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Move {
        pub from: Loc,
        pub to: Loc
    }

    impl Execute for Move {
        unsafe fn execute(&self, memory: &mut [Value]) {
            let v = *memory.get_unchecked(self.from);
            *memory.get_unchecked_mut(self.to) = v;
        }
    }

    instructions!{
        PointPointDistance(a: Complex, b: Complex) -> Real {
            (a - b).magnitude()
        }

        PointLineDistance(point: Complex, line: Line) -> Real {
            ((point - line.origin) / line.direction).imaginary.abs()
        }

        AnglePoint(arm1: Complex, origin: Complex, arm2: Complex) -> Real {
            geometry::get_angle(arm1, origin, arm2)
        }

        AnglePointDir(arm1: Complex, origin: Complex, arm2: Complex) -> Real {
            geometry::get_angle_directed(arm1, origin, arm2)
        }

        AngleLine(k: Line, l: Line) -> Real {
            (k.direction / l.direction).arg().abs()
        }

        LineFromPoints(a: Complex, b: Complex) -> Line {
            Line::new(a, (b - a).normalize())
        }

        LineLineIntersection(k: Line, l: Line) -> Complex {
            geometry::get_intersection(k, l)
        }

        Negation(x: Complex) -> Complex {
            -x
        }

        Inverse(x: Complex) -> Complex {
            x.inverse()
        }

        SwapParts(x: Complex) -> Complex {
            Complex::new(x.imaginary, x.real)
        }

        AngleBisector(arm1: Complex, origin: Complex, arm2: Complex) -> Line {
            // Make the system relative to origin
            let a = arm1 - origin;
            let b = arm2 - origin;

            // Get the bisector using the geometric mean.
            let bi_dir = (a * b).sqrt_norm();

            Line::new(origin, bi_dir)
        }

        PerpendicularThrough(point: Complex, line: Line) -> Line {
            Line::new(point, line.direction.mul_i())
        }

        ParallelThrough(point: Complex, line: Line) -> Line {
            Line::new(point, line.direction)
        }

        CircleCenter(circle: Circle) -> Complex {
            circle.center
        }

        CircleRadius(circle: Circle) -> Real {
            circle.radius
        }

        CircleConstruct(center: Complex, radius: Real) -> Circle {
            Circle {
                center,
                radius
            }
        }
    }

    // Rule-related instructions

    /// That's the infamous sigma function. It packs a [0, +inf) range into [0, 1).
    fn smooth_0_inf(x: f64) -> f64 {
        1.0 - 1.0 / (x + 1.0)
    }
    
    /// That's the infamous sigma function. It packs a (-inf, +inf) range into (0, 1).
    fn smooth_inf_inf(x: f64) -> f64 {
        if x >= 0.0 {
            (1.0 + smooth_0_inf(x)) / 2.0
        } else {
            (1.0 - smooth_0_inf(-x)) / 2.0
        }
    }
    
    /// Inverts the quality. As simple as 1 - q
    #[inline]
    fn invert_quality(q: f64) -> f64 {
        1.0 - q
    }

    instructions! {
        EqualReal(a: Real, b: Real) -> Real {
            let diff = a - b;
            // Interestingly, it's easier to calculate the quality function for != and then invert it.
            invert_quality(smooth_0_inf(1130.0 * diff.powi(2)))
        }

        EqualComplex(a: Complex, b: Complex) -> Real {
            let diff = (a - b).magnitude();
            // Interestingly, it's easier to calculate the quality function for != and then invert it.
            invert_quality(smooth_0_inf(1130.0 * diff.powi(2)))
        }

        Less(a: Real, b: Real) -> Real {
            // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
            let diff = (a - b) / a.abs();
            smooth_inf_inf(-54.0 * f64::cbrt(diff + 0.001))
        }

        Greater(a: Real, b: Real) -> Real {
            // Note that the difference is not the same as with equality. This time we have to be prepared for negative diffs.
            let diff = (a - b) / a.abs();
            smooth_inf_inf(-54.0 * f64::cbrt(diff - 0.001))
        }

        InvertQuality(q: Real) -> Real {
            1.0 - q
        }
    }
}

/// A program instruction.
#[derive(Debug, Clone, Serialize, Execute)]
pub enum Instruction {
    /// A point on a circle.
    OnCircle(PointOnCircle),
    /// A point on a line.
    OnLine(PointOnLine),
    /// Takes the average value (arithmetic mean)
    Average(Average),
    /// The point where two lines cross.
    LineLineIntersection(LineLineIntersection),
    /// The centre of a circle.
    CircleCenter(CircleCenter),
    /// A circle given the center and the radius.
    CircleConstruct(CircleConstruct),
    /// A line in euclidean space. defined by two points.
    LineFromPoints(LineFromPoints),
    /// An angle bisector.
    AngleBisector(AngleBisector),
    /// Generates a line perpendicular to $1 going through $2
    PerpendicularThrough(PerpendicularThrough),
    /// Generates a line parallel to $1 going through $2
    ParallelThrough(ParallelThrough),
    /// Euclidean distance between two points.
    PointPointDistance(PointPointDistance),
    /// Euclidean distance between a point and its rectangular projection onto a line.
    PointLineDistance(PointLineDistance),
    /// An angle defined with 3 points.
    AnglePoint(AnglePoint),
    /// A directed angle defined with 3 points.
    AnglePointDir(AnglePointDir),
    /// An angle defined with 2 lines.
    AngleLine(AngleLine),
    /// Adds two values
    Sum(Sum),
    /// Multiplies two values
    Product(Product),
    /// Partially multiplies two values
    PartialProduct(PartialProduct),
    /// Changes the sign
    Negation(Negation),
    /// Raises to a power
    Pow(PartialPow),
    /// Radius of a circle
    CircleRadius(CircleRadius),
    /// Swaps real and imaginary parts
    SwapParts(SwapParts),
    /// Compares two complexes
    EqualComplex(EqualComplex),
    /// Compares two reals
    EqualReal(EqualReal),
    /// Compares two reals
    Greater(Greater),
    /// Compares two reals
    Less(Less),
    /// Gets the maximal parameter value.
    MaxReal(Max),
    /// Inverts a rule's quality.
    InvertQuality(InvertQuality)
}
