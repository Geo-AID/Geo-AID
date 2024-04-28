use std::collections::HashMap;
use std::mem;
use num_traits::ToPrimitive;
use crate::engine::rage::generator::AdjustableTemplate;
use crate::engine::rage::generator::critic::{EvaluateProgram, FigureProgram};
use crate::engine::rage::generator::program::{Instruction, Loc, Program, ValueType};
use crate::engine::rage::generator::program::expr::{AngleBisector, AngleLine, AnglePoint, AnglePointDir, Average, CircleConstruct, EqualComplex, EqualReal, Greater, InvertQuality, Less, LineFromPoints, LineLineIntersection, Max, Negation, ParallelThrough, PartialPow, PartialProduct, PerpendicularThrough, PointLineDistance, PointOnLine, PointPointDistance, Sum, SwapParts};
use crate::geometry::{Complex, ValueEnum};
use crate::script::math::{Any, Circle, Entity, EntityId, Expr, Intermediate, Line, Number, Point, Rule, RuleKind, VarIndex};
use crate::script::token::number::ProcNum;

#[derive(Debug, Default)]
struct Cursor {
    current: Loc
}

impl Cursor {
    fn next(&mut self) -> Loc {
        self.current += 1;
        self.current - 1
    }
}

#[derive(Debug)]
pub struct Compiler<'i> {
    intermediate: &'i Intermediate,
    constants: Vec<ValueEnum>,
    constants_indices: HashMap<ProcNum, usize>,
    cursor: Cursor,
    rule_cursor: Cursor,
    instructions: Vec<Instruction>,
    entities: Vec<Loc>,
    variables: Vec<Loc>,
    alt_mode: bool,
    biases: Vec<Loc>
}

impl<'i> Compiler<'i> {
    #[must_use]
    pub fn new(intermediate: &'i Intermediate) -> Self {
        Self {
            intermediate,
            constants: Vec::new(),
            constants_indices: HashMap::new(),
            cursor: Cursor::default(),
            rule_cursor: Cursor::default(),
            instructions: Vec::new(),
            entities: Vec::new(),
            variables: Vec::new(),
            alt_mode: false,
            biases: Vec::new()
        }
    }

    fn prepare_constants<'r, I: IntoIterator<Item = &'r Any<VarIndex>>>(&mut self, adjustable_count: usize, exprs: I) {
        self.constants_indices.clear();
        self.constants.clear();
        self.entities.clear();
        self.constants.resize(adjustable_count, ValueEnum::Complex(Complex::zero()));
        self.entities.resize(adjustable_count, usize::MAX);

        for expr in exprs {
            if let Any::Number(Number::Const { value }) = expr {
                self.constants.push(ValueEnum::Complex(value.clone().to_complex()));
                let index = self.constants.len() - 1;
                self.constants_indices.insert(value.clone(), index);
            }
        }
    }

    pub fn compile_programs(mut self) -> (EvaluateProgram, FigureProgram) {
        // 1. Compile `EvaluateProgram`

        // 1a. Figure out adjustables

        let adjustables = self.intermediate.adjusted.entities
            .iter()
            .map(AdjustableTemplate::from)
            .collect();
        let adj_count = adjustables.len();

        // 1b. Figure out constants

        // The first constants are adjustable values
        self.prepare_constants(adj_count, self.intermediate.adjusted.variables.iter().map(|x| x.kind.as_ref()));

        let rule_count = self.intermediate.adjusted.rules.len();
        let program_zero = self.constants.len() + rule_count;

        // 1c. Compile all instructions

        // Cursor must be placed at the end of constants + rules
        self.cursor.current = program_zero;
        self.rule_cursor.current = self.constants.len();

        self.variables.resize(self.intermediate.adjusted.variables.len(), usize::MAX);

        for rule in &self.intermediate.adjusted.rules {
            self.compile(rule);
        }

        let memory_size = self.cursor.next();

        // 1d. Count weights
        let mut weights = Vec::new();
        weights.resize(rule_count * adjustables.len(), 0.0);

        for (i, rule) in self.intermediate.adjusted.rules.iter().enumerate() {
            for EntityId(adj) in &rule.entities {
                weights[i * adjustables.len() + *adj] = 1.0 / rule.entities.len() as f64;
            }
        }

        let evaluate = EvaluateProgram {
            base: Program {
                req_memory_size: memory_size,
                constants: mem::take(&mut self.constants),
                instructions: mem::take(&mut self.instructions)
            },
            adjustables,
            rule_count,
            biases: mem::take(&mut self.biases),
            weights
        };

        let variable_types: Vec<_> = self.intermediate.figure.variables.iter()
            .map(|expr| {
                match expr.kind.as_ref() {
                    Any::Point(_)
                    | Any::Number(_) => ValueType::Complex,
                    Any::Line(_) => ValueType::Line,
                    Any::Circle(_) => ValueType::Circle
                }
            })
            .collect();

        // 2. Compile `FigureProgram`
        self.variables.clear();

        // 2a. Figure out constants

        // The first constants are adjustable values
        self.prepare_constants(adj_count, self.intermediate.figure.variables.iter().map(|x| x.kind.as_ref()));

        let program_zero = adjustables.len();
        self.cursor.current = program_zero;

        for expr in &self.intermediate.figure.variables {
            self.compile(expr);
        }

        let figure = FigureProgram {
            base: Program {
                req_memory_size: self.cursor.next(),
                constants: self.constants,
                instructions: self.instructions
            },
            variables: variable_types.into_iter().zip(self.variables).collect()
        };

        (evaluate, figure)
    }

    fn locate_const(&self, constant: &ProcNum) -> Loc {
        self.constants_indices.get(constant).copied().unwrap()
    }

    fn next_rule(&mut self) -> Loc {
        if self.alt_mode {
            self.cursor.next()
        } else {
            self.rule_cursor.next()
        }
    }

    fn set_alt_mode(&mut self, value: bool) {
        self.alt_mode = value;
    }
}

trait Compile<T> {
    fn compile(&mut self, value: &T) -> Loc;
}

impl<'i> Compile<VarIndex> for Compiler<'i> {
    fn compile(&mut self, value: &VarIndex) -> Loc {
        let loc = self.variables[*value];
        if loc != usize::MAX {
            return loc;
        }

        let loc = self.compile(&self.intermediate.adjusted.variables[*value]);
        self.variables[*value] = loc;
        loc
    }
}

impl<'i, T, M> Compile<Expr<T, M>> for Compiler<'i> where Self: Compile<T> {
    fn compile(&mut self, value: &Expr<T, M>) -> Loc {
        self.compile(value.kind.as_ref())
    }
}

impl<'i> Compile<Any<VarIndex>> for Compiler<'i> {
    fn compile(&mut self, value: &Any<VarIndex>) -> Loc {
        match value {
            Any::Point(v) => self.compile(v),
            Any::Number(v) => self.compile(v),
            Any::Line(v) => self.compile(v),
            Any::Circle(v) => self.compile(v),
        }
    }
}

impl<'i> Compile<Point<VarIndex>> for Compiler<'i> {
    fn compile(&mut self, value: &Point<VarIndex>) -> Loc {
        match value {
            Point::LineLineIntersection { k, l } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::LineLineIntersection(LineLineIntersection {
                    k: self.compile(k),
                    l: self.compile(l),
                    target
                }));

                target
            }
            Point::Average { items } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::Average(Average {
                    params: items.iter().map(|i| self.compile(i)).collect(),
                    target
                }));

                target
            }
            Point::CircleCenter { circle } => {
                // It should be enough to just move the circle.
                self.compile(circle)
            }
            Point::Entity { id } => {
                self.compile(id)
            }
        }
    }
}

impl<'i> Compile<Line<VarIndex>> for Compiler<'i> {
    fn compile(&mut self, value: &Line<VarIndex>) -> Loc {
        match value {
            Line::PointPoint { p, q } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::LineFromPoints(LineFromPoints {
                    a: self.compile(p),
                    b: self.compile(q),
                    target
                }));

                target
            }
            Line::AngleBisector { a, b, c } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::AngleBisector(AngleBisector {
                    arm1: self.compile(a),
                    origin: self.compile(b),
                    arm2: self.compile(c),
                    target
                }));

                target
            }
            Line::ParallelThrough { point, line } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::ParallelThrough(ParallelThrough {
                    point: self.compile(point),
                    line: self.compile(line),
                    target
                }));

                target
            }
            Line::PerpendicularThrough { point, line } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::PerpendicularThrough(PerpendicularThrough {
                    point: self.compile(point),
                    line: self.compile(line),
                    target
                }));

                target
            }
        }
    }
}

impl<'i> Compile<Number<VarIndex>> for Compiler<'i> {
    fn compile(&mut self, value: &Number<VarIndex>) -> Loc {
        match value {
            Number::Entity { id } => {
                self.compile(id)
            }
            Number::Sum { plus, minus } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::Sum(Sum {
                    params: minus.iter().map(|x| self.compile(x)).collect(),
                    target
                }));

                self.instructions.push(Instruction::Negation(Negation {
                    x: target,
                    target
                }));

                self.instructions.push(Instruction::Sum(Sum {
                    params: Some(target)
                        .into_iter()
                        .chain(plus.iter().map(|x| self.compile(x)))
                        .collect(),
                    target
                }));

                target
            }
            Number::Product { times, by } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::PartialProduct(PartialProduct {
                    params: by.iter().map(|x| self.compile(x)).collect(),
                    target
                }));

                self.instructions.push(Instruction::Pow(PartialPow {
                    value: target,
                    exponent: -1.0,
                    target
                }));

                self.instructions.push(Instruction::PartialProduct(PartialProduct {
                    params: Some(target)
                        .into_iter()
                        .chain(times.iter().map(|x| self.compile(x)))
                        .collect(),
                    target
                }));

                target
            }
            Number::Const { value } => {
                self.locate_const(value)
            }
            Number::Power { value, exponent } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::Pow(PartialPow {
                    value: self.compile(value),
                    exponent: exponent.to_f64().unwrap(),
                    target
                }));

                target
            }
            Number::PointPointDistance { p, q } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::PointPointDistance(PointPointDistance {
                    a: self.compile(p),
                    b: self.compile(q),
                    target
                }));

                target
            }
            Number::PointLineDistance { p, k } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::PointLineDistance(PointLineDistance {
                    point: self.compile(p),
                    line: self.compile(k),
                    target
                }));

                target
            }
            Number::ThreePointAngle { p, q, r } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::AnglePoint(AnglePoint {
                    arm1: self.compile(p),
                    origin: self.compile(q),
                    arm2: self.compile(r),
                    target
                }));

                target
            }
            Number::ThreePointAngleDir { p, q, r } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::AnglePointDir(AnglePointDir {
                    arm1: self.compile(p),
                    origin: self.compile(q),
                    arm2: self.compile(r),
                    target
                }));

                target
            }
            Number::TwoLineAngle { k, l } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::AngleLine(AngleLine {
                    k: self.compile(k),
                    l: self.compile(l),
                    target
                }));

                target
            }
            Number::PointX { point } => {
                // Simply moving is enough.
                self.compile(point)
            }
            Number::PointY { point } => {
                // We're going to have to swap parts, instead of just moving.
                let target = self.cursor.next();

                self.instructions.push(Instruction::SwapParts(SwapParts {
                    x: self.compile(point),
                    target
                }));

                target
            }
        }
    }
}

impl<'i> Compile<Circle<VarIndex>> for Compiler<'i> {
    fn compile(&mut self, value: &Circle<VarIndex>) -> Loc {
        match value {
            Circle::Construct { center, radius } => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::CircleConstruct(CircleConstruct {
                    center: self.compile(center),
                    radius: self.compile(radius),
                    target
                }));

                target
            }
        }
    }
}

impl<'i> Compile<EntityId> for Compiler<'i> {
    fn compile(&mut self, value: &EntityId) -> Loc {
        let loc = self.entities[value.0];
        if loc != usize::MAX {
            return loc;
        }

        let ent = self.intermediate.adjusted.entities[value.0].clone();
        let loc = match ent {
            Entity::FreeReal
            | Entity::FreePoint => {
                // The first constants are entities.
                value.0
            }
            Entity::PointOnLine(line) => {
                let target = self.cursor.next();

                self.instructions.push(Instruction::OnLine(PointOnLine {
                    line: self.compile(&line),
                    clip: value.0,
                    target
                }));

                target
            }
            Entity::Bind(_) => unreachable!()
        };
        self.entities[value.0] = loc;
        loc
    }
}

impl<'i> Compile<Rule<VarIndex>> for Compiler<'i> {
    fn compile(&mut self, value: &Rule<VarIndex>) -> Loc {
        self.compile(&value.kind)
    }
}

impl<'i> Compile<RuleKind<VarIndex>> for Compiler<'i> {
    fn compile(&mut self, value: &RuleKind<VarIndex>) -> Loc {
        match value {
            RuleKind::PointEq(a, b) => {
                let target = self.next_rule();

                self.instructions.push(Instruction::EqualComplex(EqualComplex {
                    a: self.compile(a),
                    b: self.compile(b),
                    target
                }));

                target
            }
            RuleKind::NumberEq(a, b) => {
                let target = self.next_rule();

                self.instructions.push(Instruction::EqualReal(EqualReal {
                    a: self.compile(a),
                    b: self.compile(b),
                    target
                }));

                target
            }
            RuleKind::Lt(a, b) => {
                let target = self.next_rule();

                self.instructions.push(Instruction::Less(Less {
                    a: self.compile(a),
                    b: self.compile(b),
                    target
                }));

                target
            }
            RuleKind::Gt(a, b) => {
                let target = self.next_rule();

                self.instructions.push(Instruction::Greater(Greater {
                    a: self.compile(a),
                    b: self.compile(b),
                    target
                }));

                target
            }
            RuleKind::Alternative(items) => {
                let target = self.next_rule();
                let alt = self.alt_mode;

                if !alt {
                    self.alt_mode = true;
                }

                self.instructions.push(Instruction::MaxReal(Max {
                    params: items.iter().map(|x| self.compile(x)).collect(),
                    target
                }));

                if !alt {
                    self.alt_mode = false;
                }

                target
            }
            RuleKind::Invert(rule) => {
                let target = self.compile(rule.as_ref());

                self.instructions.push(Instruction::InvertQuality(InvertQuality {
                    q: target,
                    target
                }));

                target
            }
            RuleKind::Bias => {
                let index = self.next_rule();
                self.biases.push(index);
                index
            }
        }
    }
}
