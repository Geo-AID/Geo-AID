use std::{
    collections::{hash_map::Entry, HashMap},
    rc::Rc, fmt::Display,
};

use super::{
    parser::{
        Expression, GetType, LetStatement, Parse, PredefinedType, SimpleExpression,
        Statement, Type, RuleStatement, RuleOperator, PredefinedRuleOperator, BinaryOperator
    },
    token::{self, Ident, Span},
    ScriptError, ComplexUnit, SimpleUnit, builtins,
};

#[derive(Debug)]
pub struct RuleOperatorDefinition {
    pub name: String,
}

#[derive(Debug)]
pub struct PointMeta {
    pub letter: char,
    pub primes: u8,
    pub index: Option<u16>,
}

#[derive(Debug)]
pub struct Point {
    pub meta: Option<PointMeta>,
}

#[derive(Debug)]
pub enum VariableMeta {
    Point(Point),
    Scalar,
    Line,
    Properties,
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub definition_span: Span,
    pub definition: UnrolledExpression,
    pub meta: VariableMeta,
}

impl GetType for Variable {
    fn get_type(&self, _: &CompileContext) -> Result<Type, ScriptError> {
        Ok(self.definition.ty.clone())
    }

    fn match_type(&self, context: &CompileContext, t: &Type) -> Result<(), ScriptError> {
        let vartype = self.get_type(context)?;

        if &vartype == t {
            Ok(())
        } else {
            Err(ScriptError::invalid_type(
                t.clone(),
                vartype,
                self.definition_span,
            ))
        }
    }
}

#[derive(Debug)]
pub struct FunctionOverload {
    pub params: Vec<Type>,
    pub returned_type: Type,
    pub definition_span: Option<Span>,
    pub definition: UnrolledExpression
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub overloads: Vec<FunctionOverload>
}

impl Function {
    pub fn match_params(expected: &Vec<Type>, received: &Vec<Type>) -> bool {
        if expected.len() == received.len() {
            for (i, param) in expected.iter().enumerate() {
                if !received[i].can_cast(param) {
                    return false
                }
            }

            true
        } else {
            false
        }
    }

    pub fn get_overload(&self, params: &Vec<Type>) -> Option<&FunctionOverload> {
        self.overloads.iter().find(|x| Function::match_params(&x.params, params))
    }

    pub fn get_returned(&self, params: &Vec<Type>) -> Type {
        self.get_overload(params).map(
            |x| x.returned_type.clone()
        ).unwrap_or(Type::Undefined)
    }
}

#[derive(Debug)]
pub struct CompileContext {
    pub rule_ops: HashMap<String, Rc<RuleOperatorDefinition>>,
    pub variables: HashMap<String, Rc<Variable>>,
    pub points: HashMap<u64, Rc<Variable>>,
    pub functions: HashMap<String, Function>,
}

fn check_expression_iterators(expr: &Expression) -> Result<usize, ScriptError> {
    match expr {
        Expression::Simple(s) => match s.collection.len() {
            0 => check_simple_iterators(&s.first),
            l => Ok(l + 1),
        },
        Expression::Binop(e) => {
            let iters = check_expression_iterators(&e.lhs)?;
            if !check_expression_iterators_hint(&e.rhs, iters) {
                Err(ScriptError::inconsistent_iterators(expr.get_span()))
            } else {
                Ok(iters)
            }
        }
    }
}

fn check_simple_iterators(expr: &SimpleExpression) -> Result<usize, ScriptError> {
    match expr {
        SimpleExpression::Ident(_) => Ok(1),
        SimpleExpression::Number(_) => Ok(1),
        SimpleExpression::Call(e) => {
            let mut iters = 1;

            if let Some(params) = &e.params {
                for param in params.iter() {
                    match iters {
                        1 => iters = check_expression_iterators(param)?,
                        _ => {
                            if !check_expression_iterators_hint(param, iters) {
                                return Err(ScriptError::inconsistent_iterators(param.get_span()));
                            }
                        }
                    }
                }
            }

            Ok(iters)
        }
        SimpleExpression::Unop(e) => check_simple_iterators(e.rhs.as_ref()),
        SimpleExpression::Parenthised(p) => check_expression_iterators(p.content.as_ref()),
    }
}

fn check_simple_iterators_hint(expr: &SimpleExpression, hint: usize) -> bool {
    match expr {
        SimpleExpression::Ident(_) => true,
        SimpleExpression::Number(_) => true,
        SimpleExpression::Call(e) => {
            if let Some(params) = &e.params {
                for param in params.iter() {
                    if !check_expression_iterators_hint(param.as_ref(), hint) {
                        return false;
                    }
                }
            }

            true
        }
        SimpleExpression::Unop(e) => check_simple_iterators_hint(e.rhs.as_ref(), hint),
        SimpleExpression::Parenthised(e) => {
            check_expression_iterators_hint(e.content.as_ref(), hint)
        }
    }
}

fn check_expression_iterators_hint(expr: &Expression, hint: usize) -> bool {
    match expr {
        Expression::Simple(s) => match s.collection.len() {
            0 => check_simple_iterators_hint(&s.first, hint),
            l => l + 1 == hint,
        },
        Expression::Binop(e) => {
            check_expression_iterators_hint(e.lhs.as_ref(), hint)
                && check_expression_iterators_hint(e.rhs.as_ref(), hint)
        }
    }
}

#[derive(Debug)]
pub enum UnrolledRuleKind {
    Eq,
    Gt,
    Lt
}

#[derive(Debug, Clone)]
pub enum UnrolledExpressionData {
    VariableAccess(Rc<Variable>),
    PointCollection(Vec<Rc<Variable>>),
    Number(f64),
    FreePoint,
    Boxed(UnrolledExpression),
    Parameter(usize),
    IndexCollection(UnrolledExpression, usize),
    LineFromPoints(UnrolledExpression, UnrolledExpression),
    SetUnit(UnrolledExpression, ComplexUnit),
    PointPointDistance(UnrolledExpression, UnrolledExpression),
    PointLineDistance(UnrolledExpression, UnrolledExpression),
    Negate(UnrolledExpression),
    Add(UnrolledExpression, UnrolledExpression),
    Subtract(UnrolledExpression, UnrolledExpression),
    Multiply(UnrolledExpression, UnrolledExpression),
    Divide(UnrolledExpression, UnrolledExpression),
}

impl Display for UnrolledExpressionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnrolledExpressionData::VariableAccess(name) => write!(f, "{}", name.name),
            UnrolledExpressionData::PointCollection(col) => write!(
                f, "col({})",
                col.iter().map(
                    |v| v.name.clone()
                ).collect::<Vec<String>>().join(", ")
            ),
            UnrolledExpressionData::Number(num) => write!(f, "{num}"),
            UnrolledExpressionData::FreePoint => write!(f, "Point"),
            UnrolledExpressionData::Boxed(expr) => write!(f, "{expr}"),
            UnrolledExpressionData::Parameter(index) => write!(f, "${index}"),
            UnrolledExpressionData::IndexCollection(expr, index) => write!(f, "{expr}[{index}]"),
            UnrolledExpressionData::LineFromPoints(e1, e2) => write!(f, "line({e1}, {e2})"),
            UnrolledExpressionData::SetUnit(expr, _) => write!(f, "{expr}"),
            UnrolledExpressionData::PointPointDistance(e1, e2)
            | UnrolledExpressionData::PointLineDistance(e1, e2) => write!(f, "dst({e1}, {e2})"),
            UnrolledExpressionData::Negate(e) => write!(f, "-{e}"),
            UnrolledExpressionData::Add(e1, e2) => write!(f, "{e1} + {e2}"),
            UnrolledExpressionData::Multiply(e1, e2) => write!(f, "{e1} * {e2}"),
            UnrolledExpressionData::Divide(e1, e2) => write!(f, "{e1} / {e2}"),
            UnrolledExpressionData::Subtract(e1, e2) => write!(f, "{e1} - {e2}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnrolledExpression {
    pub data: Rc<UnrolledExpressionData>,
    pub ty: Type,
    pub span: Span
}

impl Display for UnrolledExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

#[derive(Debug)]
pub struct UnrolledRule {
    pub kind: UnrolledRuleKind,
    pub lhs: UnrolledExpression,
    pub rhs: UnrolledExpression,
    pub inverted: bool
}

impl Display for UnrolledRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}{} {}",
            self.lhs,
            if self.inverted {"!"} else {""},
            match self.kind {
                UnrolledRuleKind::Eq => "=",
                UnrolledRuleKind::Gt => ">",
                UnrolledRuleKind::Lt => "<"
            },
            self.rhs
        )
    }
}

fn construct_point_id(letter: char, primes: u8) -> u64 {
    ((letter as u64) << 8) | primes as u64
}

fn construct_point_name(letter: char, primes: u8) -> String {
    String::from(letter) + &"'".repeat(primes as usize)
}

fn unroll_parameters(definition: &UnrolledExpression, params: &Vec<UnrolledExpression>) -> UnrolledExpression {
    UnrolledExpression {
        ty: definition.ty.clone(),
        span: definition.span,
        data: Rc::new(match definition.data.as_ref() {
            UnrolledExpressionData::Boxed(expr)
            | UnrolledExpressionData::Negate(expr) => UnrolledExpressionData::Boxed(
                unroll_parameters(expr, params)
            ),
            UnrolledExpressionData::Parameter(index) => UnrolledExpressionData::Boxed(params[*index].clone()),
            UnrolledExpressionData::IndexCollection(expr, index) => UnrolledExpressionData::IndexCollection(
                unroll_parameters(expr, params),
                *index
            ),
            UnrolledExpressionData::LineFromPoints(e1, e2) => UnrolledExpressionData::LineFromPoints(
                unroll_parameters(e1, params),
                unroll_parameters(e2, params)
            ),
            UnrolledExpressionData::SetUnit(expr, unit) => UnrolledExpressionData::SetUnit(
                unroll_parameters(expr, params),
                unit.clone()
            ),
            UnrolledExpressionData::PointPointDistance(e1, e2) =>
                UnrolledExpressionData::PointPointDistance(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params)
                ),
            UnrolledExpressionData::PointLineDistance(e1, e2) =>
                UnrolledExpressionData::PointLineDistance(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params)
                ),
            UnrolledExpressionData::VariableAccess(_) | UnrolledExpressionData::PointCollection(_)
            | UnrolledExpressionData::Number(_) | UnrolledExpressionData::FreePoint => definition.data.as_ref().clone(),
            UnrolledExpressionData::Add(e1, e2) =>
                UnrolledExpressionData::Add(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params)
                ),
            UnrolledExpressionData::Subtract(e1, e2) =>
                UnrolledExpressionData::Subtract(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params)
                ),
            UnrolledExpressionData::Multiply(e1, e2) =>
                UnrolledExpressionData::Multiply(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params)
                ),
            UnrolledExpressionData::Divide(e1, e2) =>
                UnrolledExpressionData::Divide(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params)
                ),
        })
    }
}

fn unroll_implicit_conversion(expr: UnrolledExpression, to: &Type) -> Result<UnrolledExpression, ScriptError> {
    if to == &expr.ty {
        Ok(expr)
    } else {
        match &expr.ty {
            Type::Predefined(pre) => match pre {
                PredefinedType::PointCollection(l) => match l {
                    1 => if to == &Type::Predefined(PredefinedType::Point) {
                        Ok(UnrolledExpression {
                            data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), 0)),
                            ty: Type::Predefined(PredefinedType::Point),
                            span: expr.span,
                        })
                    } else {
                        Err(ScriptError::implicit_conversion_does_not_exist(expr.span, expr.ty, to.clone()))
                    },
                    2 => if to == &Type::Predefined(PredefinedType::Line) {
                        Ok(UnrolledExpression {
                            data: Rc::new(UnrolledExpressionData::LineFromPoints(
                                UnrolledExpression {
                                    data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), 0)),
                                    ty: Type::Predefined(PredefinedType::Point),
                                    span: expr.span,
                                },
                                UnrolledExpression {
                                    data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), 1)),
                                    ty: Type::Predefined(PredefinedType::Point),
                                    span: expr.span,
                                }
                            )),
                            ty: Type::Predefined(PredefinedType::Line),
                            span: expr.span,
                        })
                    } else {
                        Err(ScriptError::implicit_conversion_does_not_exist(expr.span, expr.ty, to.clone()))
                    },
                    _ => Err(ScriptError::implicit_conversion_does_not_exist(expr.span, expr.ty, to.clone()))
                },
                PredefinedType::Scalar(None) => if matches!(to, Type::Predefined(PredefinedType::Scalar(_))) {
                    match expr.data.as_ref() {
                        UnrolledExpressionData::Boxed(x) => Ok(UnrolledExpression {
                            ty: to.clone(),
                            span: expr.span,
                            data: Rc::new(UnrolledExpressionData::Boxed(unroll_implicit_conversion(x.clone(), to)?))
                        }),
                        UnrolledExpressionData::Number(_) => Ok(UnrolledExpression {
                            ty: to.clone(),
                            span: expr.span,
                            data: Rc::clone(&expr.data)
                        }),
                        UnrolledExpressionData::Negate(x) => Ok(UnrolledExpression {
                            ty: to.clone(),
                            span: expr.span,
                            data: Rc::new(UnrolledExpressionData::Negate(unroll_implicit_conversion(x.clone(), to)?))
                        }),
                        UnrolledExpressionData::Add(e1, e2) => Ok(UnrolledExpression {
                            ty: to.clone(),
                            span: expr.span,
                            data: Rc::new(UnrolledExpressionData::Add(
                                unroll_implicit_conversion(e1.clone(), to)?,
                                unroll_implicit_conversion(e2.clone(), to)?
                            ))
                        }),
                        UnrolledExpressionData::Subtract(e1, e2) => Ok(UnrolledExpression {
                            ty: to.clone(),
                            span: expr.span,
                            data: Rc::new(UnrolledExpressionData::Subtract(
                                unroll_implicit_conversion(e1.clone(), to)?,
                                unroll_implicit_conversion(e2.clone(), to)?
                            ))
                        }),
                        UnrolledExpressionData::Multiply(e1, e2) => Ok(UnrolledExpression {
                            ty: to.clone(),
                            span: expr.span,
                            data: Rc::new(UnrolledExpressionData::Multiply(
                                unroll_implicit_conversion(e1.clone(), to)?,
                                unroll_implicit_conversion(e2.clone(), &Type::Predefined(PredefinedType::Scalar(
                                    Some(ComplexUnit::new(SimpleUnit::Scalar))
                                )))?
                            ))
                        }),
                        UnrolledExpressionData::Divide(e1, e2) => Ok(UnrolledExpression {
                            ty: to.clone(),
                            span: expr.span,
                            data: Rc::new(UnrolledExpressionData::Divide(
                                unroll_implicit_conversion(e1.clone(), to)?,
                                unroll_implicit_conversion(e2.clone(), &Type::Predefined(PredefinedType::Scalar(
                                    Some(ComplexUnit::new(SimpleUnit::Scalar))
                                )))?
                            ))
                        }),
                        UnrolledExpressionData::VariableAccess(_) | UnrolledExpressionData::PointCollection(_)
                        | UnrolledExpressionData::FreePoint
                        |UnrolledExpressionData::Parameter(_) | UnrolledExpressionData::IndexCollection(_, _)
                        | UnrolledExpressionData::LineFromPoints(_, _) | UnrolledExpressionData::SetUnit(_, _)
                        | UnrolledExpressionData::PointPointDistance(_, _) | UnrolledExpressionData::PointLineDistance(_, _) => unreachable!(
                            "This data should not be of type scalar(none) and yet is: {:#?}",
                            expr.data
                        ),
                    }
                } else {
                    Err(ScriptError::implicit_conversion_does_not_exist(expr.span, expr.ty, to.clone()))
                },
                _ => Err(ScriptError::implicit_conversion_does_not_exist(expr.span, expr.ty, to.clone()))
            },
            _ => Err(ScriptError::implicit_conversion_does_not_exist(expr.span, expr.ty, to.clone())),
        }
    }

}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
fn unroll_simple(
    expr: &SimpleExpression,
    context: &CompileContext,
    it_index: usize
) -> Result<UnrolledExpression, ScriptError> {
    Ok(match expr {
        SimpleExpression::Ident(i) => match i {
            Ident::Named(named) => {
                let var = context.variables.get(&named.ident).ok_or_else(|| {
                    ScriptError::undefined_variable(expr.get_span(), named.ident.clone())
                })?;

                UnrolledExpression {
                    ty: var.get_type(context)?,
                    data: Rc::new(UnrolledExpressionData::VariableAccess(Rc::clone(var))),
                    span: named.span
                }
            }
            Ident::Collection(col) => UnrolledExpression {
                ty: Type::Predefined(PredefinedType::PointCollection(col.collection.len())),
                data: Rc::new(UnrolledExpressionData::PointCollection(
                    col.collection
                        .iter()
                        .map(|(letter, primes)| {
                            match context.points.get(&construct_point_id(*letter, *primes)) {
                                Some(var) => Ok(Rc::clone(var)),
                                None => Err(ScriptError::undefined_variable(
                                    col.span,
                                    construct_point_name(*letter, *primes),
                                )),
                            }
                        })
                        .collect::<Result<Vec<Rc<Variable>>, ScriptError>>()?,
                )),
                span: col.span
            },
        },
        SimpleExpression::Number(num) => UnrolledExpression {
            ty: Type::Predefined(PredefinedType::Scalar(None)),
            data: Rc::new(UnrolledExpressionData::Number(num.value)),
            span: num.get_span()
        },
        SimpleExpression::Call(e) => {
            if let Some(func) = context.functions.get(&e.name.ident) {

                let params = match &e.params {
                    Some(params) => params.iter().map(
                        |p| unroll_expression(p, context, it_index)
                    ).collect::<Result<Vec<UnrolledExpression>, ScriptError>>()?,
                    None => Vec::new()
                };

                let param_types = params.iter().map(|ex| ex.ty.clone()).collect();

                if let Some(overload) = func.get_overload(&param_types) {
                    let params = params.into_iter().enumerate().map(
                        |(i, param)| unroll_implicit_conversion(param, &overload.params[i])
                    ).collect::<Result<Vec<UnrolledExpression>, ScriptError>>()?;

                    UnrolledExpression {
                        ty: overload.returned_type.clone(),
                        data: Rc::new(UnrolledExpressionData::Boxed(unroll_parameters(&overload.definition, &params))),
                        span: e.get_span()
                    }
                } else {
                    return Err(ScriptError::overload_not_found(e.get_span(), e.name.ident.clone(), param_types))
                }
            } else {
                return Err(ScriptError::undefined_function(e.get_span(), e.name.ident.clone()))
            }
        },
        SimpleExpression::Unop(op) => {
            let unrolled = unroll_simple(&op.rhs, context, it_index)?;
            match &unrolled.ty {
                Type::Predefined(PredefinedType::Scalar(_)) => UnrolledExpression {
                    ty: unrolled.ty.clone(),
                    span: expr.get_span(),
                    data: Rc::new(UnrolledExpressionData::Negate(unrolled))
                },
                t => return Err(ScriptError::InvalidOperandType { error_span: expr.get_span(), got: (t.clone(), op.rhs.get_span()), op: String::from("-") }),
            }
        },
        SimpleExpression::Parenthised(expr) => unroll_expression(&expr.content, context, it_index)?,
    })
}

fn unroll_binop(lhs: UnrolledExpression, op: &BinaryOperator, rhs: UnrolledExpression) -> Result<UnrolledExpression, ScriptError> {
    let lhs = match &lhs.ty {
        Type::Predefined(pre) => match pre {
            PredefinedType::Scalar(_) => lhs,
            PredefinedType::PointCollection(2) => unroll_implicit_conversion(
                lhs,
                &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Distance))))
            )?,
            _ => return Err(ScriptError::InvalidOperandType { error_span: lhs.span.join(rhs.span), got: (lhs.ty, lhs.span), op: op.to_string() })
        },
        _ => return Err(ScriptError::InvalidOperandType { error_span: lhs.span.join(rhs.span), got: (lhs.ty, lhs.span), op: op.to_string() })
    };

    let rhs = match &rhs.ty {
        Type::Predefined(pre) => match pre {
            PredefinedType::Scalar(_) => rhs,
            PredefinedType::PointCollection(2) => unroll_implicit_conversion(
                rhs,
                &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Distance))))
            )?,
            _ => return Err(ScriptError::InvalidOperandType { error_span: rhs.span.join(rhs.span), got: (rhs.ty, rhs.span), op: op.to_string() })
        },
        _ => return Err(ScriptError::InvalidOperandType { error_span: rhs.span.join(rhs.span), got: (rhs.ty, rhs.span), op: op.to_string() })
    };
    
    match op {
        BinaryOperator::Add(_) | BinaryOperator::Sub(_) => {
            let lhs = match lhs.ty {
                Type::Predefined(PredefinedType::Scalar(None)) => unroll_implicit_conversion(lhs, &rhs.ty)?,
                _ => lhs
            };

            let rhs = unroll_implicit_conversion(rhs, &lhs.ty)?;

            Ok(UnrolledExpression {
                ty: lhs.ty.clone(),
                span: lhs.span.join(rhs.span),
                data: Rc::new(match op {
                    BinaryOperator::Add(_) => UnrolledExpressionData::Add(lhs, rhs),
                    BinaryOperator::Sub(_) => UnrolledExpressionData::Subtract(lhs, rhs),
                    _ => unreachable!()
                })
            })
        },
        BinaryOperator::Mul(_) | BinaryOperator::Div(_) => {
            let lhs = match lhs.ty {
                Type::Predefined(PredefinedType::Scalar(None)) => match &rhs.ty {
                    Type::Predefined(PredefinedType::Scalar(None)) => lhs,
                    _ => unroll_implicit_conversion(
                        lhs,
                        &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Scalar))))
                    )?
                },
                _ => lhs
            };

            let rhs = match rhs.ty {
                Type::Predefined(PredefinedType::Scalar(None)) => match &lhs.ty {
                    Type::Predefined(PredefinedType::Scalar(None)) => rhs,
                    _ => unroll_implicit_conversion(
                        rhs,
                        &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Scalar))))
                    )?
                },
                _ => rhs
            };

            Ok(UnrolledExpression {
                ty: match &lhs.ty {
                    Type::Predefined(PredefinedType::Scalar(None)) => lhs.ty.clone(),
                    Type::Predefined(PredefinedType::Scalar(Some(left_unit))) => {
                        if let Type::Predefined(PredefinedType::Scalar(Some(right_unit))) = &rhs.ty {
                            Type::Predefined(PredefinedType::Scalar(Some(left_unit.clone() * right_unit.clone())))
                        } else {
                            unreachable!()
                        }
                    },
                    _ => unreachable!()
                },
                span: lhs.span.join(rhs.span),
                data: Rc::new(match op {
                    BinaryOperator::Mul(_) => UnrolledExpressionData::Multiply(lhs, rhs),
                    BinaryOperator::Div(_) => UnrolledExpressionData::Divide(lhs, rhs),
                    _ => unreachable!()
                })
            })
        },
    }
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
fn unroll_expression(
    expr: &Expression,
    context: &CompileContext,
    it_index: usize,
) -> Result<UnrolledExpression, ScriptError> {
    match expr {
        Expression::Simple(simple) => unroll_simple(
            match simple.len() {
                1 => &simple.first,
                _ => simple.get(it_index).unwrap(),
            },
            context,
            it_index,
        ),
        Expression::Binop(op) => {
            let lhs = unroll_expression(&op.lhs, context, it_index)?;
            let rhs = unroll_expression(&op.rhs, context, it_index)?;

            unroll_binop(lhs, &op.operator, rhs)
        },
    }
}

/// Unpacks the expressed type as a point collection.
fn unpack_expression(expr: &UnrolledExpression, _context: &CompileContext) -> Result<Vec<UnrolledExpression>, ScriptError> {
    match &expr.ty {
        Type::Predefined(pre) => match pre {
            PredefinedType::Point => Ok(vec![expr.clone()]),
            PredefinedType::PointCollection(l) => Ok((0..*l).map(
                |i| UnrolledExpression {
                    data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), i)),
                    ty: Type::Predefined(PredefinedType::Point),
                    span: expr.span,
                }
            ).collect()),
            ty => Err(ScriptError::cannot_unpack(expr.span, Type::Predefined(ty.clone()))),
        },
        Type::Defined => Err(ScriptError::feature_not_supported(expr.span, "unpack_custom_type")),
        Type::Undefined => Err(ScriptError::cannot_unpack(expr.span, Type::Undefined)),
    }
}

fn unroll_let(stat: &LetStatement, context: &mut CompileContext, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    let max_iter_len = stat.ident.len();

    if !check_expression_iterators_hint(&stat.expr, max_iter_len) {
        return Err(ScriptError::inconsistent_iterators(stat.expr.get_span()));
    }

    let rhs_type = stat.expr.get_type(context)?;
    let mut variables = Vec::new();

    // Iterate over each identifier.
    for (i, ident) in stat.ident.iter().enumerate() {
        let rhs_unrolled = unroll_expression(&stat.expr, context, i)?;

        match ident {
            Ident::Named(named) => {
                match context.variables.entry(named.ident.clone()) {
                    // If the variable already exists, it's a redefinition error.
                    Entry::Occupied(entry) => {
                        return Err(ScriptError::redefined_variable(
                            entry.get().definition_span,
                            stat.get_span(),
                            entry.key().clone(),
                        ))
                    }
                    // Otherwise, create a new variable
                    Entry::Vacant(entry) => {
                        let var = Variable {
                            name: entry.key().clone(),
                            definition_span: stat.get_span(),
                            meta: match &rhs_type {
                                Type::Predefined(pre) => match pre {
                                    PredefinedType::Point => {
                                        VariableMeta::Point(Point { meta: None })
                                    }
                                    PredefinedType::Line => VariableMeta::Line,
                                    PredefinedType::Scalar(_) => VariableMeta::Scalar,
                                    PredefinedType::PointCollection(l) => {
                                        if *l == 1 {
                                            VariableMeta::Point(Point { meta: None })
                                        } else {
                                            return Err(ScriptError::collection_not_infered(
                                                stat.get_span(),
                                                stat.expr
                                                    .as_simple()
                                                    .unwrap()
                                                    .first
                                                    .as_ident()
                                                    .unwrap()
                                                    .as_collection()
                                                    .unwrap()
                                                    .clone(),
                                            ));
                                        }
                                    }
                                },
                                Type::Defined => VariableMeta::Properties,
                                Type::Undefined => {
                                    return Err(ScriptError::undefined_type_variable(
                                        stat.get_span(),
                                    ))
                                }
                            },
                            definition: rhs_unrolled,
                        };

                        let v = Rc::new(var);
                        variables.push(Rc::clone(&v));
                        entry.insert(v);
                    }
                }
            }
            Ident::Collection(col) => {
                let rhs_unpacked = unpack_expression(&rhs_unrolled, context)?;

                if rhs_unpacked.len() != col.len() {
                    return Err(ScriptError::CannotUnpack { error_span: rhs_unrolled.span, ty: rhs_unrolled.ty });
                }

                let mut rhs_unpacked = rhs_unpacked.into_iter();
                for pt in &col.collection {
                    let id = construct_point_id(pt.0, pt.1);

                    match context.points.entry(id) {
                        // If the variable already exists, it's a redefinition error.
                        Entry::Occupied(entry) => {
                            return Err(ScriptError::redefined_variable(
                                entry.get().definition_span,
                                stat.get_span(),
                                construct_point_name(pt.0, pt.1),
                            ))
                        }
                        // Otherwise, create a new variable
                        Entry::Vacant(entry) => {
                            let var = Variable {
                                name: construct_point_name(pt.0, pt.1),
                                definition_span: stat.get_span(),
                                meta: VariableMeta::Point(Point { meta: Some(PointMeta {
                                    letter: pt.0,
                                    primes: pt.1,
                                    index: None
                                }) }),
                                definition: rhs_unpacked.next().unwrap()
                            };
    
                            let var = Rc::new(var);
                            context.variables.insert(var.name.clone(), Rc::clone(&var));
                            variables.push(Rc::clone(&var));
                            entry.insert(var);
                        }
                    }
                }
            },
        }
    }

    let mut var_it = variables.into_iter();

    match max_iter_len {
        1 => {
            // There is no iterator in lhs.
            let lhs_unrolled = match stat.ident.get(0).unwrap() {
                Ident::Named(named) => UnrolledExpression {
                    data: Rc::new(UnrolledExpressionData::VariableAccess(var_it.next().unwrap())),
                    ty: rhs_type,
                    span: named.span,
                },
                Ident::Collection(col) => UnrolledExpression {
                    ty: Type::Predefined(PredefinedType::PointCollection(col.len())),
                    span: col.span,
                    data: Rc::new(UnrolledExpressionData::PointCollection(
                        var_it.take(col.len()).collect()
                    ))
                },
            };

            let mut max_iter_len = max_iter_len;

            for (rule, expr) in &stat.rules {
                match max_iter_len {
                    1 => max_iter_len = check_expression_iterators(expr)?,
                    _ => if !check_expression_iterators_hint(expr, max_iter_len) {
                        return Err(ScriptError::inconsistent_iterators(expr.get_span()))
                    }
                }

                for i in 0..max_iter_len {
                    unroll_rule(lhs_unrolled.clone(), rule, unroll_expression(expr, context, i)?, context, unrolled)?;
                }
            }
        },
        _ => {
            let var_it_ref = &mut var_it;

            // There is an iterator in lhs.
            let lhs_unrolled: Vec<UnrolledExpression> = (0..max_iter_len).map(
                |i| match stat.ident.get(i).unwrap() {
                    Ident::Named(named) => UnrolledExpression {
                        data: Rc::new(UnrolledExpressionData::VariableAccess(var_it_ref.next().unwrap())),
                        ty: rhs_type.clone(),
                        span: named.span,
                    },
                    Ident::Collection(col) => UnrolledExpression {
                        ty: Type::Predefined(PredefinedType::PointCollection(col.len())),
                        span: col.span,
                        data: Rc::new(UnrolledExpressionData::PointCollection(
                            var_it_ref.take(col.len()).collect()
                        ))
                    },
                }
            ).collect();

            for (rule, expr) in &stat.rules {
                if !check_expression_iterators_hint(expr, max_iter_len) {
                    return Err(ScriptError::inconsistent_iterators(expr.get_span()))
                }

                for (i, lhs) in lhs_unrolled.iter().enumerate() {
                    unroll_rule(lhs.clone(), rule, unroll_expression(expr, context, i)?, context, unrolled)?;
                }
            }
        }
    }

    Ok(())
}

fn unroll_eq(lhs: UnrolledExpression, rhs: UnrolledExpression, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    if lhs.ty == Type::Predefined(PredefinedType::PointCollection(2)) && rhs.ty == Type::Predefined(PredefinedType::PointCollection(2)) {
        // AB = CD must have different logic as it's implied that this means "equality of distances".
        unrolled.push(UnrolledRule {
            kind: UnrolledRuleKind::Eq,
            lhs: unroll_implicit_conversion(lhs, &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Distance)))))?,
            rhs: unroll_implicit_conversion(rhs, &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Distance)))))?,
            inverted: false,
        });

        Ok(())
    } else {
        let (mut lhs, mut rhs) = (lhs, rhs);
        // If any of the two types can be cast onto the other, cast and compare.
        if rhs.ty.can_cast(&lhs.ty) {
            rhs = unroll_implicit_conversion(rhs, &lhs.ty)?;
        } else if lhs.ty.can_cast(&rhs.ty) {
            lhs = unroll_implicit_conversion(lhs, &rhs.ty)?;
        } else {
            return Err(ScriptError::InconsistentTypes { expected: (lhs.ty, lhs.span), got: (rhs.ty, rhs.span) })
        }

        unrolled.push(UnrolledRule {
            kind: UnrolledRuleKind::Eq,
            lhs,
            rhs,
            inverted: false,
        });

        Ok(())
    }
}

fn unroll_gt(lhs: UnrolledExpression, rhs: UnrolledExpression, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    let left_unit = match &lhs.ty {
        Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
        Type::Predefined(PredefinedType::Scalar(None)) => None,
        Type::Predefined(PredefinedType::PointCollection(2)) => Some(ComplexUnit::new(SimpleUnit::Distance)),
        _ => return Err(ScriptError::InvalidOperandType { error_span: lhs.span.join(rhs.span), got: (lhs.ty.clone(), lhs.span), op: String::from(">") })
    };

    if let Some(ltype) = left_unit {
        if rhs.ty.can_cast(&Type::Predefined(PredefinedType::Scalar(Some(ltype.clone())))) {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(lhs, &Type::Predefined(PredefinedType::Scalar(Some(ltype.clone()))))?,
                rhs: unroll_implicit_conversion(rhs, &Type::Predefined(PredefinedType::Scalar(Some(ltype))))?,
                inverted: false
            })
        } else {
            return Err(ScriptError::InconsistentTypes { expected: (lhs.ty, lhs.span), got: (rhs.ty, rhs.span) })
        }
    } else {
        let right_unit = match &rhs.ty {
            Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
            Type::Predefined(PredefinedType::Scalar(None)) => None,
            Type::Predefined(PredefinedType::PointCollection(2)) => Some(ComplexUnit::new(SimpleUnit::Distance)),
            _ => return Err(ScriptError::InvalidOperandType { error_span: lhs.span.join(rhs.span), got: (rhs.ty.clone(), rhs.span), op: String::from(">") })
        };

        if let Some(rtype) = right_unit {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(lhs, &Type::Predefined(PredefinedType::Scalar(Some(rtype.clone()))))?,
                rhs: unroll_implicit_conversion(rhs, &Type::Predefined(PredefinedType::Scalar(Some(rtype))))?,
                inverted: false
            });
        } else {
            let common = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Scalar))));
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(lhs, &common)?,
                rhs: unroll_implicit_conversion(rhs, &common)?,
                inverted: false
            });
        }
    }
        
    Ok(())
}

fn unroll_lt(lhs: UnrolledExpression, rhs: UnrolledExpression, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    let left_unit = match &lhs.ty {
        Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
        Type::Predefined(PredefinedType::Scalar(None)) => None,
        Type::Predefined(PredefinedType::PointCollection(2)) => Some(ComplexUnit::new(SimpleUnit::Distance)),
        _ => return Err(ScriptError::InvalidOperandType { error_span: lhs.span.join(rhs.span), got: (lhs.ty.clone(), lhs.span), op: String::from("<") })
    };

    if let Some(ltype) = left_unit {
        if rhs.ty.can_cast(&Type::Predefined(PredefinedType::Scalar(Some(ltype.clone())))) {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(lhs, &Type::Predefined(PredefinedType::Scalar(Some(ltype.clone()))))?,
                rhs: unroll_implicit_conversion(rhs, &Type::Predefined(PredefinedType::Scalar(Some(ltype))))?,
                inverted: false
            })
        } else {
            return Err(ScriptError::InconsistentTypes { expected: (lhs.ty, lhs.span), got: (rhs.ty, rhs.span) })
        }
    } else {
        let right_unit = match &rhs.ty {
            Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
            Type::Predefined(PredefinedType::Scalar(None)) => None,
            Type::Predefined(PredefinedType::PointCollection(2)) => Some(ComplexUnit::new(SimpleUnit::Distance)),
            _ => return Err(ScriptError::InvalidOperandType { error_span: lhs.span.join(rhs.span), got: (rhs.ty.clone(), rhs.span), op: String::from("<") })
        };

        if let Some(rtype) = right_unit {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(lhs, &Type::Predefined(PredefinedType::Scalar(Some(rtype.clone()))))?,
                rhs: unroll_implicit_conversion(rhs, &Type::Predefined(PredefinedType::Scalar(Some(rtype))))?,
                inverted: false
            });
        } else {
            let common = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(SimpleUnit::Scalar))));
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(lhs, &common)?,
                rhs: unroll_implicit_conversion(rhs, &common)?,
                inverted: false
            });
        }
    }
        
    Ok(())
}

fn unroll_invert(lhs: UnrolledExpression, op: &RuleOperator, rhs: UnrolledExpression, context: &mut CompileContext, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    let mut unrolled_content = Vec::new();
    unroll_rule(lhs, op, rhs, context, &mut unrolled_content)?;

    unrolled.extend(unrolled_content.into_iter().map(|mut x| {x.inverted = !x.inverted; x}));

    Ok(())
}

fn unroll_gteq(lhs: UnrolledExpression, rhs: UnrolledExpression, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    let mut unrolled_content = Vec::new();
    unroll_lt(lhs, rhs, &mut unrolled_content)?;

    unrolled.extend(unrolled_content.into_iter().map(|mut x| {x.inverted = !x.inverted; x}));

    Ok(())
}

fn unroll_lteq(lhs: UnrolledExpression, rhs: UnrolledExpression, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    let mut unrolled_content = Vec::new();
    unroll_gt(lhs, rhs, &mut unrolled_content)?;

    unrolled.extend(unrolled_content.into_iter().map(|mut x| {x.inverted = !x.inverted; x}));

    Ok(())
}

fn unroll_rule(lhs: UnrolledExpression, op: &RuleOperator, rhs: UnrolledExpression, context: &mut CompileContext, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    match op {
        RuleOperator::Predefined(pre) => match pre {
            PredefinedRuleOperator::Eq(_) => unroll_eq(lhs, rhs, unrolled),
            PredefinedRuleOperator::Lt(_) => unroll_lt(lhs, rhs, unrolled),
            PredefinedRuleOperator::Gt(_) => unroll_gt(lhs, rhs, unrolled),
            PredefinedRuleOperator::Lteq(_) => unroll_lteq(lhs, rhs, unrolled),
            PredefinedRuleOperator::Gteq(_) => unroll_gteq(lhs, rhs, unrolled),
        },
        RuleOperator::Defined(_) => Err(ScriptError::feature_not_supported(op.get_span(), "custom_rule_operators")),
        RuleOperator::Inverted(op) => unroll_invert(lhs, &op.operator, rhs, context, unrolled),
    }
}

fn unroll_rulestat(rule: &RuleStatement, context: &mut CompileContext, unrolled: &mut Vec<UnrolledRule>) -> Result<(), ScriptError> {
    let max_iter_len = check_expression_iterators(&rule.lhs)?;

    match max_iter_len {
        1 => {
            // There is no iterator in lhs.
            let lhs_unrolled = unroll_expression(&rule.lhs, context, 0)?;

            for i in 0..check_expression_iterators(&rule.rhs)? {
                unroll_rule(lhs_unrolled.clone(), &rule.op, unroll_expression(&rule.rhs, context, i)?, context, unrolled)?;
            }
        },
        _ => {
            // There is an iterator in lhs.
            match check_expression_iterators(&rule.rhs)? {
                1 => {
                    let rhs_unrolled = unroll_expression(&rule.rhs, context, 0)?;

                    for i in 0..max_iter_len {
                        unroll_rule(unroll_expression(&rule.lhs, context, i)?, &rule.op, rhs_unrolled.clone(), context, unrolled)?;
                    }
                },
                l => {
                    if l != max_iter_len {
                        return Err(ScriptError::inconsistent_iterators(rule.rhs.get_span()))
                    }

                    for i in 0..max_iter_len {
                        unroll_rule(
                            unroll_expression(&rule.lhs, context, i)?,
                            &rule.op,
                            unroll_expression(&rule.rhs, context, i)?,
                            context, unrolled
                        )?;
                    }
                }
            }
        }
    }

    Ok(())
}

pub fn unroll(input: String) -> Result<(Vec<UnrolledRule>, CompileContext), ScriptError> {
    // Unfortunately, due to how context-dependent geoscript is, the code must be compiled immediately after parsing.
    let mut context = CompileContext {
        rule_ops: HashMap::new(),
        variables: HashMap::new(),
        points: HashMap::new(),
        functions: HashMap::new(),
    };

    builtins::point::register_point_function(&mut context); // Point()
    builtins::dst::register_dst_function(&mut context); // dst()

    let tokens = token::tokenize(input)?;
    let mut it = tokens.iter().peekable();

    let mut unrolled = Vec::new();

    let mut next = it.peek().is_some();
    while next {
        let stat = Statement::parse(&mut it, &context)?;

        // Compile the statement
        match stat {
            Statement::Noop(_) => (),
            Statement::Let(stat) => unroll_let(&stat, &mut context, &mut unrolled)?,
            Statement::Rule(stat) => unroll_rulestat(&stat, &mut context, &mut unrolled)?,
        }

        // println!("Context: {:#?}\nUnrolled: {:#?}", context, unrolled);

        next = it.peek().is_some();
    }

    Ok((unrolled, context))
}
