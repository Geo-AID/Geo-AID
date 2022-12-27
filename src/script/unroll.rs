use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Display,
    rc::Rc,
};

use super::{
    builtins,
    parser::{
        BinaryOperator, Expression, GetType, LetStatement, Parse, PredefinedRuleOperator,
        PredefinedType, RuleOperator, RuleStatement, SimpleExpression, Statement, Type,
    },
    token::{self, Ident, NamedIdent, PointCollection, Span},
    ComplexUnit, Error, SimpleUnit,
};

/// A definition for a user-defined rule operator.
#[derive(Debug)]
pub struct RuleOperatorDefinition {
    /// Operator's name.
    pub name: String,
}

/// Meta info about a point.
#[derive(Debug)]
pub struct PointMeta {
    /// The letter of a point.
    pub letter: char,
    /// The count of `'` in the point's name.
    pub primes: u8,
    /// The point index.
    pub index: Option<u16>,
}

/// A point in variable meta.
#[derive(Debug)]
pub struct Point {
    /// A point meta is optional, since not every point has a letter.
    pub meta: Option<PointMeta>,
}

/// Defines meta information about variables, mostly in regard to the displaying of them.
#[derive(Debug)]
pub enum VariableMeta {
    /// Point variable
    Point(Point),
    Scalar,
    Line,
    PointCollection,
    /// Properties for user-defined types.
    Properties,
}

/// A variable created with a let statement.
#[derive(Debug)]
pub struct Variable {
    /// Variable's name
    pub name: String,
    /// Variable's definition span.
    pub definition_span: Span,
    /// Variable's definition.
    pub definition: UnrolledExpression,
    /// Variable's metadata.
    pub meta: VariableMeta,
}

impl GetType for Variable {
    fn get_type(&self, _: &CompileContext) -> Result<Type, Error> {
        Ok(self.definition.ty.clone())
    }

    fn match_type(&self, context: &CompileContext, t: &Type) -> Result<(), Error> {
        let vartype = self.get_type(context)?;

        if &vartype == t {
            Ok(())
        } else {
            Err(Error::InvalidType {
                expected: t.clone(),
                got: (vartype, self.definition_span),
            })
        }
    }
}

/// An overload of a function in `GeoScript`.
#[derive(Debug)]
pub struct FunctionOverload {
    /// The parameter types.
    pub params: Vec<Type>,
    /// The returned type
    pub returned_type: Type,
    /// The definition span (if there is one).
    pub definition_span: Option<Span>,
    /// The definition.
    pub definition: UnrolledExpression,
}

/// A function.
#[derive(Debug)]
pub struct Function {
    /// Function's name
    pub name: String,
    /// Function's overloads.
    pub overloads: Vec<FunctionOverload>,
}

impl Function {
    /// Checks if the given params can be converted into the expected params.
    #[must_use]
    pub fn match_params(expected: &Vec<Type>, received: &Vec<Type>) -> bool {
        if expected.len() == received.len() {
            for (i, param) in expected.iter().enumerate() {
                if !received[i].can_cast(param) {
                    return false;
                }
            }

            true
        } else {
            false
        }
    }

    /// Tries to find an overload for the given param types.
    #[must_use]
    pub fn get_overload(&self, params: &Vec<Type>) -> Option<&FunctionOverload> {
        self.overloads
            .iter()
            .find(|x| Function::match_params(&x.params, params))
    }

    #[must_use]
    pub fn get_returned(&self, params: &Vec<Type>) -> Type {
        self.get_overload(params)
            .map_or(Type::Undefined, |x| x.returned_type.clone())
    }
}

/// The context of compilation process. It's necessary since `GeoScript` is context-dependent.
#[derive(Debug)]
pub struct CompileContext {
    /// The rule operators.
    pub rule_ops: HashMap<String, Rc<RuleOperatorDefinition>>,
    /// Variables
    pub variables: HashMap<String, Rc<Variable>>,
    /// Points
    pub points: HashMap<u64, Rc<Variable>>,
    /// Functions
    pub functions: HashMap<String, Function>,
}

/// Finds the common length of all iterators.
///
/// # Errors
/// Returns an error if the iterators are of different lengths.
fn check_expression_iterators(
    expr: &Expression,
    full_span: Span,
) -> Result<Option<(usize, Span)>, Error> {
    match expr {
        Expression::Simple(s) => match s.collection.len() {
            0 => check_simple_iterators(&s.first, full_span),
            l => Ok(Some((l + 1, s.get_span()))),
        },
        Expression::Binop(e) => match check_expression_iterators(&e.lhs, full_span)? {
            Some((hint, hint_span)) => {
                match_expression_iterators(&e.rhs, hint, hint_span, full_span)
                    .map(|_| Some((hint, hint_span)))
            }
            None => check_expression_iterators(&e.rhs, full_span),
        },
    }
}

/// Finds the common length of all iterators.
///
/// # Errors
/// Returns an error if the iterators are of different lengths.
fn check_simple_iterators(
    expr: &SimpleExpression,
    full_span: Span,
) -> Result<Option<(usize, Span)>, Error> {
    match expr {
        SimpleExpression::Number(_) | SimpleExpression::Ident(_) => Ok(None),
        SimpleExpression::Call(e) => {
            let mut iters = None;

            if let Some(params) = &e.params {
                for param in params.iter() {
                    match iters {
                        None => iters = check_expression_iterators(param, full_span)?,
                        Some((hint, hint_span)) => {
                            match_expression_iterators(param, hint, hint_span, full_span)?;
                        }
                    }
                }
            }

            Ok(iters)
        }
        SimpleExpression::Unop(e) => check_simple_iterators(e.rhs.as_ref(), full_span),
        SimpleExpression::Parenthised(p) => {
            check_expression_iterators(p.content.as_ref(), full_span)
        }
    }
}

/// Checks if all iterators are of given length.
fn match_simple_iterators(
    expr: &SimpleExpression,
    hint: usize,
    hint_span: Span,
    full_span: Span,
) -> Result<(), Error> {
    match expr {
        SimpleExpression::Number(_) | SimpleExpression::Ident(_) => Ok(()),
        SimpleExpression::Call(e) => {
            if let Some(params) = &e.params {
                for param in params.iter() {
                    match_expression_iterators(param.as_ref(), hint, hint_span, full_span)?;
                }
            }

            Ok(())
        }
        SimpleExpression::Unop(e) => {
            match_simple_iterators(e.rhs.as_ref(), hint, hint_span, full_span)
        }
        SimpleExpression::Parenthised(e) => {
            match_expression_iterators(e.content.as_ref(), hint, hint_span, full_span)
        }
    }
}

/// Checks if all iterators are of given length.
fn match_expression_iterators(
    expr: &Expression,
    hint: usize,
    hint_span: Span,
    full_span: Span,
) -> Result<(), Error> {
    match expr {
        Expression::Simple(s) => match s.collection.len() {
            0 => match_simple_iterators(&s.first, hint, hint_span, full_span),
            l => {
                if l + 1 == hint {
                    Ok(())
                } else {
                    Err(Error::InconsistentIterators {
                        first_span: hint_span,
                        first_length: hint,
                        occured_span: s.get_span(),
                        occured_length: l + 1,
                        error_span: full_span,
                    })
                }
            }
        },
        Expression::Binop(e) => {
            match_expression_iterators(e.lhs.as_ref(), hint, hint_span, full_span)?;
            match_expression_iterators(e.rhs.as_ref(), hint, hint_span, full_span)
        }
    }
}

/// The kind on the unrolled rule.
#[derive(Debug)]
pub enum UnrolledRuleKind {
    Eq,
    Gt,
    Lt,
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
    ThreePointAngle(UnrolledExpression, UnrolledExpression, UnrolledExpression),
    TwoLineAngle(UnrolledExpression, UnrolledExpression),
}

impl Display for UnrolledExpressionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnrolledExpressionData::VariableAccess(name) => write!(f, "{}", name.name),
            UnrolledExpressionData::PointCollection(col) => write!(
                f,
                "col({})",
                col.iter()
                    .map(|v| v.name.clone())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            UnrolledExpressionData::Number(num) => write!(f, "{num}"),
            UnrolledExpressionData::FreePoint => write!(f, "Point"),
            UnrolledExpressionData::Boxed(expr) | UnrolledExpressionData::SetUnit(expr, _) => {
                write!(f, "{expr}")
            }
            UnrolledExpressionData::Parameter(index) => write!(f, "${index}"),
            UnrolledExpressionData::IndexCollection(expr, index) => write!(f, "{expr}[{index}]"),
            UnrolledExpressionData::LineFromPoints(e1, e2) => write!(f, "line({e1}, {e2})"),
            UnrolledExpressionData::PointPointDistance(e1, e2)
            | UnrolledExpressionData::PointLineDistance(e1, e2) => write!(f, "dst({e1}, {e2})"),
            UnrolledExpressionData::Negate(e) => write!(f, "-{e}"),
            UnrolledExpressionData::Add(e1, e2) => write!(f, "{e1} + {e2}"),
            UnrolledExpressionData::Multiply(e1, e2) => write!(f, "{e1} * {e2}"),
            UnrolledExpressionData::Divide(e1, e2) => write!(f, "{e1} / {e2}"),
            UnrolledExpressionData::Subtract(e1, e2) => write!(f, "{e1} - {e2}"),
            UnrolledExpressionData::ThreePointAngle(e1, e2, e3) => {
                write!(f, "angle({e1}, {e2}, {e3})")
            }
            UnrolledExpressionData::TwoLineAngle(e1, e2) => write!(f, "angle({e1}, {e2})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnrolledExpression {
    pub data: Rc<UnrolledExpressionData>,
    pub ty: Type,
    pub span: Span,
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
    pub inverted: bool,
}

impl Display for UnrolledRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}{} {}",
            self.lhs,
            if self.inverted { "!" } else { "" },
            match self.kind {
                UnrolledRuleKind::Eq => "=",
                UnrolledRuleKind::Gt => ">",
                UnrolledRuleKind::Lt => "<",
            },
            self.rhs
        )
    }
}

/// A point id is a 64-bit integer. First 32 bits are for the letter codepoint, next 8 for the amount of primes, next 16 for the point index.
fn construct_point_id(letter: char, primes: u8) -> u64 {
    ((letter as u64) << 8) | u64::from(primes)
}

/// Constructs the point name based on the letter and the primes.
fn construct_point_name(letter: char, primes: u8) -> String {
    String::from(letter) + &"'".repeat(primes as usize)
}

/// Replaces all Parameter unrolled expressions with the given parameters.
fn unroll_parameters(
    definition: &UnrolledExpression,
    params: &Vec<UnrolledExpression>,
) -> UnrolledExpression {
    UnrolledExpression {
        ty: definition.ty.clone(),
        span: definition.span,
        data: Rc::new(match definition.data.as_ref() {
            UnrolledExpressionData::Boxed(expr) | UnrolledExpressionData::Negate(expr) => {
                UnrolledExpressionData::Boxed(unroll_parameters(expr, params))
            }
            UnrolledExpressionData::Parameter(index) => {
                UnrolledExpressionData::Boxed(params[*index].clone())
            }
            UnrolledExpressionData::IndexCollection(expr, index) => {
                UnrolledExpressionData::IndexCollection(unroll_parameters(expr, params), *index)
            }
            UnrolledExpressionData::LineFromPoints(e1, e2) => {
                UnrolledExpressionData::LineFromPoints(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                )
            }
            UnrolledExpressionData::SetUnit(expr, unit) => {
                UnrolledExpressionData::SetUnit(unroll_parameters(expr, params), unit.clone())
            }
            UnrolledExpressionData::PointPointDistance(e1, e2) => {
                UnrolledExpressionData::PointPointDistance(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                )
            }
            UnrolledExpressionData::PointLineDistance(e1, e2) => {
                UnrolledExpressionData::PointLineDistance(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                )
            }
            UnrolledExpressionData::VariableAccess(_)
            | UnrolledExpressionData::PointCollection(_)
            | UnrolledExpressionData::Number(_)
            | UnrolledExpressionData::FreePoint => definition.data.as_ref().clone(),
            UnrolledExpressionData::Add(e1, e2) => UnrolledExpressionData::Add(
                unroll_parameters(e1, params),
                unroll_parameters(e2, params),
            ),
            UnrolledExpressionData::Subtract(e1, e2) => UnrolledExpressionData::Subtract(
                unroll_parameters(e1, params),
                unroll_parameters(e2, params),
            ),
            UnrolledExpressionData::Multiply(e1, e2) => UnrolledExpressionData::Multiply(
                unroll_parameters(e1, params),
                unroll_parameters(e2, params),
            ),
            UnrolledExpressionData::Divide(e1, e2) => UnrolledExpressionData::Divide(
                unroll_parameters(e1, params),
                unroll_parameters(e2, params),
            ),
            UnrolledExpressionData::ThreePointAngle(e1, e2, e3) => {
                UnrolledExpressionData::ThreePointAngle(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                    unroll_parameters(e3, params),
                )
            }
            UnrolledExpressionData::TwoLineAngle(e1, e2) => UnrolledExpressionData::TwoLineAngle(
                unroll_parameters(e1, params),
                unroll_parameters(e2, params),
            ),
        }),
    }
}

/// Unrolls the conversion of a point collection into the given type.
fn unroll_pc_conversion(
    expr: &UnrolledExpression,
    to: &Type,
    collection_length: usize,
) -> Result<UnrolledExpression, Error> {
    match collection_length {
        1 => {
            if to == &Type::Predefined(PredefinedType::Point) {
                Ok(UnrolledExpression {
                    data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), 0)),
                    ty: Type::Predefined(PredefinedType::Point),
                    span: expr.span,
                })
            } else {
                Err(Error::implicit_conversion_does_not_exist(
                    expr.span,
                    expr.ty.clone(),
                    to.clone(),
                ))
            }
        }
        2 => {
            if to == &Type::Predefined(PredefinedType::Line) {
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
                        },
                    )),
                    ty: Type::Predefined(PredefinedType::Line),
                    span: expr.span,
                })
            } else {
                Err(Error::implicit_conversion_does_not_exist(
                    expr.span,
                    expr.ty.clone(),
                    to.clone(),
                ))
            }
        }
        _ => Err(Error::implicit_conversion_does_not_exist(
            expr.span,
            expr.ty.clone(),
            to.clone(),
        )),
    }
}

/// Unrolls the conversion of the given expression of type scalar(none) to a scalar type.
fn unroll_conversion_to_scalar(
    expr: &UnrolledExpression,
    to: &Type,
) -> Result<UnrolledExpression, Error> {
    match expr.data.as_ref() {
        UnrolledExpressionData::Boxed(x) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::new(UnrolledExpressionData::Boxed(unroll_implicit_conversion(
                x.clone(),
                to,
            )?)),
        }),
        UnrolledExpressionData::Number(_) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::clone(&expr.data),
        }),
        UnrolledExpressionData::Negate(x) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::new(UnrolledExpressionData::Negate(unroll_implicit_conversion(
                x.clone(),
                to,
            )?)),
        }),
        UnrolledExpressionData::Add(e1, e2) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::new(UnrolledExpressionData::Add(
                unroll_implicit_conversion(e1.clone(), to)?,
                unroll_implicit_conversion(e2.clone(), to)?,
            )),
        }),
        UnrolledExpressionData::Subtract(e1, e2) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::new(UnrolledExpressionData::Subtract(
                unroll_implicit_conversion(e1.clone(), to)?,
                unroll_implicit_conversion(e2.clone(), to)?,
            )),
        }),
        UnrolledExpressionData::Multiply(e1, e2) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::new(UnrolledExpressionData::Multiply(
                unroll_implicit_conversion(e1.clone(), to)?,
                unroll_implicit_conversion(
                    e2.clone(),
                    &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                        SimpleUnit::Scalar,
                    )))),
                )?,
            )),
        }),
        UnrolledExpressionData::Divide(e1, e2) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::new(UnrolledExpressionData::Divide(
                unroll_implicit_conversion(e1.clone(), to)?,
                unroll_implicit_conversion(
                    e2.clone(),
                    &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                        SimpleUnit::Scalar,
                    )))),
                )?,
            )),
        }),
        UnrolledExpressionData::VariableAccess(_)
        | UnrolledExpressionData::PointCollection(_)
        | UnrolledExpressionData::FreePoint
        | UnrolledExpressionData::Parameter(_)
        | UnrolledExpressionData::IndexCollection(_, _)
        | UnrolledExpressionData::LineFromPoints(_, _)
        | UnrolledExpressionData::SetUnit(_, _)
        | UnrolledExpressionData::PointPointDistance(_, _)
        | UnrolledExpressionData::PointLineDistance(_, _)
        | UnrolledExpressionData::ThreePointAngle(_, _, _)
        | UnrolledExpressionData::TwoLineAngle(_, _) => unreachable!(
            "This data should not be of type scalar(none) and yet is: {:#?}",
            expr.data
        ),
    }
}

/// Unrolls implicit conversions between types.
fn unroll_implicit_conversion(
    expr: UnrolledExpression,
    to: &Type,
) -> Result<UnrolledExpression, Error> {
    if to == &expr.ty {
        Ok(expr)
    } else {
        match &expr.ty {
            Type::Predefined(pre) => match pre {
                PredefinedType::PointCollection(l) => unroll_pc_conversion(&expr, to, *l),
                PredefinedType::Scalar(None) => {
                    match to {
                        Type::Predefined(PredefinedType::Scalar(unit)) => match unit {
                            Some(unit) => {
                                if unit.0[3] == 0 {
                                    // no angle
                                    unroll_conversion_to_scalar(&expr, to)
                                } else {
                                    Err(Error::implicit_conversion_does_not_exist(
                                        expr.span,
                                        expr.ty,
                                        to.clone(),
                                    ))
                                }
                            }
                            None => unroll_conversion_to_scalar(&expr, to),
                        },
                        _ => Err(Error::implicit_conversion_does_not_exist(
                            expr.span,
                            expr.ty,
                            to.clone(),
                        )),
                    }
                }
                _ => Err(Error::implicit_conversion_does_not_exist(
                    expr.span,
                    expr.ty,
                    to.clone(),
                )),
            },
            _ => Err(Error::implicit_conversion_does_not_exist(
                expr.span,
                expr.ty,
                to.clone(),
            )),
        }
    }
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
fn unroll_simple(
    expr: &SimpleExpression,
    context: &CompileContext,
    it_index: usize,
) -> Result<UnrolledExpression, Error> {
    Ok(match expr {
        SimpleExpression::Ident(i) => match i {
            Ident::Named(named) => {
                let var = context.variables.get(&named.ident).ok_or_else(|| {
                    Error::UndefinedVariable {
                        error_span: expr.get_span(),
                        variable_name: named.ident.clone(),
                    }
                })?;

                UnrolledExpression {
                    ty: var.get_type(context)?,
                    data: Rc::new(UnrolledExpressionData::VariableAccess(Rc::clone(var))),
                    span: named.span,
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
                                None => Err(Error::UndefinedVariable {
                                    error_span: col.span,
                                    variable_name: construct_point_name(*letter, *primes),
                                }),
                            }
                        })
                        .collect::<Result<Vec<Rc<Variable>>, Error>>()?,
                )),
                span: col.span,
            },
        },
        SimpleExpression::Number(num) => UnrolledExpression {
            ty: Type::Predefined(PredefinedType::Scalar(None)),
            data: Rc::new(UnrolledExpressionData::Number(num.value)),
            span: num.get_span(),
        },
        SimpleExpression::Call(e) => {
            if let Some(func) = context.functions.get(&e.name.ident) {
                let params = match &e.params {
                    Some(params) => params
                        .iter()
                        .map(|p| unroll_expression(p, context, it_index))
                        .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
                    None => Vec::new(),
                };

                let param_types = params.iter().map(|ex| ex.ty.clone()).collect();

                if let Some(overload) = func.get_overload(&param_types) {
                    let params = params
                        .into_iter()
                        .enumerate()
                        .map(|(i, param)| unroll_implicit_conversion(param, &overload.params[i]))
                        .collect::<Result<Vec<UnrolledExpression>, Error>>()?;

                    UnrolledExpression {
                        ty: overload.returned_type.clone(),
                        data: Rc::new(UnrolledExpressionData::Boxed(unroll_parameters(
                            &overload.definition,
                            &params,
                        ))),
                        span: e.get_span(),
                    }
                } else {
                    return Err(Error::overload_not_found(
                        e.get_span(),
                        e.name.ident.clone(),
                        param_types,
                    ));
                }
            } else {
                return Err(Error::undefined_function(e.name.span, e.name.ident.clone()));
            }
        }
        SimpleExpression::Unop(op) => {
            let unrolled = unroll_simple(&op.rhs, context, it_index)?;
            match &unrolled.ty {
                Type::Predefined(PredefinedType::Scalar(_)) => UnrolledExpression {
                    ty: unrolled.ty.clone(),
                    span: expr.get_span(),
                    data: Rc::new(UnrolledExpressionData::Negate(unrolled)),
                },
                t => {
                    return Err(Error::InvalidOperandType {
                        error_span: expr.get_span(),
                        got: (t.clone(), op.rhs.get_span()),
                        op: String::from("-"),
                    })
                }
            }
        }
        SimpleExpression::Parenthised(expr) => unroll_expression(&expr.content, context, it_index)?,
    })
}

fn unroll_muldiv(
    this: UnrolledExpression,
    other: &UnrolledExpression,
) -> Result<UnrolledExpression, Error> {
    match this.ty {
        Type::Predefined(PredefinedType::Scalar(None)) => match &other.ty {
            Type::Predefined(PredefinedType::Scalar(None)) => Ok(this),
            _ => unroll_implicit_conversion(
                this,
                &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Scalar,
                )))),
            ),
        },
        _ => Ok(this),
    }
}

fn unroll_binop(
    lhs: UnrolledExpression,
    op: &BinaryOperator,
    rhs: UnrolledExpression,
) -> Result<UnrolledExpression, Error> {
    let lhs = match &lhs.ty {
        Type::Predefined(pre) => match pre {
            PredefinedType::Scalar(_) => lhs,
            PredefinedType::PointCollection(2) => unroll_implicit_conversion(
                lhs,
                &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Distance,
                )))),
            )?,
            _ => {
                return Err(Error::InvalidOperandType {
                    error_span: lhs.span.join(rhs.span),
                    got: (lhs.ty, lhs.span),
                    op: op.to_string(),
                })
            }
        },
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: lhs.span.join(rhs.span),
                got: (lhs.ty, lhs.span),
                op: op.to_string(),
            })
        }
    };

    let rhs = match &rhs.ty {
        Type::Predefined(pre) => match pre {
            PredefinedType::Scalar(_) => rhs,
            PredefinedType::PointCollection(2) => unroll_implicit_conversion(
                rhs,
                &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Distance,
                )))),
            )?,
            _ => {
                return Err(Error::InvalidOperandType {
                    error_span: rhs.span.join(rhs.span),
                    got: (rhs.ty, rhs.span),
                    op: op.to_string(),
                })
            }
        },
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: rhs.span.join(rhs.span),
                got: (rhs.ty, rhs.span),
                op: op.to_string(),
            })
        }
    };

    match op {
        BinaryOperator::Add(_) | BinaryOperator::Sub(_) => {
            let lhs = match lhs.ty {
                Type::Predefined(PredefinedType::Scalar(None)) => {
                    unroll_implicit_conversion(lhs, &rhs.ty)?
                }
                _ => lhs,
            };

            let rhs = unroll_implicit_conversion(rhs, &lhs.ty)?;

            Ok(UnrolledExpression {
                ty: lhs.ty.clone(),
                span: lhs.span.join(rhs.span),
                data: Rc::new(match op {
                    BinaryOperator::Add(_) => UnrolledExpressionData::Add(lhs, rhs),
                    BinaryOperator::Sub(_) => UnrolledExpressionData::Subtract(lhs, rhs),
                    _ => unreachable!(),
                }),
            })
        }
        BinaryOperator::Mul(_) | BinaryOperator::Div(_) => {
            let lhs = unroll_muldiv(lhs, &rhs)?;

            let rhs = unroll_muldiv(rhs, &lhs)?;

            Ok(UnrolledExpression {
                ty: match &lhs.ty {
                    Type::Predefined(PredefinedType::Scalar(None)) => lhs.ty.clone(),
                    Type::Predefined(PredefinedType::Scalar(Some(left_unit))) => {
                        if let Type::Predefined(PredefinedType::Scalar(Some(right_unit))) = &rhs.ty
                        {
                            Type::Predefined(PredefinedType::Scalar(Some(
                                left_unit.clone() * right_unit.clone(),
                            )))
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!(),
                },
                span: lhs.span.join(rhs.span),
                data: Rc::new(match op {
                    BinaryOperator::Mul(_) => UnrolledExpressionData::Multiply(lhs, rhs),
                    BinaryOperator::Div(_) => UnrolledExpressionData::Divide(lhs, rhs),
                    _ => unreachable!(),
                }),
            })
        }
    }
}

/// Unrolls the given expression based on the given iterator index. The index is assumed valid and an out-of-bounds access leads to a panic!().
fn unroll_expression(
    expr: &Expression,
    context: &CompileContext,
    it_index: usize,
) -> Result<UnrolledExpression, Error> {
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
        }
    }
}

/// Unpacks the expressed type as a point collection.
fn unpack_expression(
    expr: &UnrolledExpression,
    _context: &CompileContext,
) -> Result<Vec<UnrolledExpression>, Error> {
    match &expr.ty {
        Type::Predefined(pre) => match pre {
            PredefinedType::Point => Ok(vec![expr.clone()]),
            PredefinedType::PointCollection(l) => Ok((0..*l)
                .map(|i| UnrolledExpression {
                    data: Rc::new(UnrolledExpressionData::IndexCollection(expr.clone(), i)),
                    ty: Type::Predefined(PredefinedType::Point),
                    span: expr.span,
                })
                .collect()),
            ty => Err(Error::cannot_unpack(
                expr.span,
                Type::Predefined(ty.clone()),
            )),
        },
        Type::Defined => Err(Error::feature_not_supported(
            expr.span,
            "unpack_custom_type",
        )),
        Type::Undefined => Err(Error::cannot_unpack(expr.span, Type::Undefined)),
    }
}

fn create_variable_named(
    stat: &LetStatement,
    context: &mut CompileContext,
    named: &NamedIdent,
    rhs_type: &Type,
    rhs_unrolled: UnrolledExpression,
    variables: &mut Vec<Rc<Variable>>,
) -> Result<(), Error> {
    match context.variables.entry(named.ident.clone()) {
        // If the variable already exists, it's a redefinition error.
        Entry::Occupied(entry) => Err(Error::redefined_variable(
            entry.get().definition_span,
            stat.get_span(),
            entry.key().clone(),
        )),
        // Otherwise, create a new variable
        Entry::Vacant(entry) => {
            let var = Variable {
                name: entry.key().clone(),
                definition_span: stat.get_span(),
                meta: match &rhs_type {
                    Type::Predefined(pre) => match pre {
                        PredefinedType::Point => VariableMeta::Point(Point { meta: None }),
                        PredefinedType::Line => VariableMeta::Line,
                        PredefinedType::Scalar(_) => VariableMeta::Scalar,
                        PredefinedType::PointCollection(l) => {
                            if *l == 1 {
                                VariableMeta::Point(Point { meta: None })
                            } else {
                                VariableMeta::PointCollection
                            }
                        }
                    },
                    Type::Defined => VariableMeta::Properties,
                    Type::Undefined => return Err(Error::undefined_type_variable(stat.get_span())),
                },
                definition: rhs_unrolled,
            };

            let v = Rc::new(var);
            variables.push(Rc::clone(&v));
            entry.insert(v);

            Ok(())
        }
    }
}

/// If the lhs of let statement is a point collection, the rhs has to be unpacked.
fn create_variable_collection(
    stat: &LetStatement,
    context: &mut CompileContext,
    col: &PointCollection,
    rhs_unrolled: UnrolledExpression,
    variables: &mut Vec<Rc<Variable>>,
) -> Result<(), Error> {
    let rhs_unpacked = unpack_expression(&rhs_unrolled, context)?;

    if rhs_unpacked.len() != col.len() {
        return Err(Error::CannotUnpack {
            error_span: rhs_unrolled.span,
            ty: rhs_unrolled.ty,
        });
    }

    let mut rhs_unpacked = rhs_unpacked.into_iter();
    for pt in &col.collection {
        let id = construct_point_id(pt.0, pt.1);

        match context.points.entry(id) {
            // If the variable already exists, it's a redefinition error.
            Entry::Occupied(entry) => {
                return Err(Error::redefined_variable(
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
                    meta: VariableMeta::Point(Point {
                        meta: Some(PointMeta {
                            letter: pt.0,
                            primes: pt.1,
                            index: None,
                        }),
                    }),
                    definition: rhs_unpacked.next().unwrap(),
                };

                let var = Rc::new(var);
                context.variables.insert(var.name.clone(), Rc::clone(&var));
                variables.push(Rc::clone(&var));
                entry.insert(var);
            }
        }
    }

    Ok(())
}

fn create_variables(
    stat: &LetStatement,
    context: &mut CompileContext,
) -> Result<(Vec<Rc<Variable>>, Type), Error> {
    let rhs_type = stat.expr.get_type(context)?;
    let mut variables = Vec::new();

    // Iterate over each identifier.
    for (i, ident) in stat.ident.iter().enumerate() {
        let rhs_unrolled = unroll_expression(&stat.expr, context, i)?;

        match ident {
            Ident::Named(named) => create_variable_named(
                stat,
                context,
                named,
                &rhs_type,
                rhs_unrolled,
                &mut variables,
            )?,
            Ident::Collection(col) => {
                create_variable_collection(stat, context, col, rhs_unrolled, &mut variables)?;
            }
        }
    }

    Ok((variables, rhs_type))
}

fn unroll_let(
    stat: &LetStatement,
    context: &mut CompileContext,
    unrolled: &mut Vec<UnrolledRule>,
) -> Result<(), Error> {
    let max_iter_len = stat.ident.len();

    match match_expression_iterators(
        &stat.expr,
        max_iter_len,
        stat.ident.get_span(),
        stat.ident.get_span().join(stat.expr.get_span()),
    ) {
        Ok(_) => (),
        Err(err) => {
            if max_iter_len == 1 {
                return Err(Error::LetStatUnexpectedIterator {
                    var_span: stat.ident.get_span(),
                    error_span: err.inconsistent_iterators_get_span().unwrap(),
                });
            }

            return Err(err);
        }
    }

    let (variables, rhs_type) = create_variables(stat, context)?;
    let mut var_it = variables.into_iter();

    if max_iter_len == 1 {
        // There is no iterator in lhs.
        let lhs_unrolled = match stat.ident.get(0).unwrap() {
            Ident::Named(named) => UnrolledExpression {
                data: Rc::new(UnrolledExpressionData::VariableAccess(
                    var_it.next().unwrap(),
                )),
                ty: rhs_type,
                span: named.span,
            },
            Ident::Collection(col) => UnrolledExpression {
                ty: Type::Predefined(PredefinedType::PointCollection(col.len())),
                span: col.span,
                data: Rc::new(UnrolledExpressionData::PointCollection(
                    var_it.take(col.len()).collect(),
                )),
            },
        };

        let mut max_iter_len = match max_iter_len {
            1 => None,
            l => Some((l, stat.ident.get_span())),
        };

        for (rule, expr) in &stat.rules {
            match max_iter_len {
                None => max_iter_len = check_expression_iterators(expr, stat.get_span())?,
                Some((hint, hint_span)) => {
                    match_expression_iterators(expr, hint, hint_span, stat.get_span())?;
                }
            }

            let l = match max_iter_len {
                None => 1,
                Some((hint, _)) => hint,
            };

            for i in 0..l {
                unroll_rule(
                    lhs_unrolled.clone(),
                    rule,
                    unroll_expression(expr, context, i)?,
                    context,
                    unrolled,
                    stat.get_span(),
                )?;
            }
        }
    } else {
        let var_it_ref = &mut var_it;

        // There is an iterator in lhs.
        let lhs_unrolled: Vec<UnrolledExpression> = (0..max_iter_len)
            .map(|i| match stat.ident.get(i).unwrap() {
                Ident::Named(named) => UnrolledExpression {
                    data: Rc::new(UnrolledExpressionData::VariableAccess(
                        var_it_ref.next().unwrap(),
                    )),
                    ty: rhs_type.clone(),
                    span: named.span,
                },
                Ident::Collection(col) => UnrolledExpression {
                    ty: Type::Predefined(PredefinedType::PointCollection(col.len())),
                    span: col.span,
                    data: Rc::new(UnrolledExpressionData::PointCollection(
                        var_it_ref.take(col.len()).collect(),
                    )),
                },
            })
            .collect();

        for (rule, expr) in &stat.rules {
            match_expression_iterators(expr, max_iter_len, stat.ident.get_span(), stat.get_span())?;

            for (i, lhs) in lhs_unrolled.iter().enumerate() {
                unroll_rule(
                    lhs.clone(),
                    rule,
                    unroll_expression(expr, context, i)?,
                    context,
                    unrolled,
                    stat.get_span(),
                )?;
            }
        }
    }

    Ok(())
}

fn unroll_eq(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    unrolled: &mut Vec<UnrolledRule>,
    full_span: Span,
) -> Result<(), Error> {
    if lhs.ty == Type::Predefined(PredefinedType::PointCollection(2))
        && rhs.ty == Type::Predefined(PredefinedType::PointCollection(2))
    {
        // AB = CD must have different logic as it's implied that this means "equality of distances".
        unrolled.push(UnrolledRule {
            kind: UnrolledRuleKind::Eq,
            lhs: unroll_implicit_conversion(
                lhs,
                &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Distance,
                )))),
            )?,
            rhs: unroll_implicit_conversion(
                rhs,
                &Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                    SimpleUnit::Distance,
                )))),
            )?,
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
            return Err(Error::InconsistentTypes {
                expected: (lhs.ty, Box::new(lhs.span)),
                got: (rhs.ty, Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });
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

fn unroll_gt(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    unrolled: &mut Vec<UnrolledRule>,
    full_span: Span,
) -> Result<(), Error> {
    let left_unit = match &lhs.ty {
        Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
        Type::Predefined(PredefinedType::Scalar(None)) => None,
        Type::Predefined(PredefinedType::PointCollection(2)) => {
            Some(ComplexUnit::new(SimpleUnit::Distance))
        }
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: full_span,
                got: (lhs.ty.clone(), lhs.span),
                op: String::from(">"),
            })
        }
    };

    if let Some(ltype) = left_unit {
        if rhs
            .ty
            .can_cast(&Type::Predefined(PredefinedType::Scalar(Some(
                ltype.clone(),
            ))))
        {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(ltype.clone()))),
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(ltype))),
                )?,
                inverted: false,
            });
        } else {
            return Err(Error::InconsistentTypes {
                expected: (lhs.ty, Box::new(lhs.span)),
                got: (rhs.ty, Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });
        }
    } else {
        let right_unit = match &rhs.ty {
            Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
            Type::Predefined(PredefinedType::Scalar(None)) => None,
            Type::Predefined(PredefinedType::PointCollection(2)) => {
                Some(ComplexUnit::new(SimpleUnit::Distance))
            }
            _ => {
                return Err(Error::InvalidOperandType {
                    error_span: lhs.span.join(rhs.span),
                    got: (rhs.ty.clone(), rhs.span),
                    op: String::from(">"),
                })
            }
        };

        if let Some(rtype) = right_unit {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(rtype.clone()))),
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(rtype))),
                )?,
                inverted: false,
            });
        } else {
            let common = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                SimpleUnit::Scalar,
            ))));
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Gt,
                lhs: unroll_implicit_conversion(lhs, &common)?,
                rhs: unroll_implicit_conversion(rhs, &common)?,
                inverted: false,
            });
        }
    }

    Ok(())
}

fn unroll_lt(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    unrolled: &mut Vec<UnrolledRule>,
    full_span: Span,
) -> Result<(), Error> {
    let left_unit = match &lhs.ty {
        Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
        Type::Predefined(PredefinedType::Scalar(None)) => None,
        Type::Predefined(PredefinedType::PointCollection(2)) => {
            Some(ComplexUnit::new(SimpleUnit::Distance))
        }
        _ => {
            return Err(Error::InvalidOperandType {
                error_span: lhs.span.join(rhs.span),
                got: (lhs.ty.clone(), lhs.span),
                op: String::from("<"),
            })
        }
    };

    if let Some(ltype) = left_unit {
        if rhs
            .ty
            .can_cast(&Type::Predefined(PredefinedType::Scalar(Some(
                ltype.clone(),
            ))))
        {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(ltype.clone()))),
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(ltype))),
                )?,
                inverted: false,
            });
        } else {
            return Err(Error::InconsistentTypes {
                expected: (lhs.ty, Box::new(lhs.span)),
                got: (rhs.ty, Box::new(rhs.span)),
                error_span: Box::new(full_span),
            });
        }
    } else {
        let right_unit = match &rhs.ty {
            Type::Predefined(PredefinedType::Scalar(Some(unit))) => Some(unit.clone()),
            Type::Predefined(PredefinedType::Scalar(None)) => None,
            Type::Predefined(PredefinedType::PointCollection(2)) => {
                Some(ComplexUnit::new(SimpleUnit::Distance))
            }
            _ => {
                return Err(Error::InvalidOperandType {
                    error_span: full_span,
                    got: (rhs.ty.clone(), rhs.span),
                    op: String::from("<"),
                })
            }
        };

        if let Some(rtype) = right_unit {
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(
                    lhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(rtype.clone()))),
                )?,
                rhs: unroll_implicit_conversion(
                    rhs,
                    &Type::Predefined(PredefinedType::Scalar(Some(rtype))),
                )?,
                inverted: false,
            });
        } else {
            let common = Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                SimpleUnit::Scalar,
            ))));
            unrolled.push(UnrolledRule {
                kind: UnrolledRuleKind::Lt,
                lhs: unroll_implicit_conversion(lhs, &common)?,
                rhs: unroll_implicit_conversion(rhs, &common)?,
                inverted: false,
            });
        }
    }

    Ok(())
}

fn unroll_invert(
    lhs: UnrolledExpression,
    op: &RuleOperator,
    rhs: UnrolledExpression,
    context: &mut CompileContext,
    unrolled: &mut Vec<UnrolledRule>,
    full_span: Span,
) -> Result<(), Error> {
    let mut unrolled_content = Vec::new();
    unroll_rule(lhs, op, rhs, context, &mut unrolled_content, full_span)?;

    unrolled.extend(unrolled_content.into_iter().map(|mut x| {
        x.inverted = !x.inverted;
        x
    }));

    Ok(())
}

fn unroll_gteq(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    unrolled: &mut Vec<UnrolledRule>,
    full_span: Span,
) -> Result<(), Error> {
    let mut unrolled_content = Vec::new();
    unroll_lt(lhs, rhs, &mut unrolled_content, full_span)?;

    unrolled.extend(unrolled_content.into_iter().map(|mut x| {
        x.inverted = !x.inverted;
        x
    }));

    Ok(())
}

fn unroll_lteq(
    lhs: UnrolledExpression,
    rhs: UnrolledExpression,
    unrolled: &mut Vec<UnrolledRule>,
    full_span: Span,
) -> Result<(), Error> {
    let mut unrolled_content = Vec::new();
    unroll_gt(lhs, rhs, &mut unrolled_content, full_span)?;

    unrolled.extend(unrolled_content.into_iter().map(|mut x| {
        x.inverted = !x.inverted;
        x
    }));

    Ok(())
}

fn unroll_rule(
    lhs: UnrolledExpression,
    op: &RuleOperator,
    rhs: UnrolledExpression,
    context: &mut CompileContext,
    unrolled: &mut Vec<UnrolledRule>,
    full_span: Span,
) -> Result<(), Error> {
    match op {
        RuleOperator::Predefined(pre) => match pre {
            PredefinedRuleOperator::Eq(_) => unroll_eq(lhs, rhs, unrolled, full_span),
            PredefinedRuleOperator::Lt(_) => unroll_lt(lhs, rhs, unrolled, full_span),
            PredefinedRuleOperator::Gt(_) => unroll_gt(lhs, rhs, unrolled, full_span),
            PredefinedRuleOperator::Lteq(_) => unroll_lteq(lhs, rhs, unrolled, full_span),
            PredefinedRuleOperator::Gteq(_) => unroll_gteq(lhs, rhs, unrolled, full_span),
        },
        RuleOperator::Defined(_) => Err(Error::feature_not_supported(
            op.get_span(),
            "custom_rule_operators",
        )),
        RuleOperator::Inverted(op) => {
            unroll_invert(lhs, &op.operator, rhs, context, unrolled, full_span)
        }
    }
}

fn unroll_rulestat(
    rule: &RuleStatement,
    context: &mut CompileContext,
    unrolled: &mut Vec<UnrolledRule>,
) -> Result<(), Error> {
    let max_iter_len = check_expression_iterators(&rule.lhs, rule.get_span())?;

    match max_iter_len {
        None => {
            // There is no iterator in lhs.
            let lhs_unrolled = unroll_expression(&rule.lhs, context, 0)?;

            for i in 0..check_expression_iterators(&rule.rhs, rule.get_span())?.map_or(1, |x| x.0) {
                unroll_rule(
                    lhs_unrolled.clone(),
                    &rule.op,
                    unroll_expression(&rule.rhs, context, i)?,
                    context,
                    unrolled,
                    rule.get_span(),
                )?;
            }
        }
        Some((hint, hint_span)) => {
            // There is an iterator in lhs.
            match check_expression_iterators(&rule.rhs, rule.get_span())? {
                None => {
                    let rhs_unrolled = unroll_expression(&rule.rhs, context, 0)?;

                    for i in 0..hint {
                        unroll_rule(
                            unroll_expression(&rule.lhs, context, i)?,
                            &rule.op,
                            rhs_unrolled.clone(),
                            context,
                            unrolled,
                            rule.get_span(),
                        )?;
                    }
                }
                Some((rhs_hint, rspan)) => {
                    if rhs_hint != hint {
                        return Err(Error::InconsistentIterators {
                            first_span: hint_span,
                            first_length: hint,
                            occured_span: rspan,
                            occured_length: rhs_hint,
                            error_span: rule.get_span(),
                        });
                    }

                    for i in 0..hint {
                        unroll_rule(
                            unroll_expression(&rule.lhs, context, i)?,
                            &rule.op,
                            unroll_expression(&rule.rhs, context, i)?,
                            context,
                            unrolled,
                            rule.get_span(),
                        )?;
                    }
                }
            }
        }
    }

    Ok(())
}

/// Unrolls the given script. All iterators are expanded and all conversions applied. The output can be immediately compiled.
///
/// # Errors
/// Specific error descriptions are in `ScriptError` documentation.
pub fn unroll(input: &str) -> Result<(Vec<UnrolledRule>, CompileContext), Error> {
    // Unfortunately, due to how context-dependent geoscript is, the code must be compiled immediately after parsing.
    let mut context = CompileContext {
        rule_ops: HashMap::new(),
        variables: HashMap::new(),
        points: HashMap::new(),
        functions: HashMap::new(),
    };

    builtins::point::register_point_function(&mut context); // Point()
    builtins::dst::register_dst_function(&mut context); // dst()
    builtins::angle::register_angle_function(&mut context); // angle()
    builtins::degrees::register_degrees_function(&mut context); // degrees()
    builtins::radians::register_radians_function(&mut context); // radians()

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
