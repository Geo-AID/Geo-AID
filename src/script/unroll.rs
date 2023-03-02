use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Display,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use super::{
    builtins,
    parser::{
        BinaryOperator, ExplicitIterator, Expression, FlagStatement, ImplicitIterator,
        LetStatement, Parse, PredefinedRuleOperator, PredefinedType, Punctuated, RuleOperator,
        RuleStatement, SimpleExpression, Statement, Type,
    },
    token::{self, Ident, NamedIdent, PointCollection, Span},
    ty, ComplexUnit, Error, SimpleUnit,
};

/// A definition for a user-defined rule operator.
#[derive(Debug)]
pub struct RuleOperatorDefinition {
    /// Operator's name.
    pub name: String,
}

/// Meta info about a point.
#[derive(Debug, Clone)]
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

impl VariableMeta {
    #[must_use]
    pub fn as_point(&self) -> Option<&Point> {
        if let Self::Point(v) = self {
            Some(v)
        } else {
            None
        }
    }
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

pub type FlagSet = HashMap<String, Flag>;

pub struct FlagSetConstructor {
    pub flags: Vec<(String, Flag)>,
}

impl FlagSetConstructor {
    #[must_use]
    pub fn new() -> Self {
        Self { flags: Vec::new() }
    }

    #[must_use]
    pub fn add_ident<S: ToString>(mut self, name: &S) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Unset),
                ty: FlagType::String,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_ident_def<S: ToString>(mut self, name: &S, default: &S) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Default(FlagValue::String(
                    default.to_string(),
                ))),
                ty: FlagType::String,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_bool<S: ToString>(mut self, name: &S) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Unset),
                ty: FlagType::Boolean,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_bool_def<S: ToString>(mut self, name: &S, default: bool) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Setting(FlagSetting::Default(FlagValue::Bool(default))),
                ty: FlagType::Boolean,
            },
        ));

        self
    }

    #[must_use]
    pub fn add_set<S: ToString>(mut self, name: &S, set: FlagSetConstructor) -> Self {
        self.flags.push((
            name.to_string(),
            Flag {
                name: name.to_string(),
                kind: FlagKind::Set(set.finish()),
                ty: FlagType::Set,
            },
        ));

        self
    }

    #[must_use]
    pub fn finish(self) -> FlagSet {
        self.flags.into_iter().collect()
    }
}

impl Default for FlagSetConstructor {
    fn default() -> Self {
        Self::new()
    }
}

/// A compiler flag.
#[derive(Debug)]
pub struct Flag {
    pub name: String,
    pub kind: FlagKind,
    pub ty: FlagType,
}

impl Flag {
    #[must_use]
    pub fn as_set(&self) -> Option<&FlagSet> {
        match &self.kind {
            FlagKind::Setting(_) => None,
            FlagKind::Set(set) => Some(set),
        }
    }

    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_value().and_then(FlagValue::as_bool).copied(),
            FlagKind::Set(_) => None,
        }
    }

    #[must_use]
    pub fn as_ident(&self) -> Option<&String> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_value().and_then(FlagValue::as_string),
            FlagKind::Set(_) => None,
        }
    }

    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        match &self.kind {
            FlagKind::Setting(setting) => setting.get_span(),
            FlagKind::Set(_) => None,
        }
    }
}

#[derive(Debug)]
pub enum FlagType {
    Set,
    Boolean,
    String,
}

/// A compiler flag.
#[derive(Debug)]
pub enum FlagKind {
    Setting(FlagSetting),
    Set(FlagSet),
}

/// A compiler flag value.
#[derive(Debug)]
pub enum FlagSetting {
    Default(FlagValue),
    Unset,
    Set(FlagValue, Span),
}

impl FlagSetting {
    #[must_use]
    pub fn get_value(&self) -> Option<&FlagValue> {
        match self {
            FlagSetting::Default(v) | FlagSetting::Set(v, _) => Some(v),
            FlagSetting::Unset => None,
        }
    }

    #[must_use]
    pub fn get_span(&self) -> Option<Span> {
        match self {
            FlagSetting::Default(_)
            | FlagSetting::Unset => None,
            FlagSetting::Set(_, sp) => Some(*sp),
        }
    }
}

/// A compiler flag value.
#[derive(Debug)]
pub enum FlagValue {
    String(String),
    Bool(bool),
}

impl FlagValue {
    #[must_use]
    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_string(&self) -> Option<&String> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
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
    /// Possible parameter group
    pub param_group: Option<Type>,
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
    ///
    /// # Panics
    /// Never. Shut up, clippy.
    #[must_use]
    pub fn match_params(
        expected: &Vec<Type>,
        received: &Vec<Type>,
        param_group: Option<&Type>,
    ) -> bool {
        if let Some(ty) = param_group {
            if expected.len() < received.len() {
                let mut received = received.iter();

                for param in expected {
                    if !received.next().unwrap().can_cast(param) {
                        return false;
                    }
                }

                for param in received {
                    if !param.can_cast(ty) {
                        return false;
                    }
                }

                true
            } else {
                false
            }
        } else if expected.len() == received.len() {
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
            .find(|x| Function::match_params(&x.params, params, x.param_group.as_ref()))
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
    /// Flags
    pub flags: FlagSet,
}

/// Represents complicated iterator structures.
#[derive(Debug)]
pub struct IterTree {
    pub variants: Vec<IterNode>,
    pub id: u8,
    pub span: Span,
}

/// A single node of `IterTree`.
#[derive(Debug)]
pub struct IterNode(Vec<IterTree>);

impl Deref for IterNode {
    type Target = Vec<IterTree>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for IterNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<const ITER: bool> From<&Expression<ITER>> for IterNode {
    fn from(value: &Expression<ITER>) -> Self {
        match value {
            Expression::ImplicitIterator(it) => IterNode::new(vec![it.into()]),
            Expression::Single(expr) => expr.as_ref().into(),
            Expression::Binop(binop) => Self::from2(binop.lhs.as_ref(), binop.rhs.as_ref()),
        }
    }
}

impl From<&SimpleExpression> for IterNode {
    fn from(value: &SimpleExpression) -> Self {
        match value {
            SimpleExpression::Ident(_) | SimpleExpression::Number(_) => IterNode::new(Vec::new()),
            SimpleExpression::Call(expr) => match &expr.params {
                Some(params) => IterNode::new(
                    params
                        .iter()
                        .flat_map(|v| IterNode::from(v).0.into_iter())
                        .collect(),
                ),
                None => IterNode::new(Vec::new()),
            },
            SimpleExpression::Unop(expr) => expr.rhs.as_ref().into(),
            SimpleExpression::Parenthised(expr) => expr.content.as_ref().into(),
            SimpleExpression::ExplicitIterator(it) => IterNode::new(vec![it.into()]),
            SimpleExpression::PointCollection(col) => IterNode::new(
                col.points
                    .iter()
                    .flat_map(|v| IterNode::from(v).0.into_iter())
                    .collect(),
            ),
        }
    }
}

impl From<&ImplicitIterator> for IterTree {
    fn from(value: &ImplicitIterator) -> Self {
        Self {
            id: 0, // Implicit iterators have an id of 0.
            variants: value.exprs.iter().map(IterNode::from).collect(),
            span: value.get_span(),
        }
    }
}

impl From<&ExplicitIterator> for IterTree {
    fn from(value: &ExplicitIterator) -> Self {
        Self {
            id: value.id,
            variants: value.exprs.iter().map(IterNode::from).collect(),
            span: value.get_span(),
        }
    }
}

impl IterTree {
    /// # Errors
    /// Returns an error when there is an inconsistency in among iterators (same id, different lengths)
    /// or when an iterator with id x contains an iterator with id x.
    pub fn get_iter_lengths(
        &self,
        lengths: &mut HashMap<u8, (usize, Span, Option<Span>)>,
        full_span: Span,
    ) -> Result<(), Error> {
        for variant in &self.variants {
            variant.get_iter_lengths(lengths, full_span)?;
        }

        Ok(())
    }
}

impl IterNode {
    #[must_use]
    pub fn new(content: Vec<IterTree>) -> Self {
        Self(content)
    }

    #[must_use]
    pub fn from2<const ITER1: bool, const ITER2: bool>(
        e1: &Expression<ITER1>,
        e2: &Expression<ITER2>,
    ) -> Self {
        let mut node = Self::from(e1);
        node.extend(Self::from(e2).0.into_iter());
        node
    }

    /// # Panics
    /// Never.
    ///
    /// # Errors
    /// Returns an error when there is an inconsistency in among iterators (same id, different lengths)
    /// or when an iterator with id x contains an iterator with id x.
    pub fn get_iter_lengths(
        &self,
        lengths: &mut HashMap<u8, (usize, Span, Option<Span>)>,
        full_span: Span,
    ) -> Result<(), Error> {
        for iter in &self.0 {
            match lengths.entry(iter.id) {
                Entry::Vacant(entry) => {
                    entry.insert((iter.variants.len(), iter.span, Some(iter.span)));
                }
                Entry::Occupied(mut entry) => {
                    if entry.get().0 != iter.variants.len() {
                        return Err(Error::InconsistentIterators {
                            first_span: entry.get().1,
                            first_length: entry.get().0,
                            occured_span: iter.span,
                            occured_length: iter.variants.len(),
                            error_span: full_span,
                        });
                    } else if let Some(parent) = entry.get().2 {
                        return Err(Error::IteratorWithSameIdIterator {
                            error_span: full_span,
                            parent_span: parent,
                            contained_span: iter.span,
                        });
                    }

                    entry.get_mut().2 = Some(iter.span);
                }
            }

            iter.get_iter_lengths(lengths, full_span)?;
            lengths.get_mut(&iter.id).unwrap().2 = None;
        }

        Ok(())
    }
}

/// A range-like iterator, except multidimensional.
#[derive(Debug)]
pub struct MultiRangeIterator {
    maxes: Vec<usize>,
    currents: Vec<usize>,
}

impl MultiRangeIterator {
    #[must_use]
    pub fn new(maxes: Vec<usize>) -> Self {
        let l = maxes.len();

        Self {
            maxes,
            currents: vec![0].repeat(l),
        }
    }

    pub fn increment(&mut self) -> Option<&Vec<usize>> {
        self.increment_place(self.currents.len() - 1)
    }

    fn increment_place(&mut self, at: usize) -> Option<&Vec<usize>> {
        self.currents[at] += 1;

        if self.currents[at] == self.maxes[at] {
            self.currents[at] = 0;
            if at == 0 {
                None
            } else {
                self.increment_place(at - 1)
            }
        } else {
            Some(&self.currents)
        }
    }

    #[must_use]
    pub fn get_currents(&self) -> &Vec<usize> {
        &self.currents
    }
}

#[derive(Debug)]
pub struct IterTreeIterator<'r> {
    /// List of lists of parallel iterators with their ids, current indices and lengths.
    steps: Vec<(Vec<(&'r IterTree, usize)>, MultiRangeIterator)>,
    currents: Option<HashMap<u8, usize>>,
}

impl<'r> IterTreeIterator<'r> {
    #[must_use]
    pub fn new(tree: &'r IterNode) -> Self {
        let mut this = Self {
            steps: Vec::new(),
            currents: Some(HashMap::new()),
        };

        this.add_node(tree);

        this
    }

    fn add_node(&mut self, node: &'r IterNode) {
        if node.len() > 0 {
            let mut visited = Vec::new();
            let mut lengths = Vec::new();

            for tree in node.iter() {
                if !visited.contains(&tree.id) {
                    visited.push(tree.id);
                    lengths.push(tree.variants.len());
                }
            }

            self.steps.push((
                // `visited` will be in order. We can use that.
                node.iter()
                    .map(|v| {
                        (
                            v,
                            visited
                                .iter()
                                .enumerate()
                                .find(|(_, x)| **x == v.id)
                                .unwrap()
                                .0,
                        )
                    })
                    .collect(),
                MultiRangeIterator::new(lengths),
            ));
            self.update_currents();

            self.update_iterators();
        }
    }

    fn update_iterators(&mut self) {
        let nodes = if let Some(node) = self.steps.last() {
            node.0
                .iter()
                .map(|iter| &iter.0.variants[node.1.get_currents()[iter.1]])
                .collect()
        } else {
            Vec::new()
        };

        for node in nodes {
            self.add_node(node);
        }
    }

    fn update_currents(&mut self) {
        let node = self.steps.last().unwrap();
        let currents = node.1.get_currents();

        for v in &node.0 {
            self.currents
                .as_mut()
                .unwrap()
                .entry(v.0.id)
                .and_modify(|x| *x = currents[v.1])
                .or_insert(currents[v.1]);
        }
    }

    pub fn next(&mut self) {
        while let Some(node) = self.steps.last_mut() {
            if node.1.increment().is_some() {
                self.update_currents();
                self.update_iterators();
                return;
            }

            self.steps.pop();
        }

        self.currents = None;
    }

    #[must_use]
    pub fn get_currents(&self) -> Option<&HashMap<u8, usize>> {
        self.currents.as_ref()
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
    PointCollection(Vec<UnrolledExpression>),
    Number(f64),
    FreePoint,
    FreeReal,
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
    AngleBisector(UnrolledExpression, UnrolledExpression, UnrolledExpression),
    Average(Vec<UnrolledExpression>),
    UnrollParameterGroup(usize),
    PerpendicularThrough(UnrolledExpression, UnrolledExpression), // Line, Point
    ParallelThrough(UnrolledExpression, UnrolledExpression),      // Line, Point
    LineLineIntersection(UnrolledExpression, UnrolledExpression),
}

impl Display for UnrolledExpressionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnrolledExpressionData::VariableAccess(name) => write!(f, "{}", name.name),
            UnrolledExpressionData::PointCollection(col) => write!(
                f,
                "col({})",
                col.iter()
                    .map(|v| format!("{v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            UnrolledExpressionData::Average(exprs) => write!(
                f,
                "average({})",
                exprs
                    .iter()
                    .map(|v| format!("{v}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            UnrolledExpressionData::Number(num) => write!(f, "{num}"),
            UnrolledExpressionData::FreePoint => write!(f, "Point"),
            UnrolledExpressionData::FreeReal => write!(f, "Real"),
            UnrolledExpressionData::Boxed(expr) | UnrolledExpressionData::SetUnit(expr, _) => {
                write!(f, "{expr}")
            }
            UnrolledExpressionData::Parameter(index) => write!(f, "${index}"),
            UnrolledExpressionData::UnrollParameterGroup(index) => write!(f, "${index}..."),
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
            UnrolledExpressionData::AngleBisector(e1, e2, e3) => {
                write!(f, "angle-bisector({e1}, {e2}, {e3})")
            }
            UnrolledExpressionData::PerpendicularThrough(l, p) => {
                write!(f, "perpendicular-through({l}, {p})")
            }
            UnrolledExpressionData::ParallelThrough(l, p) => {
                write!(f, "parallel-through({l}, {p})")
            }
            UnrolledExpressionData::LineLineIntersection(l1, l2) => {
                write!(f, "intersection({l1}, {l2})")
            }
        }
    }
}

impl UnrolledExpressionData {
    #[must_use]
    pub fn has_distance_literal(&self, self_span: Span) -> Option<Span> {
        match self {
            UnrolledExpressionData::VariableAccess(_)
            | UnrolledExpressionData::Number(_)
            | UnrolledExpressionData::Parameter(_)
            | UnrolledExpressionData::UnrollParameterGroup(_)
            | UnrolledExpressionData::FreePoint
            | UnrolledExpressionData::FreeReal => None,
            UnrolledExpressionData::PointCollection(v)
            | UnrolledExpressionData::Average(v) => {
                for expr in v {
                    if let Some(sp) = expr.has_distance_literal() {
                        return Some(sp);
                    }
                }

                None
            },
            UnrolledExpressionData::Boxed(v)
            | UnrolledExpressionData::IndexCollection(v, _)
            | UnrolledExpressionData::Negate(v) => v.has_distance_literal(),
            UnrolledExpressionData::SetUnit(v, u) => {
                if let Some(sp) = v.has_distance_literal() {
                    Some(sp)
                } else if u.0[SimpleUnit::Distance as usize] != 0 {
                    Some(self_span)
                } else {
                    None
                }
            }
            UnrolledExpressionData::PointPointDistance(_, _)
            | UnrolledExpressionData::PointLineDistance(_, _) => Some(self_span),
            UnrolledExpressionData::Add(v1, v2)
            | UnrolledExpressionData::LineFromPoints(v1, v2)
            | UnrolledExpressionData::ParallelThrough(v1, v2)
            | UnrolledExpressionData::PerpendicularThrough(v1, v2)
            | UnrolledExpressionData::LineLineIntersection(v1, v2)
            | UnrolledExpressionData::TwoLineAngle(v1, v2)
            | UnrolledExpressionData::Subtract(v1, v2)
            | UnrolledExpressionData::Multiply(v1, v2)
            | UnrolledExpressionData::Divide(v1, v2) =>
                v1.has_distance_literal()
                .or_else(|| v2.has_distance_literal()),
            UnrolledExpressionData::ThreePointAngle(v1, v2, v3)
            | UnrolledExpressionData::AngleBisector(v1, v2, v3) =>
                v1.has_distance_literal()
                .or_else(|| v2.has_distance_literal())
                .or_else(|| v3.has_distance_literal()),
        }
    }
}

pub fn display_vec<T: Display>(v: &[T]) -> String {
    v.iter()
        .map(|x| format!("{x}"))
        .collect::<Vec<String>>()
        .join(", ")
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

impl UnrolledExpression {
    /// # Panics
    /// Never.
    #[must_use]
    pub fn has_distance_literal(&self) -> Option<Span> {
        match self.data.as_ref() {
            UnrolledExpressionData::VariableAccess(_)
            | UnrolledExpressionData::Parameter(_)
            | UnrolledExpressionData::UnrollParameterGroup(_)
            | UnrolledExpressionData::FreePoint
            | UnrolledExpressionData::FreeReal => None,
            UnrolledExpressionData::PointCollection(v)
            | UnrolledExpressionData::Average(v) => {
                for expr in v {
                    if let Some(sp) = expr.has_distance_literal() {
                        return Some(sp);
                    }
                }

                None
            },
            UnrolledExpressionData::Number(_) => {
                if let Some(unit) = self.ty.as_predefined().unwrap().as_scalar().unwrap() {
                    if unit.0[SimpleUnit::Distance as usize] != 0 {
                        return Some(self.span);
                    }
                }

                None
            }
            UnrolledExpressionData::Boxed(v)
            | UnrolledExpressionData::IndexCollection(v, _)
            | UnrolledExpressionData::Negate(v) => v.has_distance_literal(),
            UnrolledExpressionData::SetUnit(v, u) => {
                if let Some(sp) = v.has_distance_literal() {
                    Some(sp)
                } else if u.0[SimpleUnit::Distance as usize] != 0 {
                    Some(self.span)
                } else {
                    None
                }
            }
            UnrolledExpressionData::PointPointDistance(e1, e2)
            | UnrolledExpressionData::PointLineDistance(e1, e2) => e1.has_distance_literal().or_else(|| e2.has_distance_literal()),
            UnrolledExpressionData::Add(v1, v2)
            | UnrolledExpressionData::LineFromPoints(v1, v2)
            | UnrolledExpressionData::ParallelThrough(v1, v2)
            | UnrolledExpressionData::PerpendicularThrough(v1, v2)
            | UnrolledExpressionData::LineLineIntersection(v1, v2)
            | UnrolledExpressionData::TwoLineAngle(v1, v2)
            | UnrolledExpressionData::Subtract(v1, v2)
            | UnrolledExpressionData::Multiply(v1, v2)
            | UnrolledExpressionData::Divide(v1, v2) =>
                v1.has_distance_literal()
                .or_else(|| v2.has_distance_literal()),
            UnrolledExpressionData::ThreePointAngle(v1, v2, v3)
            | UnrolledExpressionData::AngleBisector(v1, v2, v3) =>
                v1.has_distance_literal()
                .or_else(|| v2.has_distance_literal())
                .or_else(|| v3.has_distance_literal()),
        }
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
#[must_use]
pub fn construct_point_name(letter: char, primes: u8) -> String {
    String::from(letter) + &"'".repeat(primes as usize)
}

fn unroll_parameters_vec(
    definition: &Vec<UnrolledExpression>,
    params: &Vec<UnrolledExpression>,
) -> Vec<UnrolledExpression> {
    let mut result = Vec::new();

    for item in definition {
        match item.data.as_ref() {
            UnrolledExpressionData::UnrollParameterGroup(start_at) => {
                result.extend(params.iter().skip(*start_at).cloned());
            }
            _ => result.push(unroll_parameters(item, params)),
        }
    }

    result
}

/// Replaces all Parameter unrolled expressions with the given parameters.
#[allow(clippy::module_name_repetitions)]
#[allow(clippy::too_many_lines)]
#[must_use]
pub fn unroll_parameters(
    definition: &UnrolledExpression,
    params: &Vec<UnrolledExpression>,
) -> UnrolledExpression {
    UnrolledExpression {
        ty: definition.ty.clone(),
        span: definition.span,
        data: Rc::new(match definition.data.as_ref() {
            UnrolledExpressionData::Boxed(expr) => {
                UnrolledExpressionData::Boxed(unroll_parameters(expr, params))
            }
            UnrolledExpressionData::Negate(expr) => {
                UnrolledExpressionData::Negate(unroll_parameters(expr, params))
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
            | UnrolledExpressionData::Number(_)
            | UnrolledExpressionData::FreeReal
            | UnrolledExpressionData::FreePoint => definition.data.as_ref().clone(),
            UnrolledExpressionData::Average(exprs) => {
                UnrolledExpressionData::Average(unroll_parameters_vec(exprs, params))
            }
            UnrolledExpressionData::PointCollection(exprs) => {
                UnrolledExpressionData::PointCollection(unroll_parameters_vec(exprs, params))
            }
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
            UnrolledExpressionData::AngleBisector(e1, e2, e3) => {
                UnrolledExpressionData::AngleBisector(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                    unroll_parameters(e3, params),
                )
            }
            UnrolledExpressionData::PerpendicularThrough(e1, e2) => {
                UnrolledExpressionData::PerpendicularThrough(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                )
            }
            UnrolledExpressionData::ParallelThrough(e1, e2) => {
                UnrolledExpressionData::ParallelThrough(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                )
            }
            UnrolledExpressionData::LineLineIntersection(e1, e2) => {
                UnrolledExpressionData::LineLineIntersection(
                    unroll_parameters(e1, params),
                    unroll_parameters(e2, params),
                )
            }
            UnrolledExpressionData::UnrollParameterGroup(_) => {
                unreachable!("This should never be unrolled as parameter.")
            }
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
        2 => match to {
            Type::Predefined(pre) => match pre {
                PredefinedType::Line => Ok(UnrolledExpression {
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
                }),
                PredefinedType::Scalar(unit) => {
                    if unit == &Some(ComplexUnit::new(SimpleUnit::Distance)) {
                        Ok(UnrolledExpression {
                            data: Rc::new(UnrolledExpressionData::PointPointDistance(
                                UnrolledExpression {
                                    data: Rc::new(UnrolledExpressionData::IndexCollection(
                                        expr.clone(),
                                        0,
                                    )),
                                    ty: Type::Predefined(PredefinedType::Point),
                                    span: expr.span,
                                },
                                UnrolledExpression {
                                    data: Rc::new(UnrolledExpressionData::IndexCollection(
                                        expr.clone(),
                                        1,
                                    )),
                                    ty: Type::Predefined(PredefinedType::Point),
                                    span: expr.span,
                                },
                            )),
                            ty: Type::Predefined(PredefinedType::Scalar(Some(ComplexUnit::new(
                                SimpleUnit::Distance,
                            )))),
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
            },
            _ => Err(Error::implicit_conversion_does_not_exist(
                expr.span,
                expr.ty.clone(),
                to.clone(),
            )),
        },
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
        UnrolledExpressionData::Average(exprs) => Ok(UnrolledExpression {
            ty: to.clone(),
            span: expr.span,
            data: Rc::new(UnrolledExpressionData::Average(
                exprs
                    .iter()
                    .map(|v| unroll_conversion_to_scalar(v, to))
                    .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
            )),
        }),
        UnrolledExpressionData::VariableAccess(_)
        | UnrolledExpressionData::PointCollection(_)
        | UnrolledExpressionData::FreePoint
        | UnrolledExpressionData::FreeReal
        | UnrolledExpressionData::Parameter(_)
        | UnrolledExpressionData::IndexCollection(_, _)
        | UnrolledExpressionData::LineFromPoints(_, _)
        | UnrolledExpressionData::SetUnit(_, _)
        | UnrolledExpressionData::PointPointDistance(_, _)
        | UnrolledExpressionData::PointLineDistance(_, _)
        | UnrolledExpressionData::ThreePointAngle(_, _, _)
        | UnrolledExpressionData::AngleBisector(_, _, _)
        | UnrolledExpressionData::UnrollParameterGroup(_)
        | UnrolledExpressionData::PerpendicularThrough(_, _)
        | UnrolledExpressionData::ParallelThrough(_, _)
        | UnrolledExpressionData::LineLineIntersection(_, _)
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
#[allow(clippy::too_many_lines)]
fn unroll_simple(
    expr: &SimpleExpression,
    context: &CompileContext,
    it_index: &HashMap<u8, usize>,
) -> Result<UnrolledExpression, Error> {
    Ok(match expr {
        SimpleExpression::Ident(i) => match i {
            Ident::Named(named) => {
                let var = context.variables.get(&named.ident).ok_or_else(|| {
                    #[allow(clippy::cast_possible_truncation)]
                    let suggested = context
                        .variables
                        .iter()
                        .max_by_key(|v| (strsim::jaro(v.0, &named.ident) * 1000.0).floor() as i64)
                        .map(|x| x.0)
                        .cloned();

                    Error::UndefinedVariable {
                        error_span: expr.get_span(),
                        variable_name: named.ident.clone(),
                        suggested,
                    }
                })?;

                UnrolledExpression {
                    ty: var.definition.ty.clone(),
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
                                Some(var) => Ok(UnrolledExpression {
                                    data: Rc::new(UnrolledExpressionData::VariableAccess(
                                        Rc::clone(var),
                                    )),
                                    ty: ty::POINT,
                                    span: col.span,
                                }),
                                None => Err(Error::UndefinedVariable {
                                    error_span: col.span,
                                    variable_name: construct_point_name(*letter, *primes),
                                    suggested: None, // Pretty much every single-letter point could be considered similar, so no suggestions.
                                }),
                            }
                        })
                        .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
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
                        .map(|(i, param)| {
                            if i < overload.params.len() {
                                unroll_implicit_conversion(param, &overload.params[i])
                            } else {
                                unroll_implicit_conversion(
                                    param,
                                    overload.param_group.as_ref().unwrap(),
                                )
                            }
                        })
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
                #[allow(clippy::cast_possible_truncation)]
                let suggested = context
                    .functions
                    .iter()
                    .max_by_key(|v| (strsim::jaro(v.0, &e.name.ident) * 1000.0).floor() as i64)
                    .map(|x| x.0)
                    .cloned();

                return Err(Error::UndefinedFunction {
                    error_span: e.name.span,
                    function_name: e.name.ident.clone(),
                    suggested,
                });
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
        SimpleExpression::ExplicitIterator(it) => {
            unroll_expression(it.get(it_index[&it.id]).unwrap(), context, it_index)?
        }
        SimpleExpression::PointCollection(col) => UnrolledExpression {
            ty: ty::collection(col.points.len()),
            span: col.get_span(),
            data: Rc::new(UnrolledExpressionData::PointCollection(
                col.points
                    .iter()
                    .map(|expr| {
                        let unrolled = unroll_expression(expr, context, it_index)?;

                        if unrolled.ty.can_cast(&ty::POINT) {
                            Ok(unroll_implicit_conversion(unrolled, &ty::POINT)?)
                        } else {
                            Err(Error::NonPointInPointCollection {
                                error_span: col.get_span(),
                                received: (unrolled.span, unrolled.ty),
                            })
                        }
                    })
                    .collect::<Result<Vec<UnrolledExpression>, Error>>()?,
            )),
        },
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
            PredefinedType::PointCollection(2) => unroll_implicit_conversion(lhs, &ty::DISTANCE)?,
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
fn unroll_expression<const ITER: bool>(
    expr: &Expression<ITER>,
    context: &CompileContext,
    it_index: &HashMap<u8, usize>,
) -> Result<UnrolledExpression, Error> {
    match expr {
        Expression::Single(simple) => unroll_simple(simple.as_ref(), context, it_index),
        Expression::ImplicitIterator(it) => unroll_simple(
            it.get(it_index[&0]).unwrap(), // Implicit iterators always have an id of 0.
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
                meta: match &rhs_unrolled.ty {
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
) -> Result<Vec<Rc<Variable>>, Error> {
    let mut variables = Vec::new();

    let tree = IterNode::from(&stat.expr);

    let ind = if let Some(iter) = tree.first() {
        if stat.ident.len() == 1 {
            return Err(Error::LetStatUnexpectedIterator {
                var_span: stat.ident.get_span(),
                error_span: iter.span,
            });
        }

        for it in tree.iter() {
            if iter.id != it.id {
                return Err(Error::LetStatMoreThanOneIterator {
                    error_span: stat.expr.get_span(),
                    first_span: iter.span,
                    second_span: it.span,
                });
            }

            for variant in &it.variants {
                if let Some(it2) = variant.first() {
                    return Err(Error::LetStatMoreThanOneIterator {
                        error_span: stat.expr.get_span(),
                        first_span: iter.span,
                        second_span: it2.span,
                    });
                }
            }
        }

        let mut lengths = HashMap::new();
        tree.get_iter_lengths(&mut lengths, stat.expr.get_span())?;
        let entry = lengths.get(&iter.id).unwrap();

        if entry.0 != stat.ident.len() {
            return Err(Error::InconsistentIterators {
                first_span: stat.ident.get_span(),
                first_length: stat.ident.len(),
                occured_span: entry.1,
                occured_length: entry.0,
                error_span: stat.get_span(),
            });
        }

        None
    } else {
        Some(HashMap::new())
    };

    let mut it_index = IterTreeIterator::new(&tree);

    // Iterate over each identifier.
    for ident in stat.ident.iter() {
        let rhs_unrolled = unroll_expression(
            &stat.expr,
            context,
            ind.as_ref()
                .unwrap_or_else(|| it_index.get_currents().unwrap()),
        )?;
        it_index.next();

        match ident {
            Ident::Named(named) => {
                create_variable_named(stat, context, named, rhs_unrolled, &mut variables)?;
            }
            Ident::Collection(col) => {
                create_variable_collection(stat, context, col, rhs_unrolled, &mut variables)?;
            }
        }
    }

    Ok(variables)
}

fn unroll_let(
    stat: &LetStatement,
    context: &mut CompileContext,
    unrolled: &mut Vec<UnrolledRule>,
) -> Result<(), Error> {
    create_variables(stat, context)?;

    // First, we construct an iterator out of lhs
    let lhs: Expression<true> = Expression::ImplicitIterator(ImplicitIterator {
        exprs: Punctuated {
            first: Box::new(SimpleExpression::Ident(stat.ident.first.as_ref().clone())),
            collection: stat
                .ident
                .collection
                .iter()
                .map(|(p, i)| (*p, SimpleExpression::Ident(i.clone())))
                .collect(),
        },
    });

    // Then, we run each rule through a tree iterator.
    for (rule, expr) in &stat.rules {
        let tree = IterNode::from2(&lhs, expr);

        // Check the lengths
        tree.get_iter_lengths(&mut HashMap::new(), stat.get_span())?;

        // And create the index
        let mut index = IterTreeIterator::new(&tree);

        while let Some(it_index) = index.get_currents() {
            unroll_rule(
                unroll_expression(&lhs, context, it_index)?,
                rule,
                unroll_expression(expr, context, it_index)?,
                context,
                unrolled,
                stat.get_span(),
            )?;

            index.next();
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
    if (
            lhs.ty == ty::collection(2)
            && rhs.ty == ty::collection(2)
        ) || (
            lhs.ty == ty::collection(2)
            && rhs.ty == ty::SCALAR_UNKNOWN
        ) || (
            lhs.ty == ty::SCALAR_UNKNOWN
            && rhs.ty == ty::collection(2)
        )
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
    let tree = IterNode::from2(&rule.lhs, &rule.rhs);
    tree.get_iter_lengths(&mut HashMap::new(), rule.get_span())?;

    let mut it_index = IterTreeIterator::new(&tree);

    while let Some(index) = it_index.get_currents() {
        unroll_rule(
            unroll_expression(&rule.lhs, context, index)?,
            &rule.op,
            unroll_expression(&rule.rhs, context, index)?,
            context,
            unrolled,
            rule.get_span(),
        )?;

        it_index.next();
    }

    Ok(())
}

fn set_flag_bool(v: &mut Flag, flag: &FlagStatement) -> Result<(), Error> {
    match &flag.value {
        super::parser::FlagValue::Set(_) => {
            return Err(Error::FlagBooleanExpected {
                error_span: flag.get_span(),
            })
        }
        super::parser::FlagValue::Ident(ident) => match &mut v.kind {
            FlagKind::Setting(s) => match s {
                FlagSetting::Default(_) | FlagSetting::Unset => {
                    *s = FlagSetting::Set(
                        FlagValue::Bool(match ident.ident.as_str() {
                            "enabled" | "on" | "true" => true,
                            "disabled" | "off" | "false" => false,
                            _ => {
                                return Err(Error::FlagBooleanExpected {
                                    error_span: flag.get_span(),
                                })
                            }
                        }),
                        flag.get_span(),
                    );
                }
                FlagSetting::Set(_, sp) => {
                    return Err(Error::RedefinedFlag {
                        error_span: flag.get_span(),
                        first_defined: *sp,
                        flag_name: flag.name.name.ident.clone(),
                    })
                }
            },
            FlagKind::Set(_) => unreachable!(),
        },
        super::parser::FlagValue::Number(num) => match &mut v.kind {
            FlagKind::Setting(s) => match s {
                FlagSetting::Default(_) | FlagSetting::Unset => {
                    if num.dot.is_none() {
                        *s = FlagSetting::Set(
                            FlagValue::Bool(match num.integral {
                                1 => true,
                                0 => false,
                                _ => {
                                    return Err(Error::FlagBooleanExpected {
                                        error_span: flag.get_span(),
                                    })
                                }
                            }),
                            flag.get_span(),
                        );
                    } else {
                        return Err(Error::FlagBooleanExpected {
                            error_span: flag.get_span(),
                        });
                    }
                }
                FlagSetting::Set(_, sp) => {
                    return Err(Error::RedefinedFlag {
                        error_span: flag.get_span(),
                        first_defined: *sp,
                        flag_name: flag.name.name.ident.clone(),
                    })
                }
            },
            FlagKind::Set(_) => unreachable!(),
        },
    }

    Ok(())
}

fn set_flag(set: &mut FlagSet, flag: &FlagStatement) -> Result<(), Error> {
    if let Some(v) = set.get_mut(&flag.name.name.ident) {
        match v.ty {
            FlagType::Set => match &flag.value {
                super::parser::FlagValue::Set(set) => match &mut v.kind {
                    FlagKind::Setting(_) => unreachable!(),
                    FlagKind::Set(s) => {
                        for stat in &set.flags {
                            set_flag(s, stat)?;
                        }
                    }
                },
                super::parser::FlagValue::Ident(_) | super::parser::FlagValue::Number(_) => {
                    return Err(Error::FlagSetExpected {
                        error_span: flag.get_span(),
                    })
                }
            },
            FlagType::Boolean => set_flag_bool(v, flag)?,
            FlagType::String => match &flag.value {
                super::parser::FlagValue::Number(_) | super::parser::FlagValue::Set(_) => {
                    return Err(Error::FlagStringExpected {
                        error_span: flag.get_span(),
                    })
                }
                super::parser::FlagValue::Ident(ident) => match &mut v.kind {
                    FlagKind::Setting(s) => match s {
                        FlagSetting::Default(_) | FlagSetting::Unset => {
                            *s = FlagSetting::Set(
                                FlagValue::String(ident.ident.clone()),
                                flag.get_span(),
                            );
                        }
                        FlagSetting::Set(_, sp) => {
                            return Err(Error::RedefinedFlag {
                                error_span: flag.get_span(),
                                first_defined: *sp,
                                flag_name: flag.name.name.ident.clone(),
                            })
                        }
                    },
                    FlagKind::Set(_) => unreachable!(),
                },
            },
        }

        Ok(())
    } else {
        let flag_name = flag.name.name.ident.clone();

        #[allow(clippy::cast_possible_truncation)]
        let suggested = set
            .iter()
            .max_by_key(|v| (strsim::jaro(v.0, &flag_name) * 1000.0).floor() as i64)
            .map(|x| x.0)
            .cloned();

        Err(Error::FlagDoesNotExist {
            flag_name,
            flag_span: flag.name.get_span(),
            error_span: flag.get_span(),
            suggested,
        })
    }
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
        flags: FlagSetConstructor::new()
            .add_set(
                &"optimizations",
                FlagSetConstructor::new().add_bool_def(&"identical_expressions", true),
            )
            .add_ident_def(&"distance_literals", &"none")
            .finish()
    };

    builtins::point::register_point_function(&mut context); // Point()
    builtins::dst::register_dst_function(&mut context); // dst()
    builtins::angle::register_angle_function(&mut context); // angle()
    builtins::degrees::register_degrees_function(&mut context); // degrees()
    builtins::radians::register_radians_function(&mut context); // radians()
    builtins::mid::register_mid_function(&mut context); // mid()
    builtins::perpendicular::register_perpendicular_function(&mut context); // perpendicular_through()
    builtins::parallel::register_parallel_function(&mut context); // parallel_through()
    builtins::intersection::register_intersection_function(&mut context); // intersection()
    builtins::bisector::register(&mut context); // bisector()

    let tokens = token::tokenize(input)?;
    let mut it = tokens.iter().peekable();

    let mut unrolled = Vec::new();

    let mut statements = Vec::new();

    while it.peek().is_some() {
        statements.push(Statement::parse(&mut it, &context)?);
    }

    for flag in statements.iter().filter_map(Statement::as_flag) {
        set_flag(&mut context.flags, flag)?;
    }

    for stat in statements {
        // Unroll the statement
        match stat {
            Statement::Noop(_) | Statement::Flag(_) => (),
            Statement::Let(stat) => unroll_let(&stat, &mut context, &mut unrolled)?,
            Statement::Rule(stat) => unroll_rulestat(&stat, &mut context, &mut unrolled)?,
        }
    }

    // for v in context.variables.values() {
    //     println!("let {} = {}", v.name, v.definition);
    // }

    // for x in &unrolled {
    //     println!("{x}");
    // }

    Ok((unrolled, context))
}
