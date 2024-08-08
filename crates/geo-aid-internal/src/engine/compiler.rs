use geo_aid_figure::Figure;
use geo_aid_math::{Context, Expr as CompiledExpr};
use crate::script::math::{EntityKind, Expr, Intermediate};

pub struct Compiled {
    pub figure_fn: Box<dyn Fn(&[f64]) -> Figure>,
    pub qualities: Vec<CompiledExpr>,
    pub context: Context
}

pub fn compile(intermediate: &Intermediate) -> Compiled {
    let inputs = intermediate.adjusted.entities
        .iter()
        .map(|ent| match ent {
            EntityKind::FreePoint => 2,
            EntityKind::PointOnLine { .. } => 1,
            EntityKind::PointOnCircle { .. } => 1,
            EntityKind::FreeReal => 1,
            EntityKind::DistanceUnit => 1,
            EntityKind::Bind(_) => unreachable!()
        })
        .sum();
    let mut context = Context::new(inputs);

    let exprs = intermediate.figure.variables
        .iter()
        .map(|expr| expr.compile(&mut context));

    let exprs = context.compute_array(exprs);

    Compiled {
        figure_fn: todo!(),
        qualities: vec![],
        context: (),
    }
}

trait Compile {
    fn compile(&self, context: &mut Context) -> CompiledExpr;
}

impl Compile for Expr<()> {
    fn compile(&self, context: &mut Context) -> CompiledExpr {
        todo!()
    }
}