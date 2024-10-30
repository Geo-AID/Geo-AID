use std::fmt::{Display, Debug};

use crate::unroll::AnyExpr;

use super::prelude::*;

trait Transform<T: GeoType> {
    /// Result of the transform.
    type Output: GeoType + Into<AnyExpr>;

    /// Perform the transformation.
    #[must_use]
    fn transform(&self, object: T, context: &mut CompileContext, props: Properties) -> Self::Output;
}

trait TransformErased<T: GeoType> {
    /// Perform the transformation.
    #[must_use]
    fn transform(&self, object: T, context: &mut CompileContext, props: Properties) -> AnyExpr;
}

impl<T: Transform<U>, U: GeoType> TransformErased<U> for T {
    fn transform(&self, object: U, context: &mut CompileContext, props: Properties) -> AnyExpr {
        Transform::transform(self, object, context, props).into()
    }
}

trait DynTransform: Debug + Display + TransformErased<Expr<Point>> + TransformErased<Expr<Line>> + TransformErased<Expr<Circle>> {}

impl<T: Debug + Display + TransformErased<Expr<Point>> + TransformErased<Expr<Line>> + TransformErased<Expr<Circle>>> DynTransform for T {}

/// A transformation of space.
#[derive(Debug)]
pub struct TransformType(Box<dyn DynTransform>);

impl DerivedType for TransformType {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Display for TransformType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.0.as_ref(), f)
    }
}

impl_derived! {TransformType}

fn homothety(origin: Expr<Point>, scale: Unitless, context: &CompileContext, props: Properties) -> TransformTypeExpr {
    
}

pub fn register(library: &mut Library) {
    library.add(Function::new("homothety").overload(homothety));
}