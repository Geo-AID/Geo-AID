/// The `mid` function.
use super::prelude::*;
use geo_aid_derive::overload;

/// `mid(point...)` - the arithmetic mean of points as complexes.
pub fn function_point(
    args: Vec<Expr<Point>>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Point> {
    context.average_p_display(args, display)
}

/// `mid(scalar...)` - the arithmetic mean of reals
fn function_scalar(
    args: Vec<Expr<Scalar>>,
    context: &CompileContext,
    display: Properties,
) -> Expr<Scalar> {
    context.average_s_display(args, display)
}

/// Register the function
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("mid"),
        Function {
            overloads: vec![
                overload!((...ANGLE) -> ANGLE : function_scalar),
                overload!((...DISTANCE) -> DISTANCE : function_scalar),
                overload!((...POINT) -> POINT : function_point),
                overload!((...SCALAR) -> SCALAR : function_scalar),
            ],
        },
    );
}
