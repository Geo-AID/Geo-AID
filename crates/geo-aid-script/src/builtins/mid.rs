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

// Registers the `mid` function.
//
// # Note:
//     Moved `...DISTANCE` rule to the end
//     to avoid ambiguity with two-point rule (2-P).
//     mid(AB) should be interpreted as mid(A, B), and
//     not as average of one number, the length of AB.
//     This is somewhat temporary, eventually we should
//     have a way of specifying that there should be
//     at least two `DISTANCE arguments in the last rule.
pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("mid"),
        Function {
            overloads: vec![
                overload!((...ANGLE) -> ANGLE : function_scalar),
                overload!((...POINT) -> POINT : function_point),
                overload!((...SCALAR) -> SCALAR : function_scalar),
                overload!((2-P) -> POINT : |mut col: Expr<PointCollection>, context, display| call!(context:function_point(
                        vec![index!(node col, 0), index!(node col, 1)]
                    )with display)),
                overload!((...DISTANCE) -> DISTANCE : function_scalar),
            ],
        },
    );
}
