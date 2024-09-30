use crate::{
    parser::Type,
    unroll::{AnyExpr, Overload},
};

/// The `mid` function.
use super::prelude::*;

struct MidScalar;

impl Overload for MidScalar {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        // This overload is only valid if all params are scalars of the same unit.
        // To check this, we convert the first param to a scalar of any unit,
        // get its type and check if every next param can also be converted.

        let mut unit = None;

        for param in params {
            if let Some(u) = param.can_convert_to_scalar(unit) {
                unit = u;
            } else {
                return None;
            }
        }

        Some(Type::Scalar(unit))
    }

    fn unroll(
        &self,
        params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let ty = self.get_returned_type(&params).unwrap();

        context
            .average_s_display(
                params
                    .into_iter()
                    .map(|x| x.convert_to(ty, context).to_scalar().unwrap())
                    .collect(),
                props,
            )
            .into()
    }
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
    library.add(
        Function::new("mid")
            .overload(|mut col: Pc<0>, context: &CompileContext, props| {
                context.average_p_display(
                    (0..col.0.data.length)
                        .map(|i| index!(node col, i))
                        .collect(),
                    props,
                )
            })
            .overload(MidScalar),
    );
}
