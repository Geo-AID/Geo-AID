use std::mem;
use crate::script::builtins::macros::construct_bundle;

use crate::script::unroll::{UnrolledExpression, CompileContext, Properties, Library, Function};

use super::macros::{overload, call, index};

fn segment_function_point_point(args: &[UnrolledExpression], _context: &mut CompileContext, display: Option<Properties>) -> UnrolledExpression {
    mem::drop(display);

    construct_bundle!(
        Segment {
            A: args[0],
            B: args[1]
        }
    )
}

pub fn register(library: &mut Library) {
    library.functions.insert(
        String::from("Segment"),
        Function {
            name: String::from("Segment"),
            overloads: vec![
                overload!((2-P) -> Segment {
                    |args, context, _| call!(context:segment_function_point_point(
                        index!(args[0], 0),
                        index!(args[0], 1)
                    ))
                }),
                overload!((POINT, POINT) -> Segment : segment_function_point_point)
            ],
        },
    );
}