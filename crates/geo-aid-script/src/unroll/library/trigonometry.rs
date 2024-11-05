//! Trigonometry functions.

use num_rational::Ratio;
use num_traits::{FromPrimitive, One};

use crate::token::number::ProcNum;

use super::prelude::*;

fn asin(mut value: Unitless, context: &CompileContext, props: Properties) -> Angle {
    let one_minus_z_2_sqrt = context.pow(
        context.sub(
            number!(SCALAR ProcNum::one()),
            context.mult(value.0.clone_with_node(), value.0.clone_without_node()),
        ),
        Ratio::new(1, 2),
    );

    let to_log = context.sub(
        one_minus_z_2_sqrt,
        context.mult(value.0, number!(SCALAR ProcNum::i())),
    );
    let i_log = context.mult(context.log(to_log), number!(SCALAR ProcNum::i()));

    Angle::from(context.set_unit_display(i_log, unit::ANGLE, props))
}

fn acos(mut value: Unitless, context: &CompileContext, props: Properties) -> Angle {
    let one_minus_z_2_sqrt = context.pow(
        context.sub(
            number!(SCALAR ProcNum::one()),
            context.mult(value.0.clone_with_node(), value.0.clone_without_node()),
        ),
        Ratio::new(1, 2),
    );

    let to_log = context.add(
        context.mult(one_minus_z_2_sqrt, number!(SCALAR ProcNum::i())),
        value.0,
    );
    let i_log = context.mult(context.log(to_log), number!(-ProcNum::i()));

    Angle::from(context.set_unit_display(i_log, unit::ANGLE, props))
}

fn atan(mut value: Unitless, context: &CompileContext, props: Properties) -> Angle {
    let to_log = context.div(
        context.sub(number!(ProcNum::i()), value.0.clone_with_node()),
        context.add(number!(ProcNum::i()), value.0.clone_without_node()),
    );

    let mul_log = context.mult(
        number!(-ProcNum::i() / &ProcNum::from_u64(2).unwrap()),
        context.log(to_log),
    );

    Angle::from(context.set_unit_display(mul_log, unit::ANGLE, props))
}

/// Register all the trig functions.
pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("sin")
                .alias_method(ty::ANGLE, "sin")
                .overload(|angle: Angle, context: &CompileContext, props| {
                    Unitless::from(context.sin_display(angle.0, props))
                }),
        )
        .add(
            Function::new("csc")
                .alias_method(ty::ANGLE, "csc")
                .overload(|angle: Angle, context: &CompileContext, props| {
                    Unitless::from(context.div_display(
                        number!(SCALAR ProcNum::one()),
                        context.sin(angle.0),
                        props,
                    ))
                }),
        )
        .add(
            Function::new("cos")
                .alias_method(ty::ANGLE, "cos")
                .overload(|angle: Angle, context: &CompileContext, props| {
                    Unitless::from(context.cos_display(angle.0, props))
                }),
        )
        .add(
            Function::new("sec")
                .alias_method(ty::ANGLE, "sec")
                .overload(|angle: Angle, context: &CompileContext, props| {
                    Unitless::from(context.div_display(
                        number!(SCALAR ProcNum::one()),
                        context.cos(angle.0),
                        props,
                    ))
                }),
        )
        .add(
            Function::new("tan")
                .alias("tg")
                .alias_method(ty::ANGLE, "tan")
                .alias_method(ty::ANGLE, "tg")
                .overload(|mut angle: Angle, context: &CompileContext, props| {
                    Unitless::from(context.div_display(
                        context.sin(angle.0.clone_with_node()),
                        context.cos(angle.0.clone_without_node()),
                        props,
                    ))
                }),
        )
        .add(
            Function::new("cot")
                .alias("ctg")
                .alias_method(ty::ANGLE, "cot")
                .alias_method(ty::ANGLE, "ctg")
                .overload(|mut angle: Angle, context: &CompileContext, props| {
                    Unitless::from(context.div_display(
                        context.cos(angle.0.clone_without_node()),
                        context.sin(angle.0.clone_with_node()),
                        props,
                    ))
                }),
        )
        .add(
            Function::new("asin")
                .alias_method(ty::ANGLE, "asin")
                .overload(asin),
        )
        .add(
            Function::new("acsc")
                .alias_method(ty::ANGLE, "acsc")
                .overload(|value: Unitless, context: &CompileContext, props| {
                    asin(
                        Unitless::from(context.div(number!(SCALAR ProcNum::one()), value.0)),
                        context,
                        props,
                    )
                }),
        )
        .add(
            Function::new("acos")
                .alias_method(ty::ANGLE, "acos")
                .overload(acos),
        )
        .add(
            Function::new("asec")
                .alias_method(ty::ANGLE, "asec")
                .overload(|value: Unitless, context: &CompileContext, props| {
                    acos(
                        Unitless::from(context.div(number!(SCALAR ProcNum::one()), value.0)),
                        context,
                        props,
                    )
                }),
        )
        .add(
            Function::new("atan")
                .alias("atg")
                .alias_method(ty::ANGLE, "atan")
                .alias_method(ty::ANGLE, "atg")
                .overload(atan),
        )
        .add(
            Function::new("acot")
                .alias("actg")
                .alias_method(ty::ANGLE, "acot")
                .alias_method(ty::ANGLE, "actg")
                .overload(|value: Unitless, context: &CompileContext, props| {
                    atan(
                        Unitless::from(context.div(number!(SCALAR ProcNum::one()), value.0)),
                        context,
                        props,
                    )
                }),
        );
}
