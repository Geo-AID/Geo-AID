/*
Copyright (c) 2023 Michał Wilczek, Michał Margos

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the “Software”), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

use super::unroll::Library;

pub mod angle;
pub mod bisector;
pub mod circle;
pub mod degrees;
pub mod dst;
pub mod intersection;
pub mod lies_on;
pub mod mid;
pub mod parallel;
pub mod perpendicular;
pub mod point;
pub mod radians;
pub mod segment;

/// Returns what size of point collection can the given bundle type be cast onto.
pub const fn get_bundle_pc(_name: &'static str) -> usize {
    0
}

/// Registers all builtins
pub fn register(library: &mut Library) {
    point::register(library); // Point()
    dst::register(library); // dst()
    angle::register(library); // angle()
    degrees::register(library); // degrees()
    radians::register(library); // radians()
    mid::register(library); // mid()
    perpendicular::register(library); // perpendicular_through()
    parallel::register(library); // parallel_through()
    intersection::register(library); // intersection()
    bisector::register(library); // bisector()
    circle::register(library); // Circle()
    segment::register(library); // Segment()

    lies_on::register(library); // lies_on
}

// macro_rules! ty {
//     (DISTANCE) => {$crate::script::ty::DISTANCE};
//     (ANGLE) => {$crate::script::ty::ANGLE};
//     (SCALAR) => {$crate::script::ty::SCALAR};
//     (SCALAR_UNKNOWN) => {$crate::script::ty::SCALAR_UNKNOWN};
//     (POINT) => {$crate::script::ty::POINT};
//     (LINE) => {$crate::script::ty::LINE};
//     (CIRCLE) => {$crate::script::ty::CIRCLE};
//     ($count:literal-P) => {
//         $crate::script::ty::collection($count)
//     };
//     ($t:ident) => {$crate::script::ty::bundle(stringify!($t))}
// }

/// Helper macros
pub mod macros {
    macro_rules! expr_with {
        () => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Line::LineFromPoints(
                    $a.clone(),
                    $b.clone(),
                )),
            }
        }
    }

    macro_rules! call {
        ($fig:ident : $func:ident($($arg:expr),*)) => {
            $func($(&$arg.clone()),*, $fig, $crate::script::unroll::Properties::from(None))
        };
    }

    macro_rules! index {
        ($col:expr, $at:expr) => {
            ($col).index($at)
        };
    }

    macro_rules! field {
        (POINT $bundle:expr, $at:ident) => {
            $crate::script::unroll::Convert::convert(($bundle).index(stringify!($at))).unwrap()
        };
    }

    macro_rules! bisector {
        ($a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Line::AngleBisector(
                    $a.clone(),
                    $b.clone(),
                    $c.clone(),
                )),
            }
        };
    }

    macro_rules! line2 {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Line::LineFromPoints(
                    $a.clone(),
                    $b.clone(),
                )),
            }
        };
    }

    macro_rules! intersection {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Point::LineLineIntersection(
                    $a.clone(),
                    $b.clone(),
                )),
            }
        };
    }

    macro_rules! average {
        (POINT : $x:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Point::Average(
                    $x.iter().cloned().collect(),
                )),
            }
        };
        (SCALAR : $x:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: $x[0].data.unit,
                    data: $crate::script::unroll::ScalarData::Average($x.iter().cloned().collect()),
                }),
            }
        };
    }

    macro_rules! angle_expr {
        ($a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::ANGLE),
                    data: $crate::script::unroll::ScalarData::ThreePointAngle(
                        $a.clone(),
                        $b.clone(),
                        $c.clone(),
                    ),
                }),
            }
        };
        (dir $a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::ANGLE),
                    data: $crate::script::unroll::ScalarData::ThreePointAngleDir(
                        $a.clone(),
                        $b.clone(),
                        $c.clone(),
                    ),
                }),
            }
        };
        ($a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::ANGLE),
                    data: $crate::script::unroll::ScalarData::TwoLineAngle($a.clone(), $b.clone()),
                }),
            }
        };
    }

    macro_rules! circle_expr {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Circle::Circle(
                    $a.clone(),
                    $b.clone(),
                )),
            }
        };
    }

    macro_rules! set_unit {
        ($a:expr, %$unit:ident) => {
            $crate::script::builtins::macros::set_unit!($a, $crate::script::unit::$unit)
        };
        ($a:expr, $unit:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($unit),
                    data: $crate::script::unroll::ScalarData::SetUnit($a.clone(), $unit),
                }),
            }
        };
    }

    macro_rules! math {
        (*, $a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($a.data.unit.unwrap() * &$b.data.unit.unwrap()),
                    data: $crate::script::unroll::ScalarData::Multiply($a.clone(), $b.clone()),
                }),
            }
        };
        (/, $a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some(
                        $a.ty.as_scalar().unwrap().unwrap() / &$b.ty.as_scalar().unwrap().unwrap(),
                    ),
                    data: $crate::script::unroll::ScalarData::Divide($a.clone(), $b.clone()),
                }),
            }
        };
        (+, $a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: $a.data.unit,
                    data: $crate::script::unroll::ScalarData::Add($a.clone(), $b.clone()),
                }),
            }
        };
    }

    macro_rules! number {
        ($v:expr) => {
            $crate::script::builtins::macros::number!(SCALAR $v)
        };
        (=$v:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::DISTANCE),
                    data: $crate::script::unroll::ScalarData::DstLiteral(
                        $v.clone()
                    )
                })
            }
        };
        ($t:ident $v:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::$t),
                    data: $crate::script::unroll::ScalarData::Number($v)
                }),
            }
        };
    }

    macro_rules! entity {
        (POINT $index:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Point::Entity($index)),
            }
        };
        ($unit:ident $index:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::$unit),
                    data: $crate::script::unroll::ScalarData::Entity($index),
                }),
            }
        };
    }

    macro_rules! circle_center {
        ($circle:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Point::CircleCenter(
                    $circle.clone(),
                )),
            }
        };
    }

    macro_rules! circle_radius {
        ($circle:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::DISTANCE),
                    data: $crate::script::unroll::ScalarData::CircleRadius($circle.clone()),
                }),
            }
        };
    }

    macro_rules! parallel_through {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Line::ParallelThrough(
                    $a.clone(),
                    $b.clone(),
                )),
            }
        };
    }

    macro_rules! perpendicular_through {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Line::PerpendicularThrough(
                    $a.clone(),
                    $b.clone(),
                )),
            }
        };
    }

    macro_rules! construct_bundle {
        ($t:ident { $($field:ident : $value:expr),* $(,)? }) => {{
            let mut fields = std::collections::HashMap::new();

            $(fields.insert(stringify!($field).to_string(), $crate::script::unroll::AnyExpr::from($value.clone()));)*

            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Bundle {
                    name: stringify!($t),
                    data: $crate::script::unroll::BundleData::ConstructBundle(fields)
                })
            }
        }};
    }

    macro_rules! distance {
        (PP: $a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::DISTANCE),
                    data: $crate::script::unroll::ScalarData::PointPointDistance(
                        $a.clone(),
                        $b.clone(),
                    ),
                }),
            }
        };
        (PL: $a:expr, $b:expr) => {
            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::DISTANCE),
                    data: $crate::script::unroll::ScalarData::PointLineDistance(
                        $a.clone(),
                        $b.clone(),
                    ),
                }),
            }
        };
    }

    macro_rules! mneg {
        (neg = $neg:expr) => {
            $neg
        };
        () => {
            false
        };
    }

    macro_rules! rule {
        ($context:ident : $rule_op:ident($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $rule_op(&$lhs, &$rhs, $context, $crate::script::unroll::Properties::from(None), $crate::script::builtins::macros::mneg!($(neg = $neg)?))
        };
        ($context:ident : $rule_op:ident($lhs:expr, $rhs:expr); $props:expr; $(neg = $neg:expr)?) => {
            $rule_op(&$lhs, &$rhs, $context, $props, $crate::script::builtins::macros::mneg!($(neg = $neg)?))
        };
        ($context:ident : > ($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $context.rules.push($crate::script::unroll::UnrolledRule {
                kind: $crate::script::unroll::UnrolledRuleKind::Gt($lhs.clone(), $rhs.clone()),
                inverted: $crate::script::builtins::macros::mneg!($(neg = $neg)?),
            })
        };
        ($context:ident : S = ($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $context.rules.push($crate::script::unroll::UnrolledRule {
                kind: $crate::script::unroll::UnrolledRuleKind::ScalarEq(
                    $lhs.clone(),
                    $rhs.clone(),
                ),
                inverted: $crate::script::builtins::macros::mneg!($(neg = $neg)?),
            })
        };
        ($context:ident : P  = ($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $context.rules.push($crate::script::unroll::UnrolledRule {
                kind: $crate::script::unroll::UnrolledRuleKind::PointEq($lhs.clone(), $rhs.clone()),
                inverted: $crate::script::builtins::macros::mneg!($(neg = $neg)?),
            })
        };
    }

    pub(crate) use {
        angle_expr, average, bisector, call, circle_center, circle_expr, circle_radius,
        construct_bundle, distance, entity, field, index, intersection, line2, math, mneg, number,
        parallel_through, perpendicular_through, rule, set_unit,
    };
}
