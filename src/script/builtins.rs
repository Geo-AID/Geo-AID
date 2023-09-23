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
pub mod mid;
pub mod parallel;
pub mod perpendicular;
pub mod point;
pub mod radians;
pub mod lies_on;
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

macro_rules! ty {
    (DISTANCE) => {$crate::script::ty::DISTANCE};
    (ANGLE) => {$crate::script::ty::ANGLE};
    (SCALAR) => {$crate::script::ty::SCALAR};
    (SCALAR_UNKNOWN) => {$crate::script::ty::SCALAR_UNKNOWN};
    (POINT) => {$crate::script::ty::POINT};
    (LINE) => {$crate::script::ty::LINE};
    (CIRCLE) => {$crate::script::ty::CIRCLE};
    ($count:literal-P) => {
        $crate::script::ty::collection($count)
    };
    ($t:ident) => {$crate::script::ty::bundle(stringify!($t))}
}

macro_rules! params {
    // ($($count:literal-)? $t:ident) => ($crate::script::builtins::ty!($($count-)? $t));
    ($($($count:literal-)? $t:ident),*) => {vec![$($crate::script::builtins::macros::ty!($($count-)? $t)),*]}
}

macro_rules! group {
    () => (None);
    (...$($count:literal-)? $t:ident) => (Some($crate::script::builtins::macros::ty!($($count-)? $t)))
}

macro_rules! overload {
    (($($($count:literal-)? $t:ident),* $(...$($gc:literal-)? $gt:ident)?) -> $($rcount:literal-)? $ret:ident {$content:expr}) => {
        $crate::script::unroll::FunctionOverload {
            returned_type: $crate::script::builtins::macros::ty!($($rcount-)? $ret),
            definition: $crate::script::unroll::FunctionDefinition(Box::new(
                $content
            )),
            params: $crate::script::builtins::macros::params!($($($count-)? $t),*),
            param_group: $crate::script::builtins::macros::group!()
        }
    };
    (($($($count:literal-)? $t:ident),* $(...$($gc:literal-)? $gt:ident)?) -> $($rcount:literal-)? $ret:ident : $func:ident) => {
        $crate::script::unroll::FunctionOverload {
            returned_type: $crate::script::builtins::macros::ty!($($rcount-)? $ret),
            definition: $crate::script::unroll::FunctionDefinition(Box::new($func)),
            params: $crate::script::builtins::macros::params!($($($count-)? $t),*),
            param_group: $crate::script::builtins::macros::group!($(...$($gc-)? $gt)?)
        }
    };
    ($($lcount:literal-)? $lt:ident $op:ident $($rcount:literal-)? $rt:ident : $func:ident) => {
        $crate::script::unroll::RuleOverload {
            definition: $crate::script::unroll::RuleDefinition(Box::new($func)),
            params: (
                $crate::script::builtins::macros::ty!($($lcount-)? $lt),
                $crate::script::builtins::macros::ty!($($rcount-)? $rt)
            )
        }
    };
}

/// Helper macros
pub mod macros {
    macro_rules! call {
        ($fig:ident : $func:ident($($arg:expr),*)) => {
            $func(&[$($arg.clone()),*], $fig, None)
        };
    }

    macro_rules! index {
        ($col:expr, $at:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::IndexCollection(
                    $col.clone(),
                    $at
                ))
            }
        }
    }

    macro_rules! field {
        ($bundle:expr, $at:ident) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new(
                    $crate::script::unroll::UnrolledExpressionData::IndexBundle(
                        $bundle.clone(),
                        stringify!($at).to_string(),
                    ),
                ),
            }
        };
    }

    macro_rules! bisector {
        ($a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::LINE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::AngleBisector(
                    $a.clone(), $b.clone(), $c.clone()
                ))
            }
        }
    }

    macro_rules! line2 {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::LINE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::LineFromPoints(
                    $a.clone(), $b.clone()
                ))
            }
        }
    }

    macro_rules! intersection {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::LineLineIntersection(
                    $a.clone(), $b.clone()
                ))
            }
        }
    }

    macro_rules! average {
        ($($count:literal-)? $t:ident : $x:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::builtins::macros::ty!($($count-)? $t),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Average(
                    $x.iter().cloned().collect()
                ))
            }
        };
    }

    macro_rules! angle_expr {
        ($a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::ANGLE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::ThreePointAngle(
                    $a.clone(), $b.clone(), $c.clone()
                ))
            }
        };
        (dir $a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::ANGLE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::ThreePointAngleDir(
                    $a.clone(), $b.clone(), $c.clone()
                ))
            }
        };
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::ANGLE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::TwoLineAngle(
                    $a.clone(), $b.clone()
                ))
            }
        }
    }

    macro_rules! circle_expr {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::CIRCLE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Circle(
                    $a.clone(), $b.clone()
                ))
            }
        }
    }

    macro_rules! set_unit {
        ($a:expr, %$unit:ident) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::$unit,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::SetUnit(
                    $a.clone(), $crate::script::unit::$unit
                ))
            }
        };
        ($a:expr, $unit:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::$unit,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::SetUnit(
                    $a.clone(), $unit
                ))
            }
        }
    }

    macro_rules! math {
        (*, $a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::Type::Scalar(Some($a.ty.as_scalar().unwrap().unwrap() * &$b.ty.as_scalar().unwrap().unwrap())),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Multiply(
                    $a.clone(), $b.clone()
                ))
            } 
        };
        (/, $a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::Type::Scalar(Some($a.ty.as_scalar().unwrap().unwrap() * &$b.ty.as_scalar().unwrap().unwrap())),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Divide(
                    $a.clone(), $b.clone()
                ))
            } 
        };
    }

    macro_rules! number {
        ($v:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::SCALAR,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Number(
                    $v
                ))
            } 
        };
        (=$v:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::builtins::macros::ty!(DISTANCE),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::DstLiteral(
                    $v,
                )),
            }
        };
        ($t:ident $v:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::builtins::macros::ty!($t),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Number($v)),
            }
        };
    }

    macro_rules! entity {
        (POINT $index:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Entity($index))
            }
        };
        (SCALAR $index:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::SCALAR,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Entity($index))
            }
        };
    }

    macro_rules! circle_center {
        ($circle:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::CircleCenter($circle.clone()))
            }
        };
    }

    macro_rules! circle_radius {
        ($circle:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::DISTANCE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::CircleRadius($circle.clone()))
            }
        };
    }

    macro_rules! parallel_through {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::LINE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::ParallelThrough(
                    $a.clone(), $b.clone()
                ))
            }
        }
    }

    macro_rules! perpendicular_through {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::LINE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::PerpendicularThrough(
                    $a.clone(), $b.clone()
                ))
            }
        }
    }

    macro_rules! variable {
        ($v:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $v.borrow().definition.ty,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::VariableAccess(
                    std::rc::Rc::clone(&$v.clone())
                ))
            }
        };
    }

    macro_rules! construct_bundle {
        ($t:ident { $($field:ident : $value:expr),* $(,)? }) => {{
            let mut fields = std::collections::HashMap::new();

            $(fields.insert(stringify!($field).to_string(), $value.clone());)*

            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::bundle(stringify!($t)),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::ConstructBundle(
                    fields
                ))
            }
        }};
    }

    macro_rules! distance {
        (PP: $a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::DISTANCE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::PointPointDistance(
                    $a.clone(), $b.clone()
                ))
            }
        };
        (PL: $a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: $crate::generator::fast_float::FastFloat::One,
                ty: $crate::script::ty::DISTANCE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::PointLineDistance(
                    $a.clone(), $b.clone()
                ))
            }
        };
    }

    macro_rules! rule {
        ($context:ident : $rule_op:ident($lhs:expr, $rhs:expr)) => {
            $rule_op(&$lhs, &$rhs, $context, None)
        };
        ($context:ident : $rule_op:ident($lhs:expr, $rhs:expr); $props:expr) => {
            $rule_op(&$lhs, &$rhs, $context, $props)
        };
        ($context:ident : > ($lhs:expr, $rhs:expr)) => {
            $context.rules.push($crate::script::unroll::UnrolledRule {
                kind: $crate::script::unroll::UnrolledRuleKind::Gt,
                lhs: $lhs.clone(),
                rhs: $rhs.clone(),
                inverted: false
            })
        };
        ($context:ident : = ($lhs:expr, $rhs:expr)) => {
            $context.rules.push($crate::script::unroll::UnrolledRule {
                kind: $crate::script::unroll::UnrolledRuleKind::Eq,
                lhs: $lhs.clone(),
                rhs: $rhs.clone(),
                inverted: false
            })
        };
    }

    pub(crate) use {
        angle_expr, average, bisector, call, circle_center, circle_expr, circle_radius,
        construct_bundle, distance, entity, field, group, index, intersection, line2, math, number,
        overload, parallel_through, params, perpendicular_through, rule, set_unit, ty, variable,
    };
}
