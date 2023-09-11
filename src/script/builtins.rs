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

    lies_on::register(library); // lies_on
}

macro_rules! ty {
    ($name:ident) => {$crate::script::ty::$name};
    ($count:literal-P) => {
        $crate::script::ty::collection($count)
    }
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
                weight: 1.0,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::IndexCollection(
                    $col.clone(),
                    $at
                ))
            }
        }
    }

    macro_rules! bisector {
        ($a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
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
                weight: 1.0,
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
                weight: 1.0,
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
                weight: 1.0,
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
                weight: 1.0,
                ty: $crate::script::ty::ANGLE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::ThreePointAngle(
                    $a.clone(), $b.clone(), $c.clone()
                ))
            }
        };
        (dir $a:expr, $b:expr, $c:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
                ty: $crate::script::ty::ANGLE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::ThreePointAngleDir(
                    $a.clone(), $b.clone(), $c.clone()
                ))
            }
        };
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
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
                weight: 1.0,
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
                weight: 1.0,
                ty: $crate::script::ty::$unit,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::SetUnit(
                    $a.clone(), $crate::script::unit::$unit
                ))
            }
        };
        ($a:expr, $unit:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
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
                weight: 1.0,
                ty: $crate::script::Type::Scalar(Some($a.ty.as_scalar().unwrap().unwrap() * &$b.ty.as_scalar().unwrap().unwrap())),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Multiply(
                    $a.clone(), $b.clone()
                ))
            } 
        };
        (/, $a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
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
                weight: 1.0,
                ty: $crate::script::ty::SCALAR,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Number(
                    $v
                ))
            } 
        };
        ($t:ident $v:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
                ty: $crate::script::builtins::macros::ty!($t),
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Number(
                    $v
                ))
            } 
        };
    }

    macro_rules! entity {
        (POINT $index:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Entity($index))
            }
        };
        (SCALAR $index:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
                ty: $crate::script::ty::SCALAR,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::Entity($index))
            }
        };
    }

    macro_rules! circle_center {
        ($circle:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
                ty: $crate::script::ty::POINT,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::CircleCenter($circle.clone()))
            }
        };
    }

    macro_rules! circle_radius {
        ($circle:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
                ty: $crate::script::ty::DISTANCE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::CircleRadius($circle.clone()))
            }
        };
    }

    macro_rules! parallel_through {
        ($a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
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
                weight: 1.0,
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
                weight: 1.0,
                ty: $v.borrow().definition.ty,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::VariableAccess(
                    std::rc::Rc::clone(&$v.clone())
                ))
            }
        };
    }

    macro_rules! distance {
        (PP: $a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
                ty: $crate::script::ty::DISTANCE,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::UnrolledExpressionData::PointPointDistance(
                    $a.clone(), $b.clone()
                ))
            }
        };
        (PL: $a:expr, $b:expr) => {
            $crate::script::unroll::UnrolledExpression {
                weight: 1.0,
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
        ty, overload, params, call, index, bisector, line2,
        group, average, angle_expr, circle_expr, set_unit, math, number,
        intersection, entity, parallel_through, perpendicular_through,
        distance, variable, rule, circle_center, circle_radius
    };
}
