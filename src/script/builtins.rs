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
    macro_rules! root_node {
        (Point) => {
            $crate::script::unroll::PointNode::new(None)
        }
    }

    macro_rules! expr_with {
        ($ty:ident :: $kind:ident ($($arg1:expr),*) with ($($arg2:expr),*)) => {
            {
                let mut node = $crate::script::unroll::HierarchyNode::new($crate::script::builtins::macros::root_node!($ty));

                $(
                    let mut v = $crate::script::unroll::CloneWithNode::clone_without_node(&mut $arg2);
                    node.extend_children(v.node.take());
                )*

                $crate::script::unroll::Expr {
                    weight: $crate::generator::fast_float::FastFloat::One,
                    span: $crate::span!(0, 0, 0, 0),
                    data: std::rc::Rc::new($crate::script::unroll::$ty::$kind(
                        $($crate::script::unroll::CloneWithNode::clone_without_node(&($arg1))),*
                    )),
                    node: Some(node)
                }
            }
        };
        ($ty:ident :: $kind:ident ($($arg1:expr),*)) => {
            {
                $crate::script::unroll::Expr {
                    weight: $crate::generator::fast_float::FastFloat::One,
                    span: $crate::span!(0, 0, 0, 0),
                    data: std::rc::Rc::new($crate::script::unroll::$ty::$kind(
                        $($crate::script::unroll::CloneWithNode::clone_without_node(&$arg1)),*
                    )),
                    node: None
                }
            }
        };
        ([$unit:expr] :: $kind:ident ($($arg1:expr),*) with ($($arg2:expr),*)) => {
            {
                let mut node = $crate::script::unroll::CollectionNode::new();

                $(node.children.extend($crate::script::unroll::CloneWithNode::clone_with_node(&mut $arg2)
                    .node.take()
                    .map(|x| Box::new(x) as Box<dyn $crate::script::unroll::Node>)
                );)*

                $crate::script::unroll::Expr {
                    weight: $crate::generator::fast_float::FastFloat::One,
                    span: $crate::span!(0, 0, 0, 0),
                    data: std::rc::Rc::new($crate::script::unroll::Scalar {
                        unit: $unit,
                        data: $crate::script::unroll::ScalarData::$kind(
                            $($crate::script::unroll::CloneWithNode::clone_without_node(&$arg1)),*
                        ),
                    }),
                    node: Some(node)
                }
            }
        };
        ([$unit:expr] :: $kind:ident ($($arg1:expr),*)) => {
            {
                $crate::script::unroll::Expr {
                    weight: $crate::generator::fast_float::FastFloat::One,
                    span: $crate::span!(0, 0, 0, 0),
                    data: std::rc::Rc::new($crate::script::unroll::Scalar {
                        unit: $unit,
                        data: $crate::script::unroll::ScalarData::$kind(
                            $($crate::script::unroll::CloneWithNode::clone_without_node(&$arg1)),*
                        ),
                    }),
                    node: None
                }
            }
        };
    }

    macro_rules! call {
        ($fig:ident : $func:ident($($arg:expr),*)) => {
            $func($($arg),*, $fig, $crate::script::unroll::Properties::from(None))
        };
    }

    macro_rules! index {
        (no-node $col:expr, $at:expr) => {
            ($col).index_without_node($at)
        };
        (node $col:expr, $at:expr) => {
            ($col).index_with_node($at)
        };
    }

    macro_rules! field {
        (node POINT $bundle:expr, $at:ident) => {
            $crate::script::unroll::Convert::convert::<$crate::script::unroll::Point>(($bundle).index_with_node(stringify!($at))).unwrap()
        };
        (no-node POINT $bundle:expr, $at:ident) => {
            $crate::script::unroll::Convert::convert::<$crate::script::unroll::Point>(($bundle).index_without_node(stringify!($at))).unwrap()
        };
    }

    macro_rules! bisector {
        ($a:expr, $b:expr, $c:expr) => {
            $crate::script::builtins::macros::expr_with!(Line::AngleBisector(
                $a, $b, $c
            ) with (
                $a, $b, $c
            ))
        };
    }

    macro_rules! line2 {
        ($a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!(Line::LineFromPoints(
                $a, $b
            ) with (
                $a, $b
            ))
        };
    }

    macro_rules! average {
        (POINT : $x:expr) => {
            {
                let mut v = $x;
                let mut node = $crate::script::unroll::CollectionNode::new();
                node.children.extend(
                    v.iter_mut()
                        .flat_map(|e| e.node.take().map(|x| Box::new(x) as Box<dyn $crate::script::unroll::Node>))
                );

                $crate::script::unroll::Expr {
                    weight: $crate::generator::fast_float::FastFloat::One,
                    span: $crate::span!(0, 0, 0, 0),
                    data: std::rc::Rc::new($crate::script::unroll::Point::Average(v.into())),
                    node: Some(node)
                }
            }
        };
        (SCALAR : $x:expr) => {
            {
                let mut v = $x;
                let mut node = $crate::script::unroll::CollectionNode::new();
                node.children.extend(
                    v.iter_mut()
                        .flat_map(|e| e.node.take().map(|x| Box::new(x) as Box<dyn $crate::script::unroll::Node>))
                );

                $crate::script::unroll::Expr {
                    weight: $crate::generator::fast_float::FastFloat::One,
                    span: $crate::span!(0, 0, 0, 0),
                    data: std::rc::Rc::new($crate::script::unroll::Scalar {
                        unit: v[0].data.unit,
                    data: $crate::script::unroll::ScalarData::Average(v.into()),
                    }),
                    node: Some(node)
                }
            }
        };
    }

    macro_rules! angle_expr {
        ($a:expr, $b:expr, $c:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($crate::script::unit::ANGLE)]::ThreePointAngle(
                $a, $b, $c
            ) with (
                $a, $b, $c
            ))
        };
        (dir $a:expr, $b:expr, $c:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($crate::script::unit::ANGLE)]::ThreePointAngleDir(
                $a, $b, $c
            ) with (
                $a, $b, $c
            ))
        };
        ($a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($crate::script::unit::ANGLE)]::TwoLineAngle(
                $a, $b
            ) with (
                $a, $b
            ))
        };
    }

    macro_rules! circle_expr {
        ($a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!(Circle::Circle(
                $a, $b
            ) with (
                $a, $b
            ))
        };
    }

    macro_rules! set_unit {
        ($a:expr, %$unit:ident) => {
            $crate::script::builtins::macros::set_unit!($a, $crate::script::unit::$unit)
        };
        ($a:expr, $unit:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($unit)]::SetUnit(
                $a, $unit
            ) with (
                $a
            ))
        };
    }

    macro_rules! math {
        (*, $a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($a.data.unit.unwrap() * &$b.data.unit.unwrap())]::Multiply(
                $a, $b
            ) with (
                $a, $b
            ))
        };
        (/, $a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($a.data.unit.unwrap() / &$b.data.unit.unwrap())]::Divide(
                $a, $b
            ) with (
                $a, $b
            ))
        };
        (+, $a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($a.data.unit.unwrap())]::Add(
                $a, $b
            ) with (
                $a, $b
            ))
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
                }),
                node: None
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
                node: None
            }
        };
    }

    macro_rules! entity {
        (POINT $index:expr) => {
            $crate::script::builtins::macros::expr_with!(Point::Entity($index))
        };
        ($unit:ident $index:expr) => {
            $crate::script::builtins::macros::expr_with!([Some($crate::script::unit::$unit)]::Entity($index))
        };
    }

    macro_rules! parallel_through {
        ($a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!(Line::ParallelThrough(
                $a, $b
            ) with (
                $a, $b
            ))
        };
    }

    macro_rules! perpendicular_through {
        ($a:expr, $b:expr) => {
            $crate::script::builtins::macros::expr_with!(Line::PerpendicularThrough(
                $a, $b
            ) with (
                $a, $b
            ))
        };
    }

    macro_rules! construct_bundle {
        ($t:ident { $($field:ident : $value:expr),* $(,)? }) => {{
            let mut fields = std::collections::HashMap::new();
            let mut node = $crate::script::unroll::BundleNode::new();

            $(
                let mut v = $crate::script::unroll::CloneWithNode::clone_with_node(&mut $value);
                node.insert(stringify!($field).to_string(), v.node.take());
                fields.insert(stringify!($field).to_string(), $crate::script::unroll::AnyExpr::from(v));
            )*

            $crate::script::unroll::Expr {
                weight: $crate::generator::fast_float::FastFloat::One,
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Bundle {
                    name: stringify!($t),
                    data: $crate::script::unroll::BundleData::ConstructBundle(fields.into())
                }),
                node: Some(node)
            }
        }};
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
        ($context:ident, $f:ident, $lhs:expr, $rhs:expr, $($neg:expr)?) => {
            {
                let (mut lhs, mut rhs) = ($lhs, $rhs);
                let mut node = $crate::script::unroll::CollectionNode::new();

                node.children.extend($crate::script::unroll::CloneWithNode::clone_with_node(&mut lhs)
                    .node.take()
                    .map(|x| Box::new(x) as Box<dyn $crate::script::unroll::Node>)
                );
                node.children.extend($crate::script::unroll::CloneWithNode::clone_with_node(&mut rhs)
                    .node.take()
                    .map(|x| Box::new(x) as Box<dyn $crate::script::unroll::Node>)
                );

                $context.rules.push($crate::script::unroll::UnrolledRule {
                    kind: $crate::script::unroll::UnrolledRuleKind::$f(
                        $crate::script::unroll::CloneWithNode::clone_without_node(&lhs),
                        $crate::script::unroll::CloneWithNode::clone_without_node(&rhs)
                    ),
                    inverted: $crate::script::builtins::macros::mneg!($(neg = $neg)?),
                    node: Some(Box::new(node))
                })
            }
        };
        ($context:ident : display $node:expr) => {
            $context.rules.push($crate::script::unroll::UnrolledRule {
                kind: $crate::script::unroll::UnrolledRuleKind::Display,
                inverted: false,
                node: Some(Box::new($node))
            })
        };
        ($context:ident : $rule_op:ident($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $rule_op($lhs, $rhs, $context, $crate::script::unroll::Properties::from(None), $crate::script::builtins::macros::mneg!($(neg = $neg)?))
        };
        ($context:ident : $rule_op:ident($lhs:expr, $rhs:expr); $props:expr; $(neg = $neg:expr)?) => {
            $rule_op($lhs, $rhs, $context, $props, $crate::script::builtins::macros::mneg!($(neg = $neg)?))
        };
        ($context:ident : > ($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $crate::script::builtins::macros::rule!($context, Gt, $lhs, $rhs, $($neg)?)
        };
        ($context:ident : < ($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $crate::script::builtins::macros::rule!($context, Lt, $lhs, $rhs, $($neg)?)
        };
        ($context:ident : P  = ($lhs:expr, $rhs:expr) $(neg = $neg:expr)?) => {
            $crate::script::builtins::macros::rule!($context, PointEq, $lhs, $rhs, $($neg)?)
        };
    }

    pub(crate) use {
        angle_expr, average, bisector, call, circle_expr,
        construct_bundle, entity, field, index, line2, math, mneg, number,
        parallel_through, perpendicular_through, rule, set_unit, expr_with, root_node
    };
}
