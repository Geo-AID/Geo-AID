//! GeoScript's builtin functions and types

use super::unroll::Library;

pub mod angle;
pub mod bisector;
pub mod circle;
pub mod degrees;
pub mod dst;
pub mod intersection;
pub mod lies_on;
pub mod line;
pub mod mid;
pub mod parallel;
pub mod perpendicular;
pub mod point;
pub mod radians;
pub mod segment;

/// A prelude for builtin functions.
pub mod prelude {
    pub(crate) use crate::script::{
        builtins::macros::*,
        unit,
        unroll::{
            context::CompileContext,
            figure::{
                BuildAssociated, BundleNode, CollectionNode, HierarchyNode, LineNode, LineType,
                PointNode, ScalarNode,
            },
            Bundle, Circle, CloneWithNode, Expr, Function, Library, Line, Point, PointCollection,
            Properties, Rule, Scalar, ScalarData, UnrolledRule, UnrolledRuleKind,
        },
    };
    pub(crate) use geo_aid_figure::Style;
}

/// Returns what size of point collection can the given bundle type be cast onto.
/// 0 signifies that casting is not possible
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
    line::register(library); // Line()

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
    /// Call a function with given arguments, context, and possibly properties.
    macro_rules! call {
        ($fig:ident : $func:ident($($arg:expr),*)) => {
            $func($($arg),*, $fig, $crate::script::unroll::Properties::from(None))
        };
        ($fig:ident : $func:ident($($arg:expr),*) with $props:expr) => {
            $func($($arg),*, $fig, $props)
        };
    }

    /// Get the expression at given index in a point collection.
    macro_rules! index {
        (no-node $col:expr, $at:expr) => {
            ($col).index_without_node($at)
        };
        (node $col:expr, $at:expr) => {
            ($col).index_with_node($at)
        };
    }

    /// Get a specific field from a bundle.
    macro_rules! field {
        (node POINT $bundle:expr, $at:ident with $context:ident) => {
            $crate::script::unroll::Convert::convert::<$crate::script::unroll::Point>(
                ($bundle).index_with_node(stringify!($at)),
                $context,
            )
        };
        (no-node POINT $bundle:expr, $at:ident with $context:ident) => {
            $crate::script::unroll::Convert::convert::<$crate::script::unroll::Point>(
                ($bundle).index_without_node(stringify!($at)),
                $context,
            )
        };
    }

    /// Create a constant number expression
    macro_rules! number {
        ($v:expr) => {
            $crate::script::builtins::macros::number!(SCALAR $v)
        };
        (=$v:expr) => {
            $crate::script::unroll::Expr {
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
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Scalar {
                    unit: Some($crate::script::unit::$t),
                    data: $crate::script::unroll::ScalarData::Number($v)
                }),
                node: None
            }
        };
    }

    /// Construct a bundle from a name and fields
    macro_rules! construct_bundle {
        ($t:ident { $($field:ident : $value:expr),* $(,)? }) => {{
            let mut fields = std::collections::HashMap::new();
            let mut node = $crate::script::unroll::figure::BundleNode::new();

            $(
                let mut v = $crate::script::unroll::CloneWithNode::clone_with_node(&mut $value);
                node.insert(stringify!($field).to_string(), $crate::script::unroll::CloneWithNode::clone_with_node(&mut v));
                fields.insert(stringify!($field).to_string(), $crate::script::unroll::AnyExpr::from(v));
            )*

            $crate::script::unroll::Expr {
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::script::unroll::Bundle {
                    name: stringify!($t),
                    data: $crate::script::unroll::BundleData::ConstructBundle(fields.into())
                }),
                node: Some($crate::script::unroll::figure::HierarchyNode::new(node))
            }
        }};
    }

    pub(crate) use {call, construct_bundle, field, index, number};
}
