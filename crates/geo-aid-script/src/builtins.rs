//! `GeoScript`'s builtin functions and types

use std::ops::{Deref, DerefMut};

use num_rational::Ratio;

use crate::{
    parser::Type,
    unit,
    unroll::{AnyExpr, Expr, GeoType, PointCollection, Scalar},
    ComplexUnit,
};

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
    pub(crate) use crate::{
        builtins::{macros::*, Angle, Distance, Pc, Unitless},
        unit,
        unroll::{
            context::CompileContext,
            figure::{
                BuildAssociated, BundleNode, CollectionNode, HierarchyNode, LineNode, LineType,
                PointNode, ScalarNode,
            },
            Bundle, Circle, CloneWithNode, Expr, Function, GeoType, Library, Line, Point,
            Properties, Rule, ScalarData, UnrolledRule, UnrolledRuleKind,
        },
    };
    pub(crate) use geo_aid_figure::Style;
}

/// Point collection with a specific size
pub struct Pc<const N: usize>(pub Expr<PointCollection>);

impl<const N: usize> GeoType for Pc<N> {
    type Target = PointCollection;

    fn get_type() -> Type {
        Type::PointCollection(N)
    }
}

impl<const N: usize> From<Expr<PointCollection>> for Pc<N> {
    fn from(value: Expr<PointCollection>) -> Self {
        assert!(value.data.length == N || N == 0);
        Self(value)
    }
}

impl<const N: usize> Deref for Pc<N> {
    type Target = Expr<PointCollection>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const N: usize> DerefMut for Pc<N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Scalar with a specific unit.
pub struct ScalarUnit<
    const DST_NUM: i64,
    const DST_DENOM: i64,
    const ANG_NUM: i64,
    const ANG_DENOM: i64,
>(pub Expr<Scalar>);

impl<const DST_NUM: i64, const DST_DENOM: i64, const ANG_NUM: i64, const ANG_DENOM: i64>
    ScalarUnit<DST_NUM, DST_DENOM, ANG_NUM, ANG_DENOM>
{
    #[must_use]
    pub fn get_unit() -> ComplexUnit {
        unit::DISTANCE.pow(Ratio::new(DST_NUM, DST_DENOM))
            * &unit::ANGLE.pow(Ratio::new(ANG_NUM, ANG_DENOM))
    }
}

impl<const DST_NUM: i64, const DST_DENOM: i64, const ANG_NUM: i64, const ANG_DENOM: i64> GeoType
    for ScalarUnit<DST_NUM, DST_DENOM, ANG_NUM, ANG_DENOM>
{
    type Target = Scalar;

    fn get_type() -> Type {
        Type::Scalar(Some(Self::get_unit()))
    }
}

impl<const DST_NUM: i64, const DST_DENOM: i64, const ANG_NUM: i64, const ANG_DENOM: i64>
    From<Expr<Scalar>> for ScalarUnit<DST_NUM, DST_DENOM, ANG_NUM, ANG_DENOM>
{
    fn from(value: Expr<Scalar>) -> Self {
        assert_eq!(value.data.unit, Some(Self::get_unit()));
        Self(value)
    }
}

impl<const DST_NUM: i64, const DST_DENOM: i64, const ANG_NUM: i64, const ANG_DENOM: i64> Deref
    for ScalarUnit<DST_NUM, DST_DENOM, ANG_NUM, ANG_DENOM>
{
    type Target = Expr<Scalar>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const DST_NUM: i64, const DST_DENOM: i64, const ANG_NUM: i64, const ANG_DENOM: i64> DerefMut
    for ScalarUnit<DST_NUM, DST_DENOM, ANG_NUM, ANG_DENOM>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<const DST_NUM: i64, const DST_DENOM: i64, const ANG_NUM: i64, const ANG_DENOM: i64>
    From<ScalarUnit<DST_NUM, DST_DENOM, ANG_NUM, ANG_DENOM>> for AnyExpr
{
    fn from(value: ScalarUnit<DST_NUM, DST_DENOM, ANG_NUM, ANG_DENOM>) -> Self {
        value.0.into()
    }
}

pub type Distance = ScalarUnit<1, 1, 0, 1>;
pub type Angle = ScalarUnit<0, 1, 1, 1>;
pub type Unitless = ScalarUnit<0, 1, 0, 1>;

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
            $crate::unroll::Convert::convert::<$crate::unroll::Point>(
                ($bundle).index_with_node(stringify!($at)),
                $context,
            )
        };
        (no-node POINT $bundle:expr, $at:ident with $context:ident) => {
            $crate::unroll::Convert::convert::<$crate::unroll::Point>(
                ($bundle).index_without_node(stringify!($at)),
                $context,
            )
        };
    }

    /// Create a constant number expression
    macro_rules! number {
        ($v:expr) => {
            $crate::builtins::macros::number!(SCALAR $v)
        };
        (=$v:expr) => {
            $crate::unroll::Expr {
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::unroll::Scalar {
                    unit: Some($crate::unit::DISTANCE),
                    data: $crate::unroll::ScalarData::DstLiteral(
                        $v.clone()
                    )
                }),
                node: None
            }
        };
        ($t:ident $v:expr) => {
            $crate::unroll::Expr {
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::unroll::Scalar {
                    unit: Some($crate::unit::$t),
                    data: $crate::unroll::ScalarData::Number($v)
                }),
                node: None
            }
        };
    }

    /// Construct a bundle from a name and fields
    macro_rules! construct_bundle {
        ($t:ident { $($field:ident : $value:expr),* $(,)? }) => {{
            let mut fields = std::collections::HashMap::new();
            let mut node = $crate::unroll::figure::BundleNode::new();

            $(
                let mut v = $crate::unroll::CloneWithNode::clone_with_node(&mut $value);
                node.insert(stringify!($field).to_string(), $crate::unroll::CloneWithNode::clone_with_node(&mut v));
                fields.insert(stringify!($field).to_string(), $crate::unroll::AnyExpr::from(v));
            )*

            $t::from($crate::unroll::Expr {
                span: $crate::span!(0, 0, 0, 0),
                data: std::rc::Rc::new($crate::unroll::Bundle {
                    name: stringify!($t),
                    data: $crate::unroll::BundleData::ConstructBundle(fields.into())
                }),
                node: Some($crate::unroll::figure::HierarchyNode::new(node))
            })
        }};
    }

    /// Define a new bundle type
    macro_rules! define_bundle {
        ($t:ident {}) => {
            pub struct $t(Expr<Bundle>);

            impl GeoType for $t {
                type Target = Bundle;

                fn get_type() -> $crate::parser::Type {
                    $crate::parser::Type::Bundle(stringify!($t))
                }
            }

            impl From<Expr<Bundle>> for $t {
                fn from(value: Expr<Bundle>) -> Self {
                    assert_eq!(value.data.name, stringify!($t));
                    Self(value)
                }
            }

            impl std::ops::Deref for $t {
                type Target = Expr<Bundle>;

                fn deref(&self) -> &Self::Target {
                    &self.0
                }
            }

            impl std::ops::DerefMut for $t {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.0
                }
            }

            impl From<$t> for $crate::unroll::AnyExpr {
                fn from(value: $t) -> Self {
                    value.0.into()
                }
            }
        };
    }

    pub(crate) use {construct_bundle, define_bundle, field, index, number};
}
