use super::unroll::CompileContext;

pub mod angle;
pub mod bisector;
pub mod degrees;
pub mod dst;
pub mod intersection;
pub mod mid;
pub mod parallel;
pub mod perpendicular;
pub mod point;
pub mod radians;
pub mod circle;

/// Registers all builtins
pub fn register(context: &mut CompileContext) {
    point::register(context); // Point()
    dst::register(context); // dst()
    angle::register(context); // angle()
    degrees::register(context); // degrees()
    radians::register(context); // radians()
    mid::register(context); // mid()
    perpendicular::register(context); // perpendicular_through()
    parallel::register(context); // parallel_through()
    intersection::register(context); // intersection()
    bisector::register(context); // bisector()
    circle::register(context); // Circle()
}
