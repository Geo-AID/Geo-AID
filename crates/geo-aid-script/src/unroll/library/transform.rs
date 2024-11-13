use std::fmt::{Debug, Display};

use num_traits::{One, Zero};

use crate::{
    parser::{PropertyValue, Type},
    token::{number::ProcNum, Span, StrLit},
    unroll::{figure::NoContentNode, AnyExpr},
};

use super::{prelude::*, Overload};

trait Transform<T: GeoType> {
    /// Result of the transform.
    type Output: GeoType + Into<AnyExpr>;

    /// Perform the transformation.
    #[must_use]
    fn transform(&self, object: T, context: &mut CompileContext, props: Properties)
        -> Self::Output;
}

trait TransformErased<T: GeoType> {
    /// Perform the transformation.
    #[must_use]
    fn transform_erased(
        &self,
        object: T,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr;

    /// What type is the output
    #[must_use]
    fn get_transformed_type(&self) -> Type;
}

impl<T: Transform<U>, U: GeoType> TransformErased<U> for T {
    fn transform_erased(
        &self,
        object: U,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        Transform::transform(self, object, context, props).into()
    }

    fn get_transformed_type(&self) -> Type {
        T::Output::get_type()
    }
}

trait DynTransform: Debug + Display {
    fn transform_any(
        &self,
        expr: AnyExpr,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr;

    fn transform_type(&self, ty: Type) -> Option<Type>;
}

macro_rules! impl_dyn {
    ($ty:ty) => {
        impl DynTransform for $ty {
            fn transform_any(
                &self,
                expr: AnyExpr,
                context: &mut CompileContext,
                props: Properties,
            ) -> AnyExpr {
                match expr {
                    AnyExpr::Point(v) => self.transform_erased(v, context, props),
                    AnyExpr::Line(v) => self.transform_erased(v, context, props),
                    AnyExpr::Circle(v) => self.transform_erased(v, context, props),
                    _ => panic!(),
                }
            }

            fn transform_type(&self, ty: Type) -> Option<Type> {
                match ty {
                    Type::Point | Type::PointCollection(1) => {
                        Some(TransformErased::<Expr<Point>>::get_transformed_type(self))
                    }
                    Type::Line | Type::PointCollection(2) => {
                        Some(TransformErased::<Expr<Line>>::get_transformed_type(self))
                    }
                    Type::Circle => {
                        Some(TransformErased::<Expr<Circle>>::get_transformed_type(self))
                    }
                    _ => None,
                }
            }
        }
    };
}

/// A transformation of space.
#[derive(Debug)]
pub struct TransformType(Box<dyn DynTransform>);

impl DerivedType for TransformType {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Display for TransformType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.0.as_ref(), f)
    }
}

impl_derived! {TransformType}

fn projection(point: &Distance, line: &Expr<Line>, context: &CompileContext) -> Expr<Point> {
    context.intersection(
        context.perpendicular_through(
            line.clone_without_node(),
            context.to_point(point.0.clone_without_node()),
        ),
        line.clone_without_node(),
    )
}

#[derive(Debug)]
struct Translation {
    vector: Distance,
}

impl Display for Translation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Translation({})", self.vector)
    }
}

impl Transform<Expr<Point>> for Translation {
    type Output = Expr<Point>;

    fn transform(
        &self,
        object: Expr<Point>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let p = context.to_complex(object);
        let q = context.add(p, self.vector.0.clone_without_node());
        context.to_point_display(q, props)
    }
}

impl Transform<Expr<Line>> for Translation {
    type Output = Expr<Line>;

    fn transform(
        &self,
        object: Expr<Line>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let proj = projection(
            &Distance::from(number!(DISTANCE ProcNum::zero())),
            &object,
            context,
        );
        let transformed_proj = self.transform(proj, context, Properties::default());
        context.parallel_through_display(object, transformed_proj, props)
    }
}

impl Transform<Expr<Circle>> for Translation {
    type Output = Expr<Circle>;

    fn transform(
        &self,
        object: Expr<Circle>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let old_radius = context.circle_radius(object.clone_without_node());
        let old_center = context.circle_center(object);
        let new_center = self.transform(old_center, context, Properties::default());
        context.circle_display(new_center, old_radius, props)
    }
}

impl_dyn! {Translation}

fn translation(
    mut vector: Distance,
    context: &CompileContext,
    mut props: Properties,
) -> TransformTypeExpr {
    let node = NoContentNode {
        display: props.get("display").maybe_unset(true),
    };
    props.ignore("default-label");
    props.finish(context);

    let mut node = HierarchyNode::new_dyn(node);
    node.extend_children(vector.take_node());

    TransformTypeExpr::new(TransformType(Box::new(Translation { vector })), node)
}

#[derive(Debug)]
pub struct Spiral {
    origin: Distance,
    scale: Unitless,
    vector: Unitless,
}

impl Display for Spiral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Spiral similarity with origin {}, scale {}, and transform vector {}",
            self.origin, self.scale, self.vector
        )
    }
}

impl Transform<Expr<Point>> for Spiral {
    type Output = Expr<Point>;

    fn transform(
        &self,
        object: Expr<Point>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let point = context.to_complex(object);
        let diff = context.sub(point, self.origin.clone_without_node());
        let new_diff = context.mult(diff, self.vector.clone_without_node());
        let new_point = context.add(self.origin.clone_without_node(), new_diff);
        context.to_point_display(new_point, props)
    }
}

impl Transform<Expr<Line>> for Spiral {
    type Output = Expr<Line>;

    fn transform(
        &self,
        object: Expr<Line>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let proj = projection(&self.origin, &object, context);
        let transformed_proj = self.transform(proj, context, Properties::default());
        let ray = context.line(
            context.to_point(self.origin.0.clone_without_node()),
            transformed_proj.clone_without_node(),
        );

        context.perpendicular_through_display(ray, transformed_proj, props)
    }
}

impl Transform<Expr<Circle>> for Spiral {
    type Output = Expr<Circle>;

    fn transform(
        &self,
        object: Expr<Circle>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let center = context.circle_center(object.clone_without_node());
        let radius = context.circle_radius(object);

        let new_center = self.transform(center, context, Properties::default());
        let new_radius = context.mult(radius, self.scale.0.clone_without_node());
        context.circle_display(new_center, new_radius, props)
    }
}

impl_dyn! {Spiral}

fn spiral(
    mut origin: Expr<Point>,
    mut angle: Angle,
    mut scale: Unitless,
    context: &CompileContext,
    mut props: Properties,
) -> TransformTypeExpr {
    let node = NoContentNode {
        display: props.get("display").maybe_unset(true),
    };
    props.ignore("default-label");
    props.finish(context);

    let mut node = HierarchyNode::new_dyn(node);
    node.extend_children(origin.take_node());
    node.extend_children(scale.0.take_node());
    node.extend_children(angle.0.take_node());

    let vector = context.add(
        context.cos(angle.0.clone_without_node()),
        context.mult(context.sin(angle.0), number!(SCALAR ProcNum::i())),
    );
    let vector = context.mult(vector, scale.0.clone_without_node());

    TransformTypeExpr::new(
        TransformType(Box::new(Spiral {
            origin: Distance::from(context.to_complex(origin)),
            scale,
            vector: Unitless::from(vector),
        })),
        node,
    )
}

#[derive(Debug)]
pub struct Reflect {
    line: Expr<Line>,
}

impl Display for Reflect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Reflection by line {}", self.line)
    }
}

impl Transform<Expr<Point>> for Reflect {
    type Output = Expr<Point>;

    fn transform(
        &self,
        object: Expr<Point>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let mut no_display = Properties::default();
        no_display.add_if_not_present(
            "display",
            (
                Span::empty(),
                PropertyValue::String(StrLit {
                    span: Span::empty(),
                    content: String::from("false"),
                }),
            ),
        );
        let proj = context.intersection_display(
            context
                .perpendicular_through(self.line.clone_without_node(), object.clone_without_node()),
            self.line.clone_without_node(),
            no_display,
        );
        let proj = context.to_complex(proj);
        let obj = context.to_complex(object);
        let img = context.add(proj.clone_without_node(), context.sub(proj, obj));
        context.to_point_display(img, props)
    }
}

impl Transform<Expr<Line>> for Reflect {
    type Output = Expr<Line>;

    fn transform(
        &self,
        object: Expr<Line>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let inter =
            context.intersection(self.line.clone_without_node(), object.clone_without_node());
        let obj_dir = context.direction(self.line.clone_without_node());
        let self_dir = context.direction(self.line.clone_without_node());
        let dir_rel = context.div(obj_dir, self_dir.clone_without_node());
        let conj = context.add(
            context.real(dir_rel.clone_without_node()),
            context.neg(context.imaginary(dir_rel)),
        );
        let new_dir = context.mult(self_dir, conj);
        context.point_vector_display(inter, new_dir, props)
    }
}

impl Transform<Expr<Circle>> for Reflect {
    type Output = Expr<Circle>;

    fn transform(
        &self,
        object: Expr<Circle>,
        context: &mut CompileContext,
        props: Properties,
    ) -> Self::Output {
        let center = context.circle_center(object.clone_without_node());
        let radius = context.circle_radius(object);

        let new_center = self.transform(center, context, Properties::default());
        context.circle_display(new_center, radius, props)
    }
}

impl_dyn! {Reflect}

fn reflection(
    mut line: Expr<Line>,
    context: &CompileContext,
    mut props: Properties,
) -> TransformTypeExpr {
    let node = NoContentNode {
        display: props.get("display").maybe_unset(true),
    };
    props.ignore("default-label");
    props.finish(context);

    let mut node = HierarchyNode::new_dyn(node);
    node.extend_children(line.take_node());

    TransformTypeExpr::new(TransformType(Box::new(Reflect { line })), node)
}

#[derive(Debug)]
struct Compose {
    t1: TransformTypeExpr,
    t2: TransformTypeExpr,
}

impl Display for Compose {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Composition of ({}) and ({})", self.t1, self.t2)
    }
}

impl DynTransform for Compose {
    fn transform_any(
        &self,
        expr: AnyExpr,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let t2_res = self
            .t2
            .get()
            .unwrap()
            .0
            .transform_any(expr, context, Properties::default());
        self.t1
            .get()
            .unwrap()
            .0
            .transform_any(t2_res, context, props)
    }

    fn transform_type(&self, ty: Type) -> Option<Type> {
        let t2_res = self.t2.get()?.0.transform_type(ty)?;
        self.t1.get()?.0.transform_type(t2_res)
    }
}

fn compose(
    mut t1: TransformTypeExpr,
    mut t2: TransformTypeExpr,
    context: &CompileContext,
    mut props: Properties,
) -> TransformTypeExpr {
    let node = NoContentNode {
        display: props.get("display").maybe_unset(true),
    };
    props.ignore("default-label");
    props.finish(context);

    let mut node = HierarchyNode::new_dyn(node);
    node.extend_children(t1.take_node());
    node.extend_children(t2.take_node());

    TransformTypeExpr::new(TransformType(Box::new(Compose { t1, t2 })), node)
}

struct TransformOverload;

impl Overload for TransformOverload {
    fn get_returned_type(&self, params: &[AnyExpr]) -> Option<Type> {
        if params.len() == 2 && params[0].can_convert_to_derived("TransformType") {
            TransformTypeExpr::from(params[0].as_derived()?.clone_without_node())
                .get()
                .and_then(|v| v.0.transform_type(params[1].get_type()))
        } else {
            None
        }
    }

    fn unroll(
        &self,
        mut params: Vec<AnyExpr>,
        context: &mut CompileContext,
        props: Properties,
    ) -> AnyExpr {
        let ty = self.get_returned_type(&params).unwrap();
        let o = params.swap_remove(1).convert_to(ty, context);
        let t = TransformTypeExpr::from(params.swap_remove(0).try_into_derived().unwrap());

        t.get().unwrap().0.transform_any(o, context, props)
    }
}

pub fn register(library: &mut Library) {
    library
        .add(
            Function::new("homothety")
                .alias("scale")
                .overload(
                    |scale: Unitless, origin: Expr<Point>, context: &CompileContext, props| {
                        spiral(
                            origin,
                            Angle::from(number!(SCALAR ProcNum::zero())),
                            scale,
                            context,
                            props,
                        )
                    },
                )
                .overload(
                    |origin: Expr<Point>, scale: Unitless, context: &CompileContext, props| {
                        spiral(
                            origin,
                            Angle::from(number!(ANGLE ProcNum::zero())),
                            scale,
                            context,
                            props,
                        )
                    },
                ),
        )
        .add(
            Function::new("spiral")
                .alias("rotate")
                .overload(spiral)
                .overload(
                    |origin: Expr<Point>,
                     scale: Unitless,
                     angle: Angle,
                     context: &CompileContext,
                     props| { spiral(origin, angle, scale, context, props) },
                )
                .overload(
                    |angle: Angle,
                     origin: Expr<Point>,
                     scale: Unitless,
                     context: &CompileContext,
                     props| { spiral(origin, angle, scale, context, props) },
                )
                .overload(
                    |angle: Angle,
                     scale: Unitless,
                     origin: Expr<Point>,
                     context: &CompileContext,
                     props| { spiral(origin, angle, scale, context, props) },
                )
                .overload(
                    |scale: Unitless,
                     angle: Angle,
                     origin: Expr<Point>,
                     context: &CompileContext,
                     props| { spiral(origin, angle, scale, context, props) },
                )
                .overload(
                    |scale: Unitless,
                     origin: Expr<Point>,
                     angle: Angle,
                     context: &CompileContext,
                     props| { spiral(origin, angle, scale, context, props) },
                )
                .overload(
                    |origin: Expr<Point>, angle: Angle, context: &CompileContext, props| {
                        spiral(
                            origin,
                            angle,
                            Unitless::from(number!(SCALAR ProcNum::one())),
                            context,
                            props,
                        )
                    },
                )
                .overload(
                    |angle: Angle, origin: Expr<Point>, context: &CompileContext, props| {
                        spiral(
                            origin,
                            angle,
                            Unitless::from(number!(SCALAR ProcNum::one())),
                            context,
                            props,
                        )
                    },
                ),
        )
        .add(
            Function::new("translation")
                .alias("translate")
                .overload(translation),
        )
        .add(
            Function::new("reflect")
                .alias("reflection")
                .overload(reflection),
        )
        .add(
            Function::new("transform")
                .alias_method(ty::derived("TransformType"), "transform")
                .alias_method(ty::derived("TransformType"), "t")
                .overload(TransformOverload),
        )
        .add(
            Function::new("compose")
                .alias_method(Type::Derived("TransformType"), "compose")
                .overload(compose),
        );
}
