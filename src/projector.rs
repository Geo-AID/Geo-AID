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

use std::sync::Arc;
use std::{collections::HashMap, rc::Rc};

use serde::Serialize;

use uuid::Uuid;
use crate::geometry::{Complex, ValueEnum};

use crate::labels::point_label_position;
use crate::script::figure::{CircleItem, Figure, Item, LineItem, MathString, PointItem, RayItem, SegmentItem, Style};
use crate::script::math;
use crate::script::math::{Entity, Expr, Flags, VarIndex};

trait Project {
    fn project() {

    }    
}

/// Enum representing the things that are later drawn in the drawers.
#[derive(Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum Rendered {
    Point(RenderedPoint),
    Line(RenderedLine),
    Angle(RenderedAngle),
    Segment(RenderedSegment),
    Ray(RenderedRay),
    Circle(RenderedCircle),
}

/// The final product passed to the drawers.
#[derive(Serialize)]
pub struct Output {
    /// final product of the project function
    pub vec_rendered: Vec<Rendered>,
    /// Entities used by the figure
    pub entities: Vec<Entity<VarIndex, usize>>,
    /// Variables used by the figure
    pub variables: Vec<math::Expr<math::Any<VarIndex>, usize>>,
    /// Figure items.
}

#[derive(Debug, Serialize)]
pub struct RenderedPoint {
    /// Label's position relative to point's
    pub label_position: Complex,
    /// Point's position on the canvas.
    pub position: Complex,
    /// Point's defining item.
    pub item: PointItem,
}



#[derive(Serialize)]
pub struct RenderedLine {
    /// Two ends of the line (calculated f)
    pub points: (Complex, Complex),
    /// Line's defining item.
    pub item: LineItem
}

#[derive(Serialize)]
pub struct RenderedAngle {
    /// Points defining the angle
    pub points: (Complex, Complex, Complex),
    /// Self-explanatory
    pub angle_value: f64,
    /// The defining item.
    pub item: () // placeholder
}
#[derive(Serialize)]
pub struct RenderedSegment {
    /// Points defining the segment
    pub points: (Complex, Complex),
    /// The defining item.
    pub item: SegmentItem
}

#[derive(Serialize)]
pub struct RenderedRay {
    /// Points defining the ray
    pub points: (Complex, Complex),
    /// The defining item.
    pub item: RayItem
}

#[derive(Serialize)]
pub struct RenderedCircle {
    /// Center of the circle
    pub center: Complex,
    /// Self-explanatory
    pub radius: f64,
    /// The defining item.
    pub item: CircleItem
}

/// Function getting the points defining the angle from the Expression defining it.
///
/// # Panics
/// It panics when the two lines that you are trying find crossing point of, are parallel.
fn get_angle_points(
    angle: &Arc<Expression<ScalarExpr>>,
    args: &EvaluationArgs,
) -> (Complex, Complex, Complex) {
    match &angle.kind {
        ScalarExpr::AnglePoint(AnglePoint { arm1, origin, arm2 }) => {
            let arm1 = arm1.evaluate(args);
            let origin = origin.evaluate(args);
            let arm2 = arm2.evaluate(args);

            (arm1, origin, arm2)
        }
        ScalarExpr::AngleLine(AngleLine { k, l }) => {
            let ev_ln1 = k.evaluate(args);
            let ev_ln2 = l.evaluate(args);

            let origin = geometry::get_intersection(ev_ln1, ev_ln2);

            (
                ev_ln1.origin + ev_ln1.direction,
                origin,
                ev_ln2.origin + ev_ln2.direction,
            )
        }
        _ => unreachable!(),
    }
}

/// Function getting the intersection points of the line with the picture's frame.
fn get_line_ends(figure: &Figure, ln_c: Line) -> (Complex, Complex) {
    fn choose_intersection(i: usize, j: usize) -> impl Fn(f64, &[Complex]) -> Complex {
        move |width, intersections| {
            let x = intersections[i];

            if x.real > 0f64 && x.real < width {
                x
            } else {
                intersections[j]
            }
        }
    }

    // +--0--+
    // |     |
    // 1     2
    // |     |
    // +--3--+

    #[allow(clippy::cast_precision_loss)]
    let width = figure.canvas_size.0 as f64;
    #[allow(clippy::cast_precision_loss)]
    let height = figure.canvas_size.1 as f64;

    let intersections = [
        geometry::get_intersection(
            ln_c,
            geometry::get_line(Complex::new(0.0, height), Complex::new(1.0, height)),
        ),
        geometry::get_intersection(
            ln_c,
            geometry::get_line(Complex::new(0.0, 0.0), Complex::new(0.0, 1.0)),
        ),
        geometry::get_intersection(
            ln_c,
            geometry::get_line(Complex::new(width, 0.0), Complex::new(width, 1.0)),
        ),
        geometry::get_intersection(
            ln_c,
            geometry::get_line(Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)),
        ),
    ];

    // If the product of the real and imaginary is negative, line is "going down".
    let a = ln_c.direction.imaginary * ln_c.direction.real;

    #[allow(clippy::cast_precision_loss)]
    if a < 0f64 {
        // There must be one intersection with lines 0/1 and 2/3
        let i1 = choose_intersection(0, 1)(width, &intersections);

        let i2 = choose_intersection(3, 2)(width, &intersections);

        (i1, i2)
    } else {
        // There must be one intersection with lines 1/3 and 0/2
        let i1 = choose_intersection(3, 1)(width, &intersections);

        let i2 = choose_intersection(0, 2)(width, &intersections);

        (i1, i2)
    }
}

/// Pure utitlity function, used for scaling and transforming points which were missed by fn `project`().
fn transform(offset: Complex, scale: f64, size: Complex, pt: Complex) -> Complex {
    (pt - offset) * scale + size
}

/// Function that outputs the vector contaning the lines.
/// ///
/// # Panics
/// It shouldn't panic.
fn project_lines(
    figure: &Figure,
    offset: Complex,
    scale: f64,
    size: Complex,
    args: &EvaluationArgs,
) -> Vec<RenderedLine> {
    let mut blueprint_lines = Vec::new();
    for ln in &figure.lines {
        let mut ln_c = ln.0.evaluate(args);
        ln_c.origin = transform(offset, scale, size, ln_c.origin);
        let line_ends = get_line_ends(figure, ln_c);
        blueprint_lines.push(RenderedLine {
            label: String::new(),
            points: (line_ends.0, line_ends.1),
            expr: Arc::clone(&ln.0),
            style: ln.1,
        });
    }
    blueprint_lines;


}

/// Function that outputs the vector containing the angles.
///
/// # Panics
/// It shouldn't panic.
fn angles(
    figure: &Figure,
    offset: Complex,
    scale: f64,
    size: Complex,
    args: &EvaluationArgs,
) -> Vec<RenderedAngle> {
    let mut blueprint_angles = Vec::new();
    for ang in &figure.angles {
        let angle_points = get_angle_points(&ang.0, args);
        blueprint_angles.push(RenderedAngle {
            label: String::new(),
            points: (
                transform(offset, scale, size, angle_points.0),
                transform(offset, scale, size, angle_points.1),
                transform(offset, scale, size, angle_points.2),
            ),
            no_arcs: ang.1,
            expr: Arc::clone(&ang.0),
            angle_value: ang.0.evaluate(args),
            style: ang.2,
        });
    }
    blueprint_angles
}

/// Function that outputs the vector contaning the segments.
///
/// # Panics
/// It shouldn't panic.
fn segments(
    figure: &Figure,
    offset: Complex,
    scale: f64,
    size: Complex,
    args: &EvaluationArgs,
) -> Vec<RenderedSegment> {
    let mut blueprint_segments = Vec::new();
    for segment in &figure.segments {
        let seg1 = segment.0.evaluate(args);
        let seg2 = segment.1.evaluate(args);
        blueprint_segments.push(RenderedSegment {
            label: String::new(),
            points: (
                transform(offset, scale, size, seg1),
                transform(offset, scale, size, seg2),
            ),
            style: segment.2,
        });
    }
    blueprint_segments
}

fn rays(
    figure: &Figure,
    offset: Complex,
    scale: f64,
    size: Complex,
    args: &EvaluationArgs,
) -> Vec<RenderedRay> {
    let mut blueprint_rays = Vec::new();
    for ray in &figure.rays {
        let ray_a = ray.0.evaluate(args);
        let ray_b = ray.1.evaluate(args);

        let ray_a = transform(offset, scale, size, ray_a);
        let ray_b = transform(offset, scale, size, ray_b);

        let line = get_line(ray_a, ray_b);
        let intercepts = get_line_ends(figure, line);

        let vec1 = (ray_b - ray_a).normalize();
        let vec2 = (intercepts.1 - ray_a).normalize();
        let second_point;

        if vec1.real < 0.5 && vec1.real > -0.5 {
            if (vec1.imaginary - vec2.imaginary).abs() < 1e-4 {
                second_point = intercepts.1;
            } else {
                second_point = intercepts.0;
            }
        } else if (vec1.real - vec2.real).abs() < 1e-4 {
            second_point = intercepts.1;
        } else {
            second_point = intercepts.0;
        }

        blueprint_rays.push(RenderedRay {
            label: String::new(),
            points: (ray_a, second_point),
            draw_point: ray_b,
            style: ray.2,
        });
    }

    blueprint_rays
}

fn circles(
    figure: &Figure,
    offset: Complex,
    scale: f64,
    size: Complex,
    args: &EvaluationArgs,
) -> Vec<RenderedCircle> {
    let mut blueprint_circles = Vec::new();
    for circle_main in &figure.circles {
        let circle = circle_main.0.evaluate(args);
        let center = transform(offset, scale, size, circle.center);
        let draw_point = Complex::new(circle.center.real + circle.radius, circle.center.imaginary);
        let sc_rad = circle.radius * scale;
        blueprint_circles.push(RenderedCircle {
            label: String::new(),
            center,
            draw_point: transform(offset, scale, size, draw_point),
            radius: sc_rad,
            style: circle_main.1,
        });
    }

    blueprint_circles
}

/// Takes the figure and rendered adjustables and attempts to design a figure that can then be rendered in chosen format.
pub fn project(
    figure: Figure,
    generated_points: &[ValueEnum],
    flags: &Arc<Flags>,
    canvas_size: (usize, usize)
) -> Output {
    let entities: Vec<_> = figure.entities
        .into_iter()
        .map(|ent| Entity {
            kind: ent.kind,
            meta: generated_points[ent.meta]
        })
        .collect();
    let variables: Vec<_> = figure.variables
        .into_iter()
        .map(|var| Expr {
            kind: var.kind,
            meta: generated_points[var.meta]
        })
        .collect();
    let items = figure.items;

    // Collect points (currently only the actual points, in the future also some more, like circle borders etc)
    let points: Vec<_> = items.iter().flat_map(|x| match x {
        Item::Point(pt) => variables[pt.id].meta.as_complex(),
        _ => None
    }).collect();

    // Frame top left point.
    let mut topleft = Complex::new(
        points.iter()
            .map(|pt| pt.real)
            .min().unwrap_or_default(),
        points.iter()
            .map(|pt| pt.imaginary)
            .min().unwrap_or_default()
    );

    let offset = -topleft;

    // Frame bottom right point.
    let mut furthest = Complex::new(
        points.iter()
            .map(|pt| pt.real)
            .max().unwrap_or_default(),
        points.iter()
            .map(|pt| pt.imaginary)
            .max().unwrap_or_default()
    );
    let total_size = furthest + offset;

    #[allow(clippy::cast_precision_loss)]
    let size1 = Complex::new(canvas_size.0 as f64, canvas_size.1 as f64);
    let size09 = size1 * 0.9;
    let size005 = size1 * 0.05;

    // The scaled frame should be at most (and equal for at least one dimension) 90% of the size of the desired image (margins for rendering).
    let scale = f64::min(
        size09.real / total_size.real,
        size09.imaginary / total_size.imaginary,
    );

    // let points: Vec<Complex> = points.into_iter().map(|x| x * scale + size005).collect();
    let sizes = (size1, size09, size005);

    let rendered = items.into_iter()
        .map(Project::project)

    let mut vec_associated = Vec::new();

    let mut blueprint_points = Vec::new();

    let blueprint_lines = project_lines(figure, offset, scale, size005, &args);

    let blueprint_angles = angles(figure, offset, scale, size005, &args);

    let blueprint_segments = segments(figure, offset, scale, size005, &args);

    let blueprint_rays = rays(figure, offset, scale, size005, &args);

    let blueprint_circles = circles(figure, offset, scale, size005, &args);

    for (i, pt) in points.iter().enumerate() {
        let math_string = figure.points[i].1.clone();
        let id = Uuid::new_v4();

        let point = *pt;

        blueprint_points.push(Rc::new(RenderedPoint {
            label_position: point_label_position(
                &blueprint_lines,
                &blueprint_angles,
                &blueprint_segments,
                &blueprint_rays,
                &blueprint_circles,
                vec_associated.clone(),
                point,
            ),
            position: *pt,
            uuid: id,
            math_string,
        }));
        vec_associated.clear();
    }

    // Creating a HashMap (the bridge between Expressions defining the points and those points).
    let mut iden = HashMap::new();
    for (i, pt) in figure.points.clone().iter().enumerate() {
        let point = HashableArc::new(Arc::clone(&pt.0));
        iden.insert(point, Rc::clone(&blueprint_points[i]));
    }

    Output {
        map: iden,
        vec_rendered: blueprint_points
            .into_iter()
            .map(Rendered::Point)
            .chain(blueprint_lines.into_iter().map(Rendered::Line))
            .chain(blueprint_angles.into_iter().map(Rendered::Angle))
            .chain(blueprint_segments.into_iter().map(Rendered::Segment))
            .chain(blueprint_rays.into_iter().map(Rendered::Ray))
            .chain(blueprint_circles.into_iter().map(Rendered::Circle))
            .collect(),
    }
}
