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
use std::f64::consts::PI;

use serde::Serialize;

use crate::geometry;
use crate::geometry::{Circle, Complex, Line, ValueEnum};

use crate::script::figure::{CircleItem, Generated, Item, LineItem, PointItem, RayItem, SegmentItem};
use crate::script::math::{Entity, Expr, Flags, VarIndex};

struct Projector {
    /// Transform used by the projector
    pub transform_: Transform,
    /// Variables used by the figure
    pub variables: Vec<MathVariable>,
    /// Picture width
    pub width: f64,
    /// Picture height
    pub height: f64,
    /// Segments visible on the picture.
    pub segments: Vec<(Complex, Complex)>,
    /// Circles visible on the picture.
    pub circles: Vec<(Complex, f64)>
}

impl Projector {
    fn transform(&self, c: Complex) -> Complex {
        self.transform_.transform(c)
    }

    /// Gets the intersection points of the line with the picture's frame.
    fn get_line_ends(&self, ln_c: Line) -> (Complex, Complex) {
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

        let intersections = [
            geometry::get_intersection(
                ln_c,
                geometry::get_line(Complex::new(0.0, self.height), Complex::new(1.0, self.height)),
            ),
            geometry::get_intersection(
                ln_c,
                geometry::get_line(Complex::new(0.0, 0.0), Complex::new(0.0, 1.0)),
            ),
            geometry::get_intersection(
                ln_c,
                geometry::get_line(Complex::new(self.width, 0.0), Complex::new(self.width, 1.0)),
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
            let i1 = choose_intersection(0, 1)(self.width, &intersections);

            let i2 = choose_intersection(3, 2)(self.width, &intersections);

            (i1, i2)
        } else {
            // There must be one intersection with lines 1/3 and 0/2
            let i1 = choose_intersection(3, 1)(self.width, &intersections);

            let i2 = choose_intersection(0, 2)(self.width, &intersections);

            (i1, i2)
        }
    }

    fn get_label_position(&self, point: Complex) -> Complex {
        let mut vectors = Vec::new();

        // Checking the lines for proximity.
        for (a, b) in &self.segments {
            // Identifying the "first" point by the real axis.
            let (seg1, seg2) = if a.real < b.real {
                (*a, *b)
            } else {
                (*b, *a)
            };

            let ln = geometry::get_line(seg1, seg2);
            let distance = geometry::distance_pt_ln(point, ln);

            // Defining the little nudge applied to the seg1 and seg2 to also include the points defining the segment.
            let unit = ln.direction * 1e-2;
            let u1 = unit.real;
            let u2 = unit.imaginary;

            if distance < 1e-2 && seg1.real - u1 < point.real
                    && point.real < seg2.real + u1
                    && seg1.imaginary - u2 < point.imaginary && point.imaginary < seg2.imaginary + u2 {
                if geometry::distance_pt_pt(point, seg1) < 1.0 {
                    vectors.push(ln.direction);
                } else if geometry::distance_pt_pt(point, seg2) < 1.0 {
                    vectors.push(-ln.direction);
                } else {
                    vectors.push(ln.direction);
                    vectors.push(-ln.direction);
                }
            }
        }

        // Checking the circles for associated vectors.
        for &(center, radius) in &self.circles {
            if (geometry::distance_pt_pt(center, point) - radius).abs() < 1e-4 {
                let direction = (center - point).normalize().mul_i();

                vectors.push(direction);
                vectors.push(-direction);
            }
        }

        // Sorting by the complex number argument.
        vectors.sort_by(|a, b| a.arg().partial_cmp(&b.arg()).unwrap());

        let mut vec_iter = vectors.iter();
        vec_iter.next();

        if vectors.is_empty() {
            // No vectors associated with the given point.
            Complex::new(2.0, 2.0)
        } else if vectors.len() == 1 {
            // Only one vector which is associated with the given point.
            -4.0 * vectors.first().unwrap().normalize()
        } else {
            let mut flip = false;
            let mut biggest_angle = 0.0;
            // Vectors between which the label should be located.
            let mut label_vectors = (Complex::default(), Complex::default());

            // If there is more than one associated vector.
            for vec in &vectors {
                if let Some(vec_next) = vec_iter.next() {
                    let angle = vec_next.arg() - vec.arg();
                    if angle > biggest_angle {
                        biggest_angle = angle;
                        label_vectors = (*vec, *vec_next);
                    }
                } else {
                    let first = vectors.first().unwrap();
                    let last = vectors.last().unwrap();
                    let angle = 2.0 * PI - (last.arg() - first.arg());
                    if angle > biggest_angle {
                        biggest_angle = angle;
                        label_vectors = (*first, *last);
                        flip = true; // The only case when the label offset will have to be flipped (scale -1)
                    }
                    break;
                }
            }

            // We get the bisector angle.
            let bisector_angle = (label_vectors.1.arg() + label_vectors.0.arg()) / 2.0;

            // This is just the standard complex number formula.
            let mut bisector_vec = Complex::new(bisector_angle.cos(), bisector_angle.sin());

            // to do -> better scaling
            if flip {
                biggest_angle *= -1.0;
                bisector_vec *= -1.0;
            }

            let scale = 540.0 / biggest_angle.to_degrees().abs();

            bisector_vec * 3.0 * scale
        }
    }
}

trait UnVar<T> {
    /// Returns the actual variable value.
    fn un_var(&self, id: VarIndex) -> Option<T>;
}

impl UnVar<Complex> for Projector {
    fn un_var(&self, id: VarIndex) -> Option<Complex> {
        self.variables[id.0].meta.as_complex()
    }
}

impl UnVar<Line> for Projector {
    fn un_var(&self, id: VarIndex) -> Option<Line> {
        self.variables[id.0].meta.as_line()
    }
}

impl UnVar<Circle> for Projector {
    fn un_var(&self, id: VarIndex) -> Option<Circle> {
        self.variables[id.0].meta.as_circle()
    }
}

trait Project<T> {
    type Result;

    fn project(&mut self, item: T) -> Self::Result;
}

impl Project<Item> for Projector {
    type Result = Rendered;

    fn project(&mut self, item: Item) -> Self::Result {
        match item {
            Item::Point(v) => Rendered::Point(self.project(v)),
            Item::Circle(v) => Rendered::Circle(self.project(v)),
            Item::Line(v) => Rendered::Line(self.project(v)),
            Item::Ray(v) => Rendered::Ray(self.project(v)),
            Item::Segment(v) => Rendered::Segment(self.project(v)),
        }
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

impl Rendered {
    #[must_use]
    pub fn as_point_mut(&mut self) -> Option<&mut RenderedPoint> {
        match self {
            Self::Point(p) => Some(p),
            _ => None
        }
    }
}

type MathVariable = Expr<ValueEnum>;

/// The final product passed to the drawers.
#[derive(Serialize)]
pub struct Output {
    /// final product of the project function
    pub rendered: Vec<Rendered>,
    /// Entities used by the figure
    pub entities: Vec<Entity<ValueEnum>>,
    /// Variables used by the figure
    pub variables: Vec<MathVariable>,
    /// Picture size
    pub canvas_size: (usize, usize)
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

impl Project<PointItem> for Projector {
    type Result = RenderedPoint;

    fn project(&mut self, item: PointItem) -> Self::Result {
        RenderedPoint {
            label_position: Complex::zero(),
            position: self.transform(self.un_var(item.id).unwrap()),
            item
        }
    }
}

#[derive(Serialize)]
pub struct RenderedLine {
    /// Two ends of the line (calculated f)
    pub points: (Complex, Complex),
    /// Line's defining item.
    pub item: LineItem
}

impl Project<LineItem> for Projector {
    type Result = RenderedLine;
    
    fn project(&mut self, item: LineItem) -> Self::Result {
        let mut ln_c: Line = self.un_var(item.id).unwrap();
        ln_c.origin = self.transform(ln_c.origin);
        let points = self.get_line_ends(ln_c);
        self.segments.push(points);

        RenderedLine {
            points,
            item
        }
    }
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

impl Project<SegmentItem> for Projector {
    type Result = RenderedSegment;

    fn project(&mut self, item: SegmentItem) -> Self::Result {
        let seg1 = self.transform(self.un_var(item.p_id).unwrap());
        let seg2 = self.transform(self.un_var(item.q_id).unwrap());
        self.segments.push((seg1, seg2));

        RenderedSegment {
            points: (seg1, seg2),
            item
        }
    }
}

#[derive(Serialize)]
pub struct RenderedRay {
    /// Points defining the ray
    pub points: (Complex, Complex),
    /// The defining item.
    pub item: RayItem
}

impl Project<RayItem> for Projector {
    type Result = RenderedRay;

    fn project(&mut self, item: RayItem) -> Self::Result {
        let ray_a = self.transform(self.un_var(item.p_id).unwrap());
        let ray_b = self.transform(self.un_var(item.q_id).unwrap());

        let line = geometry::get_line(ray_a, ray_b);
        let ends = self.get_line_ends(line);

        let vec1 = (ray_b - ray_a).normalize();
        let vec2 = (ends.1 - ray_a).normalize();
        let second_point;

        if vec1.real < 0.5 && vec1.real > -0.5 {
            if (vec1.imaginary - vec2.imaginary).abs() < 1e-4 {
                second_point = ends.1;
            } else {
                second_point = ends.0;
            }
        } else if (vec1.real - vec2.real).abs() < 1e-4 {
            second_point = ends.1;
        } else {
            second_point = ends.0;
        }
        self.segments.push((ray_a, second_point));

        RenderedRay {
            points: (ray_a, second_point),
            item
        }
    }
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

impl Project<CircleItem> for Projector {
    type Result = RenderedCircle;

    fn project(&mut self, item: CircleItem) -> Self::Result {
        let circle: Circle = self.un_var(item.id).unwrap();
        let center = self.transform(circle.center);
        let radius = circle.radius * self.transform_.scale;
        self.circles.push((center, radius));

        RenderedCircle {
            center,
            radius,
            item
        }
    }
}

// /// Function getting the points defining the angle from the Expression defining it.
// ///
// /// # Panics
// /// If given invalid data.
// fn get_angle_points(
//     angle: &ExprKind,
//     variables: &[Expr<ValueEnum>],
// ) -> (Complex, Complex, Complex) {
//     match angle {
//         ExprKind::ThreePointAngle { p, q, r } => {
//             let arm1 = variables[p.0].meta.as_complex().unwrap();
//             let origin = variables[q.0].meta.as_complex().unwrap();
//             let arm2 = variables[r.0].meta.as_complex().unwrap();
//
//             (arm1, origin, arm2)
//         }
//         ExprKind::ThreePointAngleDir { p, q, r } => {
//             let arm1 = variables[p.0].meta.as_complex().unwrap();
//             let origin = variables[q.0].meta.as_complex().unwrap();
//             let arm2 = variables[r.0].meta.as_complex().unwrap();
//
//             (arm1, origin, arm2)
//         }
//         ExprKind::TwoLineAngle { k, l } => {
//             let ev_ln1 = variables[k.0].meta.as_line().unwrap();
//             let ev_ln2 = variables[l.0].meta.as_line().unwrap();
//
//             let origin = geometry::get_intersection(ev_ln1, ev_ln2);
//
//             (
//                 ev_ln1.origin + ev_ln1.direction,
//                 origin,
//                 ev_ln2.origin + ev_ln2.direction,
//             )
//         }
//         _ => unreachable!(),
//     }
// }

struct Transform {
    offset: Complex,
    scale: f64,
    margin: Complex
}

impl Transform {
    /// Translates generator coordinates to projector coordinates.
    fn transform(&self, pt: Complex) -> Complex {
        (pt + self.offset) * self.scale + self.margin
    }
}

// /// Function that outputs the vector containing the angles.
// ///
// /// # Panics
// /// It shouldn't panic.
// fn angles(
//     figure: &Figure,
//     offset: Complex,
//     scale: f64,
//     size: Complex,
//     args: &EvaluationArgs,
// ) -> Vec<RenderedAngle> {
//     let mut blueprint_angles = Vec::new();
//     for ang in &figure.angles {
//         let angle_points = get_angle_points(&ang.0, args);
//         blueprint_angles.push(RenderedAngle {
//             label: String::new(),
//             points: (
//                 transform(offset, scale, size, angle_points.0),
//                 transform(offset, scale, size, angle_points.1),
//                 transform(offset, scale, size, angle_points.2),
//             ),
//             no_arcs: ang.1,
//             expr: Arc::clone(&ang.0),
//             angle_value: ang.0.evaluate(args),
//             style: ang.2,
//         });
//     }
//     blueprint_angles
// }

/// Takes the figure and rendered adjustables and attempts to design a figure that can then be rendered in chosen format.
pub fn project(
    figure: Generated,
    _flags: &Arc<Flags>,
    canvas_size: (usize, usize)
) -> Output {
    let entities: Vec<_> = figure.entities;
    let variables: Vec<_> = figure.variables;
    let items = figure.items;

    // Collect points (currently only the actual points, in the future also some more, like circle borders etc.)
    let points: Vec<_> = items.iter().filter_map(|x| match x {
        Item::Point(pt) => variables[pt.id.0].meta.as_complex(),
        _ => None
    }).collect();

    // Frame top left point.
    let top_left = Complex::new(
        points.iter()
            .map(|pt| pt.real)
            .reduce(f64::min).unwrap_or_default(),
        points.iter()
            .map(|pt| pt.imaginary)
            .reduce(f64::min).unwrap_or_default()
    );

    let offset = -top_left;

    // Frame bottom right point.
    let furthest = Complex::new(
        points.iter()
            .map(|pt| pt.real)
            .reduce(f64::max).unwrap_or_default(),
        points.iter()
            .map(|pt| pt.imaginary)
            .reduce(f64::max).unwrap_or_default()
    );

    // println!("{top_left}/{furthest}");
    let mut total_size = furthest + offset;
    total_size.real = f64::max(total_size.real, 0.1);
    total_size.imaginary = f64::max(total_size.imaginary, 0.1);

    #[allow(clippy::cast_precision_loss)]
    let size1 = Complex::new(canvas_size.0 as f64, canvas_size.1 as f64);
    let size09 = size1 * 0.9;
    let size005 = size1 * 0.05;

    // The scaled frame should be at most (and equal for at least one dimension) 90% of the size of the desired image (margins for rendering).
    let scale = f64::min(
        size09.real / total_size.real,
        size09.imaginary / total_size.imaginary,
    );

    // println!("Scale: {scale}");
    // println!("Frame size: {total_size}");

    // let points: Vec<Complex> = points.into_iter().map(|x| x * scale + size005).collect();
    let mut projector = Projector {
        transform_: Transform {
            offset,
            scale,
            margin: size005
        },
        variables,
        width: size1.real,
        height: size1.imaginary,
        segments: Vec::new(),
        circles: Vec::new()
    };

    let mut rendered: Vec<_> = items.into_iter()
        .map(|v| projector.project(v))
        .collect();

    for point in rendered.iter_mut().filter_map(Rendered::as_point_mut) {
        point.label_position = projector.get_label_position(point.position);
    }

    Output {
        rendered,
        entities,
        variables: projector.variables,
        canvas_size
    }
}
