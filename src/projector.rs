use std::sync::Arc;
use std::{collections::HashMap, rc::Rc};

use serde::Serialize;

use uuid::Uuid;

use crate::{
    generator::{
        critic::{self, evaluate_expression_simple},
        geometry::{self},
        Complex, EvaluationError,
    },
    script::{figure::Figure, unroll, Expression, HashableArc, Weighed},
};

#[cfg(test)]
mod testing {
    use std::{path::PathBuf, sync::Arc};

    use crate::{
        drawer,
        generator::Complex,
        script::{
            figure::Figure,
            unroll::PointMeta,
            Expression::{FreePoint, Line, AnglePoint},
            Weighed,
        },
    };

    use super::project;

    #[test]
    fn test_project() {
        let x:u8 = 1;
        let gen_points: [Complex; 3] = [
            Complex {
                real: 0.3463,
                imaginary: 0.436,
            },
            Complex {
                real: 0.23,
                imaginary: 0.87,
            },
            Complex {
                real: 0.312,
                imaginary: 0.314,
            },
        ];

        let fig = Figure {
            points: vec![
                (
                    Arc::new(Weighed::one(FreePoint(0))),
                    PointMeta {
                        letter: 'A',
                        primes: 0,
                        index: None,
                    },
                ),
                (
                    Arc::new(Weighed::one(FreePoint(1))),
                    PointMeta {
                        letter: 'B',
                        primes: 0,
                        index: None,
                    },
                ),
                (
                    Arc::new(Weighed::one(FreePoint(2))),
                    PointMeta {
                        letter: 'C',
                        primes: 0,
                        index: None,
                    },
                ),
            ],
            lines: vec![
                Arc::new(Weighed::one(Line(
                    Arc::new(Weighed::one(FreePoint(0))),
                    Arc::new(Weighed::one(FreePoint(1))),
                ))),
                Arc::new(Weighed::one(Line(
                    Arc::new(Weighed::one(FreePoint(1))),
                    Arc::new(Weighed::one(FreePoint(2))),
                ))),
            ],
            angles: vec![
                (
                    Arc::new(Weighed::one(AnglePoint(
                        Arc::new(Weighed::one(FreePoint(0))),
                        Arc::new(Weighed::one(FreePoint(1))),
                        Arc::new(Weighed::one(FreePoint(2))), 
                    ))),
                    x,
                ),
            ],
            canvas_size: (200, 200),
        };

        // let path_latex = PathBuf::from("testoutputs\\test.latex");
        let path_svg = PathBuf::from("testoutputs\\test.svg");
        let path_json = PathBuf::from("testoutputs\\test.json");
        // let path_raw = PathBuf::from("testoutputs\\test.raw");

        let pr = &project(&fig, &gen_points).unwrap();

        // drawer::latex::draw(&path_latex, (fig.canvas_size.0, fig.canvas_size.1), pr);

        drawer::svg::draw(&path_svg, (fig.canvas_size.0, fig.canvas_size.1), pr);

        drawer::json::draw(&path_json, (fig.canvas_size.0, fig.canvas_size.1), pr);

        // drawer::raw::draw(&path_raw, (fig.canvas_size.0, fig.canvas_size.1), pr);
    }
}

pub struct Blueprint {
    pub points: Vec<RenderedPoint>,
    pub lines: Vec<RenderedLine>,
    pub angles: Vec<RenderedAngle>,
}

#[derive(Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub enum Rendered {
    Point(Rc<RenderedPoint>),
    Line(RenderedLine),
    Angle(RenderedAngle),
}

#[derive(Serialize)]
pub struct Output {
    /// Map containing points as keys and point structs as values
    pub map: HashMap<HashableArc<Weighed<Expression>>, Rc<RenderedPoint>>,
    /// final product of the project function
    pub vec_rendered: Vec<Rendered>,
}

#[derive(Debug, Serialize)]
pub struct RenderedPoint {
    /// The point's label
    pub label: String,
    /// Point's position
    pub position: Complex,
    /// Point's custom uuid
    pub uuid: Uuid,
    //pub expr: Arc<Weighed<Expression>>,
}

#[derive(Serialize)]
pub struct RenderedLine {
    /// The line's label
    pub label: String,
    /// The line's thickness
    /// Two ends of the line
    pub points: (Complex, Complex),
    /// Expression defining the line
    pub expr: Arc<Weighed<Expression>>,
}

#[derive(Serialize)]
pub struct RenderedAngle {
    /// The angle's label
    pub label: String,
    /// Points defining the angle
    pub points: (Complex, Complex, Complex),
    /// Number of arcs in the angle
    pub no_arcs: u8,
    /// Expression that the angle was defined by
    pub expr: Arc<Weighed<Expression>>,
    // Value of the angle (who'd have guessed)
    pub angle_value: f64,
}

fn get_angle_points(
    angle: &Arc<Weighed<Expression>>,
    generated_points: &[Complex],
) -> (Complex, Complex, Complex) {
    match &angle.object {
        Expression::AnglePoint(p1, p2, p3) => {
            let arm1 = evaluate_expression_simple(p1, generated_points).unwrap();
            let origin = evaluate_expression_simple(p2, generated_points).unwrap();
            let arm2 = evaluate_expression_simple(p3, generated_points).unwrap();

            (arm1.0, origin.0, arm2.0)
        }
        Expression::AngleLine(ln1, ln2) => {
            let ev_ln1 = evaluate_expression_simple(ln1, generated_points).unwrap();
            let ev_ln2 = evaluate_expression_simple(ln2, generated_points).unwrap();

            let origin = geometry::get_crossing(ev_ln1.0, ev_ln2.0).unwrap();

            if ev_ln1.0.imaginary == 0.0 {
                let arm1 = Complex::new(origin.real, origin.imaginary + 2.0);
                let arm2 = Complex::new(
                    origin.real + 2.0,
                    ev_ln2.0.real * (origin.real + 2.0) + ev_ln2.0.imaginary,
                );
                (arm1, origin, arm2)
            } else if ev_ln2.0.imaginary == 0.0 {
                let arm1 = Complex::new(
                    origin.real + 2.0,
                    ev_ln1.0.real * (origin.real + 2.0) + ev_ln1.0.imaginary,
                );
                let arm2 = Complex::new(origin.real, origin.imaginary + 2.0);
                (arm1, origin, arm2)
            } else {
                let arm1 = Complex::new(
                    origin.real + 2.0,
                    ev_ln1.0.real * (origin.real + 2.0) + ev_ln1.0.imaginary,
                );
                let arm2 = Complex::new(
                    origin.real + 2.0,
                    ev_ln2.0.real * (origin.real + 2.0) + ev_ln2.0.imaginary,
                );
                (arm1, origin, arm2)
            }
        }
        _ => unreachable!(),
    }
}

fn get_angle_value(angle: &Arc<Weighed<Expression>>, generated_points: &[Complex]) -> f64 {
    match &angle.object {
        Expression::AnglePoint(p1, p2, p3) => {
            let arm1 = evaluate_expression_simple(p1, generated_points).unwrap();
            let origin = evaluate_expression_simple(p2, generated_points).unwrap();
            let arm2 = evaluate_expression_simple(p3, generated_points).unwrap();

            geometry::get_angle(arm1.0, origin.0, arm2.0)
        }
        Expression::AngleLine(ln1, ln2) => {
            let ev_ln1 = evaluate_expression_simple(ln1, generated_points).unwrap();
            let ev_ln2 = evaluate_expression_simple(ln2, generated_points).unwrap();

            let origin = geometry::get_crossing(ev_ln1.0, ev_ln2.0).unwrap();

            if ev_ln1.0.imaginary == 0.0 {
                let arm1 = Complex::new(origin.real, origin.imaginary + 2.0);
                let arm2 = Complex::new(
                    origin.real + 2.0,
                    ev_ln2.0.real * (origin.real + 2.0) + ev_ln2.0.imaginary,
                );
                return geometry::get_angle(arm1, origin, arm2);
            } else if ev_ln2.0.imaginary == 0.0 {
                let arm1 = Complex::new(
                    origin.real + 2.0,
                    ev_ln1.0.real * (origin.real + 2.0) + ev_ln1.0.imaginary,
                );
                let arm2 = Complex::new(origin.real, origin.imaginary + 2.0);
                return geometry::get_angle(arm1, origin, arm2);
            }
            let arm1 = Complex::new(
                origin.real + 2.0,
                ev_ln1.0.real * (origin.real + 2.0) + ev_ln1.0.imaginary,
            );
            let arm2 = Complex::new(
                origin.real + 2.0,
                ev_ln2.0.real * (origin.real + 2.0) + ev_ln2.0.imaginary,
            );
            geometry::get_angle(arm1, origin, arm2)
        }
        _ => unreachable!(),
    }
}

fn get_line_ends(figure: &Figure, ln_c: Complex) -> (Complex, Complex) {
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
        geometry::get_crossing(
            ln_c,
            geometry::get_line(Complex::new(0.0, height), Complex::new(1.0, height)),
        ),
        geometry::get_crossing(
            ln_c,
            geometry::get_line(Complex::new(0.0, 0.0), Complex::new(0.0, 1.0)),
        ),
        geometry::get_crossing(
            ln_c,
            geometry::get_line(Complex::new(width, 0.0), Complex::new(width, 1.0)),
        ),
        geometry::get_crossing(
            ln_c,
            geometry::get_line(Complex::new(0.0, 0.0), Complex::new(1.0, 0.0)),
        ),
    ];

    // If the a in ax+b is negative, line is "going down".
    let a = ln_c.real;

    // println!("a is {a}");
    // println!("intersection points are {:#?}", intersections);

    #[allow(clippy::cast_precision_loss)]
    if a < 0f64 {
        // There must be one intersection with lines 0/1 and 2/3
        let i1 = intersections[0].as_ref().map_or_else(
            |_| intersections[1].as_ref().unwrap(),
            |x| {
                if (x.real > 0f64 && x.real < width) || intersections[1].is_err() {
                    x
                } else {
                    intersections[1].as_ref().unwrap()
                }
            },
        );

        let i2 = intersections[3].as_ref().map_or_else(
            |_| intersections[2].as_ref().unwrap(),
            |x| {
                if (x.real > 0f64 && x.real < width) || intersections[2].is_err() {
                    x
                } else {
                    intersections[2].as_ref().unwrap()
                }
            },
        );

        (*i1, *i2)
    } else {
        // There must be one intersection with lines 1/3 and 0/2
        let i1 = intersections[3].as_ref().map_or_else(
            |_| intersections[1].as_ref().unwrap(),
            |x| {
                if (x.real > 0f64 && x.real < width) || intersections[1].is_err() {
                    x
                } else {
                    intersections[1].as_ref().unwrap()
                }
            },
        );

        let i2 = intersections[0].as_ref().map_or_else(
            |_| intersections[2].as_ref().unwrap(),
            |x| {
                if (x.real > 0f64 && x.real < width) || intersections[2].is_err() {
                    x
                } else {
                    intersections[2].as_ref().unwrap()
                }
            },
        );

        (*i1, *i2)
    }
}

/// Takes the figure and rendered points and attempt to design a figure that can then be rendered in chosen format.
///
/// # Panics
/// Despite containing .unwrap() calls, it shouldn't panic.
///
/// # Errors
/// Returns an error if there is a problem with evaluating constructs (e. g. intersection of two parallel lines).
pub fn project(figure: &Figure, generated_points: &[Complex]) -> Result<Output, EvaluationError> {
    let points: Vec<Complex> = figure
        .points
        .iter()
        .map(|pt| Ok(critic::evaluate_expression_simple(&pt.0, generated_points)?.0))
        .collect::<Result<Vec<Complex>, EvaluationError>>()?;

    #[allow(clippy::cast_precision_loss)]
    let size1 = Complex::new(figure.canvas_size.0 as f64, figure.canvas_size.1 as f64);
    let size09 = size1 * 0.9;
    let size005 = size1 * 0.05;

    // Frame topleft point.
    let mut offset = points.get(0).copied().unwrap_or_default();

    for x in &points {
        if x.real < offset.real {
            offset.real = x.real;
        }

        if x.imaginary < offset.imaginary {
            offset.imaginary = x.imaginary;
        }
    }

    // println!("Points preoffset: {:?}", points);
    // println!("Offset: {offset}");
    let points: Vec<Complex> = points.into_iter().map(|x| x - offset).collect();
    // println!("Points postoffset: {:?}", points);

    // Frame bottomright point.
    let mut furthest = points.get(0).copied().unwrap_or_default();

    for x in &points {
        if x.real > furthest.real {
            furthest.real = x.real;
        }

        if x.imaginary > furthest.imaginary {
            furthest.imaginary = x.imaginary;
        }
    }

    // The scaled frame should be at most (and equal for at least one dimension) 90% of the size of the desired image (margins for rendering).
    let scale = f64::min(
        size09.real / furthest.real,
        size09.imaginary / furthest.imaginary,
    );
    // println!("furthest: {furthest}, scale: {scale}");

    let points: Vec<Complex> = points.into_iter().map(|x| x * scale + size005).collect();

    let mut blueprint_points = Vec::new();

    for (i, pt) in points.iter().enumerate() {
        let pt_label =
            unroll::construct_point_name(figure.points[i].1.letter, figure.points[i].1.primes);
        let id = Uuid::new_v4();
        blueprint_points.push(Rc::new(RenderedPoint {
            label: pt_label,
            position: *pt,
            uuid: id,
        }));
    }

    let mut iden = HashMap::new();
    for (i, pt) in figure.points.clone().iter().enumerate() {
        let point = HashableArc::new(Arc::clone(&pt.0));
        iden.insert(point, Rc::clone(&blueprint_points[i]));
    }

    let mut blueprint_lines = Vec::new();

    for ln in &figure.lines {
        let ln_c = evaluate_expression_simple(ln, generated_points)?;

        blueprint_lines.push(RenderedLine {
            label: String::new(),
            points: get_line_ends(figure, ln_c.0),
            expr: Arc::clone(ln),
        });
    }

    let mut blueprint_angles = Vec::new();

    for ang in &figure.angles {
        blueprint_angles.push(RenderedAngle {
            label: String::new(),
            points: get_angle_points(&ang.0, generated_points),
            no_arcs: ang.1,
            expr: Arc::clone(&ang.0),
            angle_value: get_angle_value(&ang.0, generated_points),
        });
    }

    // println!("{:#?}", blueprint_points);
    Ok(Output {
        map: iden,
        vec_rendered: blueprint_points
            .into_iter()
            .map(Rendered::Point)
            .chain(blueprint_lines.into_iter().map(Rendered::Line))
            .chain(blueprint_angles.into_iter().map(Rendered::Angle))
            .collect(),
    })
}
