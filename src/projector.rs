use crate::{generator::{Complex, geometry, EvaluationError}, script::figure::{Figure, PointDefinition, LineDefinition, Point}};

pub struct Blueprint {
    pub points: Vec<RenderedPoint>,
    pub lines: Vec<RenderedLine>
}

pub enum Rendered {
    Point(RenderedPoint),
    Line(RenderedLine)
}

pub struct RenderedPoint {
    /// The point's label
    pub label: String,
    /// Point's position
    pub position: Complex
}

pub struct RenderedLine {
    /// The line's label
    pub label: String,
    /// The line's thickness
    /// Two ends of the line
    pub points: (Complex, Complex),
}

fn evaluate_line(line: &LineDefinition, figure_points: &Vec<Point>, generated_points: &Vec<Complex>, points: &mut Vec<Option<Complex>>) -> Result<Complex, EvaluationError> {
    Ok(match line {
        LineDefinition::TwoPoints(i1, i2) => geometry::get_line(
            evaluate_point(figure_points, *i1, generated_points, points)?,
            evaluate_point(figure_points, *i2, generated_points, points)?
        ),
    })
}

fn evaluate_point(figure_points: &Vec<Point>, index: usize, generated_points: &Vec<Complex>, points: &mut Vec<Option<Complex>>) -> Result<Complex, EvaluationError> {
    Ok(match &figure_points[index].definition {
        PointDefinition::Indexed(gen_index) => {
            match points[index] {
                Some(v) => v,
                None => {
                    points[index] = Some(generated_points[*gen_index]);
                    points[index].unwrap()
                }
            }
        },
        PointDefinition::Crossing(l1, l2) => {
            let l1 = evaluate_line(l1, figure_points, generated_points, points)?;
            let l2 = evaluate_line(l2, figure_points, generated_points, points)?;

            geometry::get_crossing(l1, l2)?
        },
    })
}

pub fn project(figure: &Figure, generated_points: Vec<Complex>) -> Result<Vec<Rendered>, EvaluationError> {
    let mut points = Vec::new();
    points.resize(figure.points.len(), None);

    // First, evaluate the exact position of each point
    for index in 0..figure.points.len() {
        evaluate_point(&figure.points, index, &generated_points, &mut points)?;
    }

    let size1 = Complex::new(figure.canvas_size.0 as f64, figure.canvas_size.1 as f64);
    let size09 = size1 * 0.9;
    let size005 = size1 * 0.05;

    let points = points.into_iter().collect::<Option<Vec<Complex>>>().unwrap();

    // Frame topleft point.
    let mut offset = Complex::new(0.0, 0.0);

    for x in points.iter().map(|x| Complex::new(0.0, 0.0) - *x) {
        if x.real > offset.real {
            offset.real = x.real
        }

        if x.imaginary > offset.imaginary {
            offset.imaginary = x.imaginary;
        }
    }

    let points: Vec<Complex> = points.into_iter().map(|x| x + offset).collect();

    // Frame bottomright point.
    let mut furthest = Complex::new(0.0, 0.0);

    for x in points.iter() {
        if x.real > furthest.real {
            furthest.real = x.real
        }

        if x.imaginary > furthest.imaginary {
            furthest.imaginary = x.imaginary;
        }
    }

    // The scaled frame should be at most (and equal for at least one dimension) 90% of the size of the desired image (margins for rendering).
    let scale = f64::min(size09.real / furthest.real, size09.imaginary / furthest.imaginary);

    let points: Vec<Complex> = points.into_iter().map(|x| x * scale + size005).collect();

    let mut blueprint_points = Vec::new();

    for (i, pt) in points.iter().enumerate() {
        blueprint_points.push(RenderedPoint {
            label: figure.points[i].label.clone(),
            position: *pt
        });
    }

    let mut blueprint_lines = Vec::new();

    for ln in figure.lines.iter() {
        let ln_c = match &ln.definition {
            LineDefinition::TwoPoints(p1, p2) => geometry::get_line(points[*p1], points[*p2]),
        };

        // +--0--+
        // |     |
        // 1     2
        // |     |
        // +--3--+

        let intersections = [
            geometry::get_crossing(ln_c, geometry::get_line(Complex::new(0.0, 0.0), Complex::new(1.0, 0.0))),
            geometry::get_crossing(ln_c, geometry::get_line(Complex::new(0.0, 0.0), Complex::new(0.0, 1.0))),
            geometry::get_crossing(ln_c, geometry::get_line(Complex::new(figure.canvas_size.0 as f64, 0.0), Complex::new(figure.canvas_size.0 as f64, 1.0))),
            geometry::get_crossing(ln_c, geometry::get_line(Complex::new(0.0, figure.canvas_size.1 as f64), Complex::new(1.0, figure.canvas_size.1 as f64))),
        ];

        // If the a in ax+b is negative, line is "going down".
        let a = ln_c.real;

        let (i1, i2) = if a < 0f64 {
            // There must be one intersection with lines 0/1 and 2/3
            let i1 = intersections[0].as_ref().map_or_else(|_| {
                intersections[1].as_ref().unwrap()
            }, |x| if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64) || intersections[1].is_err() {
                x
            } else {
                intersections[1].as_ref().unwrap()
            });

            let i2 = intersections[3].as_ref().map_or_else(|_| {
                intersections[2].as_ref().unwrap()
            }, |x| if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64) || intersections[2].is_err() {
                x
            } else {
                intersections[2].as_ref().unwrap()
            });

            (*i1, *i2)
        } else {
            // There must be one intersection with lines 1/3 and 0/2
            let i1 = intersections[3].as_ref().map_or_else(|_| {
                intersections[1].as_ref().unwrap()
            }, |x| if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64) || intersections[1].is_err() {
                x
            } else {
                intersections[1].as_ref().unwrap()
            });

            let i2 = intersections[0].as_ref().map_or_else(|_| {
                intersections[2].as_ref().unwrap()
            }, |x| if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64) || intersections[2].is_err() {
                x
            } else {
                intersections[2].as_ref().unwrap()
            });

            (*i1, *i2)
        };

        blueprint_lines.push(RenderedLine {
            label: ln.label.clone(),
            points: (i1, i2)
        });
    }

    Ok(blueprint_points.into_iter().map(Rendered::Point).chain(blueprint_lines.into_iter().map(Rendered::Line)).collect())
}