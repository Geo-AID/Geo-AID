use crate::{
    generator::{geometry, Complex, EvaluationError},
    script::figure::{Figure, LineDefinition, PointDefinition},
};

pub struct Blueprint {
    pub points: Vec<RenderedPoint>,
    pub lines: Vec<RenderedLine>,
}

pub enum Rendered {
    Point(RenderedPoint),
    Line(RenderedLine),
}

#[derive(Debug)]
pub struct RenderedPoint {
    /// The point's label
    pub label: String,
    /// Point's position
    pub position: Complex,
}

pub struct RenderedLine {
    /// The line's label
    pub label: String,
    /// The line's thickness
    /// Two ends of the line
    pub points: (Complex, Complex),
}

fn evaluate_line(
    line: &LineDefinition,
    points: &Vec<Complex>,
) -> Result<Complex, EvaluationError> {
    Ok(match line {
        LineDefinition::TwoPoints(i1, i2) => geometry::get_line(
            evaluate_point(i1, points)?,
            evaluate_point(i2, points)?,
        ),
    })
}

fn evaluate_point(
    definition: &PointDefinition,
    points: &Vec<Complex>,
) -> Result<Complex, EvaluationError> {
    Ok(match definition {
        PointDefinition::Indexed(gen_index) => points[*gen_index],
        PointDefinition::Crossing(l1, l2) => {
            let l1 = evaluate_line(l1, points)?;
            let l2 = evaluate_line(l2, points)?;

            geometry::get_crossing(l1, l2)?
        }
    })
}

pub fn project(
    figure: &Figure,
    generated_points: Vec<Complex>,
) -> Result<Vec<Rendered>, EvaluationError> {
    let points: Vec<Complex> = figure.points.iter().map(
        |pt| evaluate_point(&pt.definition, &generated_points)
    ).collect::<Result<Vec<Complex>, EvaluationError>>()?;

    let size1 = Complex::new(figure.canvas_size.0 as f64, figure.canvas_size.1 as f64);
    let size09 = size1 * 0.9;
    let size005 = size1 * 0.05;

    // Frame topleft point.
    let mut offset = points.get(0).cloned().unwrap_or_default();

    for x in points.iter() {
        if x.real < offset.real {
            offset.real = x.real
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
    let mut furthest = points.get(0).cloned().unwrap_or_default();

    for x in points.iter() {
        if x.real > furthest.real {
            furthest.real = x.real
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
        blueprint_points.push(RenderedPoint {
            label: figure.points[i].label.clone(),
            position: *pt,
        });
    }

    let mut blueprint_lines = Vec::new();

    for ln in figure.lines.iter() {
        let ln_c = evaluate_line(&ln.definition, &generated_points)?;

        // +--0--+
        // |     |
        // 1     2
        // |     |
        // +--3--+

        let intersections = [
            geometry::get_crossing(
                ln_c,
                geometry::get_line(
                    Complex::new(0.0, figure.canvas_size.1 as f64),
                    Complex::new(1.0, figure.canvas_size.1 as f64),
                ),
            ),
            geometry::get_crossing(
                ln_c,
                geometry::get_line(Complex::new(0.0, 0.0), Complex::new(0.0, 1.0)),
            ),
            geometry::get_crossing(
                ln_c,
                geometry::get_line(
                    Complex::new(figure.canvas_size.0 as f64, 0.0),
                    Complex::new(figure.canvas_size.0 as f64, 1.0),
                ),
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

        let (i1, i2) = if a < 0f64 {
            // There must be one intersection with lines 0/1 and 2/3
            let i1 = intersections[0].as_ref().map_or_else(
                |_| intersections[1].as_ref().unwrap(),
                |x| {
                    if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64)
                        || intersections[1].is_err()
                    {
                        x
                    } else {
                        intersections[1].as_ref().unwrap()
                    }
                },
            );

            let i2 = intersections[3].as_ref().map_or_else(
                |_| intersections[2].as_ref().unwrap(),
                |x| {
                    if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64)
                        || intersections[2].is_err()
                    {
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
                    if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64)
                        || intersections[1].is_err()
                    {
                        x
                    } else {
                        intersections[1].as_ref().unwrap()
                    }
                },
            );

            let i2 = intersections[0].as_ref().map_or_else(
                |_| intersections[2].as_ref().unwrap(),
                |x| {
                    if (x.real > 0f64 && x.real < figure.canvas_size.0 as f64)
                        || intersections[2].is_err()
                    {
                        x
                    } else {
                        intersections[2].as_ref().unwrap()
                    }
                },
            );

            (*i1, *i2)
        };

        // println!("i1, i2: {i1}, {i2}");

        blueprint_lines.push(RenderedLine {
            label: ln.label.clone(),
            points: (i1, i2),
        });
    }

    // println!("{:#?}", blueprint_points);

    Ok(blueprint_points
        .into_iter()
        .map(Rendered::Point)
        .chain(blueprint_lines.into_iter().map(Rendered::Line))
        .collect())
}
