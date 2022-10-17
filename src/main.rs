use std::sync::Arc;

use geo_aid::{generator::Generator, script::{Weighed, CriteriaKind, Expression, geometry::Point, ComplexUnit, SimpleUnit}};

fn main() {
    let points = vec![
        Arc::new(Point {
            position: None,
            index: 0,
            name: String::from("A")
        }),
        Arc::new(Point {
            position: None,
            index: 1,
            name: String::from("B")
        }),
        Arc::new(Point {
            position: None,
            index: 2,
            name: String::from("C")
        }),
        Arc::new(Point {
            position: None,
            index: 3,
            name: String::from("D")
        })
    ];

    let mut gen = Generator::new(4, 5, Arc::new(
        vec![
            Weighed {
                weight: 1.0,
                object: CriteriaKind::Greater(
                    Box::new(Weighed {
                        weight: 1.0,
                        object: Expression::PointLineDistance(Box::new(Weighed {
                            weight: 1.0,
                            object: Expression::Point(Arc::clone(&points[2]))
                        }), Box::new(Weighed {
                            weight: 1.0,
                            object: Expression::Line(
                                Box::new(Weighed{weight:1.0,object:Expression::Point(Arc::clone(&points[0]))}),
                                Box::new(Weighed{weight:1.0,object:Expression::Point(Arc::clone(&points[1]))}),
                            )
                        }))
                    }),
                    Box::new(Weighed {
                        weight: 1.0,
                        object: Expression::Literal(0.01, ComplexUnit::new(SimpleUnit::Distance))
                    })
                )
            },
            Weighed {
                weight: 1.0,
                object: CriteriaKind::Less(
                    Box::new(Weighed {
                        weight: 1.0,
                        object: Expression::PointLineDistance(Box::new(Weighed {
                            weight: 1.0,
                            object: Expression::Point(Arc::clone(&points[3]))
                        }), Box::new(Weighed {
                            weight: 1.0,
                            object: Expression::Line(
                                Box::new(Weighed{weight:1.0,object:Expression::Point(Arc::clone(&points[0]))}),
                                Box::new(Weighed{weight:1.0,object:Expression::Point(Arc::clone(&points[1]))}),
                            )
                        }))
                    }),
                    Box::new(Weighed {
                        weight: 1.0,
                        object: Expression::Literal(0.01, ComplexUnit::new(SimpleUnit::Distance))
                    })
                )
            }
        ]
    ));

    gen.cycle_until_mean_delta(0.5, 4, 0.01);

    println!("{:#?}, q: {}", gen.get_points(), gen.get_total_quality());
}
