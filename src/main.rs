use std::sync::Arc;

use geo_aid::{generator::Generator, script::{Weighed, CriteriaKind, Expression, ComplexUnit, SimpleUnit, figure::{Figure, Point, PointDefinition, Line, LineDefinition}}, projector, drawer::svg};

fn main() {
    // let points = vec![
    //     Arc::new(Point {
    //         position: None,
    //         index: 0,
    //         name: String::from("A")
    //     }),
    //     Arc::new(Point {
    //         position: None,
    //         index: 1,
    //         name: String::from("B")
    //     }),
    //     Arc::new(Point {
    //         position: None,
    //         index: 2,
    //         name: String::from("C")
    //     }),
    //     Arc::new(Point {
    //         position: None,
    //         index: 3,
    //         name: String::from("D")
    //     })
    // ];

    let mut gen = Generator::new(4, 128, Arc::new(
        vec![
            Weighed {
                weight: 1.0,
                object: CriteriaKind::Greater(
                    Box::new(Weighed {
                        weight: 1.0,
                        object: Expression::PointLineDistance(Box::new(Weighed {
                            weight: 1.0,
                            object: Expression::FreePoint(2)
                        }), Box::new(Weighed {
                            weight: 1.0,
                            object: Expression::Line(
                                Box::new(Weighed{weight:1.0,object:Expression::FreePoint(0)}),
                                Box::new(Weighed{weight:1.0,object:Expression::FreePoint(1)}),
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
                            object: Expression::FreePoint(3)
                        }), Box::new(Weighed {
                            weight: 1.0,
                            object: Expression::Line(
                                Box::new(Weighed{weight:1.0,object:Expression::FreePoint(0)}),
                                Box::new(Weighed{weight:1.0,object:Expression::FreePoint(1)}),
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

    let fig = Figure {
        points: vec![
            Point {
                label: String::from("A"),
                definition: PointDefinition::Indexed(0)
            },
            Point {
                label: String::from("B"),
                definition: PointDefinition::Indexed(1)
            },
            Point {
                label: String::from("C"),
                definition: PointDefinition::Indexed(2)
            },
            Point {
                label: String::from("D"),
                definition: PointDefinition::Indexed(3)
            }
        ],
        lines: vec![
            Line {
                label: String::from("a"),
                definition: LineDefinition::TwoPoints(0, 1)
            }
        ],
        segments: vec![],
        canvas_size: (300, 300),
    };

    gen.cycle_until_mean_delta(0.5, 4, 0.001);
    let rendered = projector::project(&fig, gen.get_points().iter().map(|x| x.0).collect()).unwrap();
    svg::draw("debug-output/output.svg".to_string(), (300, 300), rendered);
    
    
    // for i in 1..=200 {
    //     gen.single_cycle(0.5);
    //     let rendered = projector::project(&fig, gen.get_points().iter().map(|x| x.0).collect()).unwrap();
    //     svg::draw(format!("debug-output/gen{i}.svg"), (300, 300), rendered);
    // }

    println!("Finished rendering with total quality {}%.", gen.get_total_quality() * 100.0);
}
