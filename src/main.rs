use std::{sync::Arc, env, fs, path::PathBuf, str::FromStr};

use geo_aid::{
    drawer::latex,
    generator::Generator,
    projector,
    script::compile,
};

fn main() {
    let script_path: Vec<String> = env::args().collect();

    let script = fs::read_to_string(PathBuf::from_str(script_path.get(1).unwrap()).unwrap()).unwrap();

    let (criteria, figure, point_count) = compile::compile(script, (500, 500)).unwrap();

    // println!("{:#?}", figure);

    let mut gen = Generator::new(
        point_count,
        128,
        Arc::new(criteria)
    );

    // for rule in unroll::unroll(script.to_string()).unwrap().0 {
    //     println!("{rule}");
    // }

    // let mut gen = Generator::new(
    //     4,
    //     128,
    //     Arc::new(vec![
    //         Weighed {
    //             weight: 1.0,
    //             object: CriteriaKind::Greater(
    //                 Arc::new(Weighed {
    //                     weight: 1.0,
    //                     object: Expression::PointLineDistance(
    //                         Arc::new(Weighed {
    //                             weight: 1.0,
    //                             object: Expression::FreePoint(2),
    //                         }),
    //                         Arc::new(Weighed {
    //                             weight: 1.0,
    //                             object: Expression::Line(
    //                                 Arc::new(Weighed {
    //                                     weight: 1.0,
    //                                     object: Expression::FreePoint(0),
    //                                 }),
    //                                 Arc::new(Weighed {
    //                                     weight: 1.0,
    //                                     object: Expression::FreePoint(1),
    //                                 }),
    //                             ),
    //                         }),
    //                     ),
    //                 }),
    //                 Arc::new(Weighed {
    //                     weight: 1.0,
    //                     object: Expression::Literal(0.01, ComplexUnit::new(SimpleUnit::Distance)),
    //                 }),
    //             ),
    //         },
    //         Weighed {
    //             weight: 1.0,
    //             object: CriteriaKind::Less(
    //                 Arc::new(Weighed {
    //                     weight: 1.0,
    //                     object: Expression::PointLineDistance(
    //                         Arc::new(Weighed {
    //                             weight: 1.0,
    //                             object: Expression::FreePoint(3),
    //                         }),
    //                         Arc::new(Weighed {
    //                             weight: 1.0,
    //                             object: Expression::Line(
    //                                 Arc::new(Weighed {
    //                                     weight: 1.0,
    //                                     object: Expression::FreePoint(0),
    //                                 }),
    //                                 Arc::new(Weighed {
    //                                     weight: 1.0,
    //                                     object: Expression::FreePoint(1),
    //                                 }),
    //                             ),
    //                         }),
    //                     ),
    //                 }),
    //                 Arc::new(Weighed {
    //                     weight: 1.0,
    //                     object: Expression::Literal(0.01, ComplexUnit::new(SimpleUnit::Distance)),
    //                 }),
    //             ),
    //         },
    //     ]),
    // );

    // let fig = Figure {
    //     points: vec![
    //         Point {
    //             label: String::from("A"),
    //             definition: PointDefinition::Indexed(0),
    //         },
    //         Point {
    //             label: String::from("B"),
    //             definition: PointDefinition::Indexed(1),
    //         },
    //         Point {
    //             label: String::from("C"),
    //             definition: PointDefinition::Indexed(2),
    //         },
    //         Point {
    //             label: String::from("D"),
    //             definition: PointDefinition::Indexed(3),
    //         },
    //     ],
    //     lines: vec![Line {
    //         label: String::from("a"),
    //         definition: LineDefinition::TwoPoints(
    //             Box::new(PointDefinition::Indexed(0)),
    //             Box::new(PointDefinition::Indexed(1))
    //         ),
    //     }],
    //     segments: vec![],
    //     canvas_size: (300, 300),
    // };

    gen.cycle_until_mean_delta(0.5, 20, 0.001);
    // println!("{:#?}", gen.get_points());
    let rendered =
        projector::project(&figure, gen.get_points().iter().map(|x| x.0).collect()).unwrap();
    latex::draw("debug-output/output.svg".to_string(), (500, 500), rendered);
    
    // for i in 1..=200 {
    //     gen.single_cycle(0.5);
    //     let rendered = projector::project(&fig, gen.get_points().iter().map(|x| x.0).collect()).unwrap();
    //     svg::draw(format!("debug-output/gen{i}.svg"), (300, 300), rendered);
    // }

    println!(
        "Finished rendering with total quality {}%.",
        gen.get_total_quality() * 100.0
    );
}
