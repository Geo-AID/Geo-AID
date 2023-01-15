#![warn(clippy::pedantic)]

use std::{
    fs::{self, File},
    io::{self, Write},
    path::PathBuf,
    process,
    sync::Arc,
};

use clap::{Parser, ValueEnum};
use crossterm::{cursor, terminal, ExecutableCommand, QueueableCommand};
use geo_aid::{
    cli::{Diagnostic, DiagnosticKind},
    drawer::{json, raw},
};
use geo_aid::{
    drawer::{latex, svg},
    generator::{Complex, Generator},
    projector,
    script::compile,
};

#[derive(Debug, Parser)]
#[command(name = "Geo-AID")]
#[command(version)]
#[command(author)]
#[command(about = "Tool for generating and rendering geometrical figures.", long_about = None)]
struct Args {
    /// The input script file.
    input: PathBuf,
    /// The output target.
    output: PathBuf,
    /// The maximum mean quality delta.
    #[arg(long, short, default_value_t = 0.0001)]
    delta_max_mean: f64,
    /// The count of workers to use for generation.
    #[arg(long, short, default_value_t = 512)]
    count_of_workers: usize,
    /// The count of last deltas to include in mean calculation.
    #[arg(long, short, default_value_t = 128)]
    mean_count: usize,
    /// Maximal adjustment of a point during generation.
    #[arg(long, short, default_value_t = 0.5)]
    adjustment_max: f64,
    /// Renderer to use.
    #[arg(long, short, default_value_t = Renderer::Latex, value_enum)]
    renderer: Renderer,
    /// Canvas width
    #[arg(long, default_value_t = 500)]
    width: usize,
    /// Canvas height
    #[arg(long, default_value_t = 500)]
    height: usize,
    /// Where to put the log output
    #[arg(long, short)]
    log: Option<PathBuf>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Renderer {
    /// The LaTeX + tikz renderer.
    Latex,
    /// The SVG format renderer.
    Svg,
    /// The JSON (machine-readable) format renderer.
    Json,
    /// The raw (human-readable) format renderer.
    Raw,
}

fn main() {
    let args = Args::parse();
    let script = fs::read_to_string(&args.input).expect("Failed to read file.");
    let canvas_size = (args.width, args.height);

    let result = compile::compile(&script, canvas_size);

    let (criteria, figure, point_count, flags) = match result {
        Ok(v) => v,
        Err(err) => {
            let data = err.diagnostic();
            let diagnostic = Diagnostic::new(DiagnosticKind::Error, data, &args.input, &script);

            println!("{diagnostic}");

            if let Some(path) = &args.log {
                let mut log = File::create(path)
                    .unwrap_or_else(|_| panic!("Failed to create log file at {}", path.display()));

                log.write_all("-1".as_bytes())
                    .expect("Writing to log file failed.");
            }

            process::exit(0);
        }
    };

    // println!("{criteria:#?}");

    let mut gen = Generator::new(
        point_count,
        args.count_of_workers,
        &Arc::new(criteria),
        &Arc::new(flags),
    );

    let mut stdout = io::stdout();

    stdout.execute(cursor::Hide).unwrap();

    let duration = gen.cycle_until_mean_delta(
        args.adjustment_max,
        args.mean_count,
        args.delta_max_mean,
        |quality| {
            stdout
                .queue(terminal::Clear(terminal::ClearType::FromCursorDown))
                .unwrap();

            stdout.queue(cursor::SavePosition).unwrap();
            stdout
                .write_all(format!("Quality: {:.2}% ", quality * 100.0).as_bytes())
                .unwrap();
            stdout.queue(cursor::RestorePosition).unwrap();
            stdout.flush().unwrap();
        },
    );

    stdout.execute(cursor::Show).unwrap();

    let rendered = projector::project(
        &figure,
        &gen.get_points()
            .iter()
            .map(|x| x.0)
            .collect::<Vec<Complex>>(),
    )
    .unwrap();

    match args.renderer {
        Renderer::Latex => latex::draw(&args.output, canvas_size, &rendered),
        Renderer::Svg => svg::draw(&args.output, canvas_size, &rendered),
        Renderer::Json => json::draw(&args.output, canvas_size, &rendered),
        Renderer::Raw => raw::draw(&args.output, canvas_size, &rendered),
    }

    // for i in 1..=200 {
    //     gen.single_cycle(0.5);
    //     let rendered = projector::project(&fig, gen.get_points().iter().map(|x| x.0).collect()).unwrap();
    //     svg::draw(format!("debug-output/gen{i}.svg"), (300, 300), rendered);
    // }

    println!(
        "Finished rendering with total quality {:.2}% in {:.2} seconds.",
        gen.get_total_quality() * 100.0,
        duration.as_secs_f64()
    );

    if let Some(path) = &args.log {
        let mut log = File::create(path)
            .unwrap_or_else(|_| panic!("Failed to create log file at {}", path.display()));

        let full = format!("0\n{}\n{}", gen.get_total_quality(), duration.as_secs_f64());
        log.write_all(full.as_bytes())
            .expect("Writing to log file failed.");
    }
}
