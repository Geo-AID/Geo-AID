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
    generator::GenerationArgs,
};
use geo_aid::{
    drawer::{latex, svg},
    generator::Generator,
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
    #[arg(long, short, default_value_t = Renderer::Svg, value_enum)]
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
    #[arg(long, hide = true)]
    markdown_help: Option<PathBuf>,
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

    if let Some(path) = args.markdown_help {
        fs::write(path, clap_markdown::help_markdown::<Args>()).unwrap();
    }

    let script = fs::read_to_string(&args.input).expect("Failed to read file.");
    let canvas_size = (args.width, args.height);

    let result = compile::compile(&script, canvas_size);

    let (criteria, figure, template, flags) = match result {
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

    let flags = Arc::new(flags);
    let mut gen = Generator::new(
        &template,
        args.count_of_workers,
        &GenerationArgs {
            criteria: Arc::new(criteria),
            point_count: template.len(),
        },
        &flags,
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

    let rendered = projector::project(&figure, gen.get_state(), &flags);

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
