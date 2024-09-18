#![warn(clippy::pedantic)]

use clap::{Parser, ValueEnum};
use crossterm::{cursor, terminal, ExecutableCommand, QueueableCommand};
use geo_aid_internal::engine::glide::Glide;
use geo_aid_internal::engine::rage::GenParams;
use geo_aid_internal::engine::{glide, rage};
use geo_aid_internal::format::json::Json;
use geo_aid_internal::format::latex::Latex;
use geo_aid_internal::format::raw::Raw;
use geo_aid_internal::format::svg::Svg;
use geo_aid_internal::projector;
use geo_aid_internal::script::figure::Generated;
use geo_aid_internal::{
    engine::rage::Rage,
    format::Draw,
    script::cli::{Diagnostic, DiagnosticKind},
    script::math,
};
use std::time::Duration;
use std::{
    fs::{self, File},
    io::{self, Write},
    path::PathBuf,
    process,
    sync::Arc,
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
    /// The engine to use for figure generation.
    #[arg(long, short, default_value_t = Engine::Glide, value_enum)]
    engine: Engine,
    /// The maximum mean quality delta.
    #[arg(long, short, default_value_t = 0.0001)]
    delta_max_mean: f64,
    /// The count of workers (threads) to use for generation.
    #[arg(long, short, default_value_t = 32)]
    worker_count: usize,
    /// The count of last deltas to include in mean calculation.
    #[arg(long, short, default_value_t = 128)]
    mean_count: usize,
    /// Maximal adjustment of a point during generation. Only works with `--engine rage`.
    #[arg(long, short, default_value_t = 0.5)]
    adjustment_max: f64,
    /// Amount of samples to test during generation.
    #[arg(long, default_value_t = 512)]
    samples: usize,
    /// How strict are the rules. Negative values make the engine less strict. Zero is not allowed.
    #[arg(long, short, default_value_t = 2.0)]
    strictness: f64,
    /// Target format.
    #[arg(long, short, default_value_t = Format::Svg, value_enum)]
    format: Format,
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
enum Engine {
    /// Random Adjustment Generation Engine
    Rage,
    /// Gradient-Led Iterative Descent Engine
    Glide,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Format {
    /// LaTeX + tikz + tikz-euclide.
    Latex,
    /// Simple SVG
    Svg,
    /// JSON (machine-readable) format according to the Schema included in the repository.
    Json,
    /// Plain text (human-readable) format.
    Raw,
}

struct GenerationResult {
    generated: Generated,
    total_quality: f64,
    time: Duration,
}

#[allow(clippy::too_many_lines)]
fn main() {
    let args = Args::parse();

    // Has to be tested.
    if args.strictness == 0.0 {
        println!("Strictness must not be 0.");
        return;
    }

    if let Some(path) = args.markdown_help {
        fs::write(path, clap_markdown::help_markdown::<Args>()).unwrap();
    }

    let Ok(script) = fs::read_to_string(&args.input) else {
        println!("Failed to read file. Does it exist?");
        return;
    };
    let canvas_size = (args.width, args.height);

    let intermediate = match math::load_script(&script) {
        Ok(v) => v,
        Err(errors) => {
            for err in errors {
                let data = err.diagnostic();
                let diagnostic = Diagnostic::new(DiagnosticKind::Error, data, &args.input, &script);

                println!("{diagnostic}");
            }

            if let Some(path) = &args.log {
                let mut log = File::create(path)
                    .unwrap_or_else(|_| panic!("Failed to create log file at {}", path.display()));

                log.write_all("-1".as_bytes())
                    .expect("Writing to log file failed.");
            }

            process::exit(0);
        }
    };

    // println!("{intermediate:#?}");

    let GenerationResult {
        generated,
        total_quality,
        time,
    } = match args.engine {
        Engine::Rage => {
            let mut rage = Rage::new(
                rage::Params {
                    strictness: args.strictness,
                    samples: args.samples,
                    worker_count: args.worker_count,
                },
                &intermediate,
            );

            let mut stdout = io::stdout();

            stdout.execute(cursor::Hide).unwrap();

            let time = rage.generate_mean_delta(GenParams {
                max_adjustment: args.adjustment_max,
                mean_count: args.mean_count,
                delta_max_mean: args.delta_max_mean,
                progress_update: Box::new(|quality| {
                    let mut stdout = io::stdout();
                    stdout
                        .queue(terminal::Clear(terminal::ClearType::FromCursorDown))
                        .unwrap();

                    stdout.queue(cursor::SavePosition).unwrap();
                    stdout
                        .write_all(format!("Quality: {:.2}% ", quality * 100.0).as_bytes())
                        .unwrap();
                    stdout.queue(cursor::RestorePosition).unwrap();
                    stdout.flush().unwrap();
                }),
            });

            GenerationResult {
                time,
                total_quality: rage.gen().get_total_quality(),
                generated: rage.get_figure(),
            }
        }
        Engine::Glide => {
            let mut glide = Glide::new(
                glide::Params {
                    strictness: args.strictness,
                    samples: args.samples,
                    worker_count: args.worker_count,
                    mean_count: args.mean_count,
                    max_mean_delta: args.delta_max_mean,
                },
                &intermediate,
            );

            let mut samples = 0;
            let time = glide.generate(|| {
                samples += 1;
                let mut stdout = io::stdout();
                stdout
                    .queue(terminal::Clear(terminal::ClearType::FromCursorDown))
                    .unwrap();

                stdout.queue(cursor::SavePosition).unwrap();
                stdout
                    .write_all(format!("{}/{} samples ", samples, args.samples).as_bytes())
                    .unwrap();
                stdout.queue(cursor::RestorePosition).unwrap();
                stdout.flush().unwrap();
            });

            io::stdout().execute(cursor::Show).unwrap();

            GenerationResult {
                time,
                generated: glide.get_figure(),
                total_quality: glide.get_total_quality(),
            }
        }
    };

    let flags = Arc::new(intermediate.flags);
    let rendered = projector::project(generated, &flags, canvas_size);

    match args.format {
        Format::Latex => {
            Latex::default().draw(&args.output, &rendered).unwrap();
        }
        Format::Json => {
            Json.draw(&args.output, &rendered).unwrap();
        }
        Format::Svg => {
            Svg::default().draw(&args.output, &rendered).unwrap();
        }
        Format::Raw => {
            Raw::default().draw(&args.output, &rendered).unwrap();
        }
    }

    println!(
        "Finished rendering with total quality {:.2}% in {:.2} seconds.",
        total_quality * 100.0,
        time.as_secs_f64()
    );

    if let Some(path) = &args.log {
        let mut log = File::create(path)
            .unwrap_or_else(|_| panic!("Failed to create log file at {}", path.display()));

        let full = format!("0\n{}\n{}", total_quality, time.as_secs_f64());
        log.write_all(full.as_bytes())
            .expect("Writing to log file failed.");
    }
}
