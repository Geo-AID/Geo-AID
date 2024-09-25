use egui::{Color32, Context, RichText, Slider};
use egui_file::FileDialog;
use geo_aid_internal::engine::compiler::{self, Compiled, FigureFn};
use geo_aid_internal::projector;
use geo_aid_internal::projector::figure::{Item, Label, Position};
use geo_aid_internal::script::math;
use geo_aid_internal::script::math::Flags;
use geo_aid_math::{Context as MathContext, Func};
use macroquad::prelude::*;
use rand::Rng;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use ::rand;

mod egui_macroquad;
mod egui_miniquad;

struct Figure {
    flags: Arc<Flags>,
    // error_func: Func,
    gradient_func: Func,
    figure_func: FigureFn,
    current_state: Vec<f64>,
}

struct Debugger {
    dialog: FileDialog,
    file: Option<PathBuf>,
    file_valid: bool,
    figure: Option<Figure>,
    gradient_step: f64,
}

impl Debugger {
    #[must_use]
    pub fn new() -> Self {
        let dialog = FileDialog::open_file(None);

        Self {
            dialog,
            file: None,
            file_valid: true,
            figure: None,
            gradient_step: 1.0,
        }
    }

    pub fn show(&mut self, ctx: &Context) {
        egui::Window::new("Generating").show(ctx, |ui| {
            let mut quit = false;

            if let Some(figure) = &mut self.figure {
                if ui.button("Quit").clicked() {
                    quit = true;
                }

                ui.add(Slider::new(&mut self.gradient_step, 0.0..=10.0));

                if ui.button("Step").clicked() {
                    let mut gradient = figure.current_state.clone();
                    figure
                        .gradient_func
                        .call(&figure.current_state, &mut gradient);

                    for (target, delta) in figure.current_state.iter_mut().zip(gradient) {
                        *target -= delta * self.gradient_step;
                    }
                }

                if ui.button("Randomize").clicked() {
                    let mut rng = rand::thread_rng();
                    figure.current_state = (0..figure.current_state.len())
                        .map(|_| rng.gen::<f64>() * 10.0)
                        .collect();
                }
            } else {
                egui::Grid::new("file-data")
                    .num_columns(2)
                    .striped(true)
                    .show(ui, |ui| {
                        ui.label("File:");
                        if let Some(file) = &self.file {
                            ui.horizontal(|ui| {
                                ui.label(file.to_string_lossy());

                                if ui.button("Change").clicked() {
                                    self.dialog.open();
                                }
                            });
                        } else if ui.button("Open").clicked() {
                            self.dialog.open();
                        }
                        ui.end_row();

                        if !self.file_valid {
                            ui.label(RichText::new("Invalid file").color(Color32::RED));
                            ui.end_row();
                        }

                        ui.label("");
                        if ui.button("Generate").clicked() {
                            let file = self
                                .file
                                .as_ref()
                                .and_then(|file| fs::read_to_string(file).ok())
                                .and_then(|file| math::load_script(&file).ok());

                            self.file_valid = file.is_some();

                            if let Some(file) = file {
                                let Compiled {
                                    mut context,
                                    errors,
                                    figure_fn,
                                    input_count,
                                    ..
                                } = compiler::compile(&file);

                                #[allow(clippy::cast_precision_loss)]
                                let errors_len = context.constant(errors.len() as f64);
                                let mean_exponent = 2.0;
                                let total_error_power =
                                    errors.into_iter().fold(MathContext::zero(), |a, b| {
                                        let b_strict = context.pow(b, mean_exponent);
                                        let b_divided = context.div(b_strict, errors_len);
                                        context.add(a, b_divided)
                                    });
                                let total_error =
                                    context.pow(total_error_power, mean_exponent.recip());

                                let mut rng = rand::thread_rng();

                                let flags = Arc::new(file.flags.clone());

                                self.figure = Some(Figure {
                                    flags,
                                    // error_func: context.compute([total_error]),
                                    gradient_func: context.compute_gradient(total_error),
                                    figure_func: figure_fn,
                                    current_state: (0..input_count)
                                        .map(|_| rng.gen::<f64>() * 10.0)
                                        .collect(),
                                })
                            }
                        }
                        ui.end_row();
                    });
            }

            if quit {
                self.figure = None;
            }
        });

        if self.dialog.show(ctx).selected() {
            if let Some(path) = self.dialog.path() {
                self.file = Some(path.to_path_buf());
            }
        }
    }
}

fn window_conf() -> Conf {
    Conf {
        window_resizable: true,
        window_title: String::from("Geo-AID Debugger"),
        ..Conf::default()
    }
}

fn draw_label(label: &Option<Label>, color: Color) {
    if let Some(label) = label {
        draw_text(
            &label.content.to_string(),
            label.position.x as f32,
            label.position.y as f32,
            18.0,
            color,
        );
    }
}

fn draw_points(points: &(Position, Position), color: Color) {
    draw_line(
        points.0.x as f32,
        points.0.y as f32,
        points.1.x as f32,
        points.1.y as f32,
        1.0,
        color,
    );
}

fn draw_figure(figure: &projector::figure::Figure, color: Color) {
    for item in &figure.items {
        match item {
            Item::Point(pt) => {
                if pt.display_dot {
                    draw_circle(pt.position.x as f32, pt.position.y as f32, 2.0, color);
                }
                draw_label(&pt.label, color);
            }
            Item::Line(ln) => {
                draw_points(&ln.points, color);
                draw_label(&ln.label, color);
            }
            Item::Segment(x) | Item::Ray(x) => {
                draw_points(&x.points, color);
                draw_label(&x.label, color);
            }
            Item::Circle(circle) => {
                draw_circle_lines(
                    circle.center.x as f32,
                    circle.center.y as f32,
                    circle.radius as f32,
                    1.0,
                    color,
                );
                draw_label(&circle.label, color);
            }
        }
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    let mut debugger = Debugger::new();

    loop {
        clear_background(WHITE);

        if let Some(fig) = &debugger.figure {
            let figure = projector::project(
                (fig.figure_func)(&fig.current_state),
                &fig.flags,
                (screen_width() as f64 - 300.0, screen_height() as f64),
            );

            draw_figure(&figure, BLACK);

            let mut gradient = fig.current_state.clone();
            fig.gradient_func.call(&fig.current_state, &mut gradient);
            let mut state = fig.current_state.clone();

            for (target, delta) in state.iter_mut().zip(gradient) {
                *target -= delta * debugger.gradient_step;
            }

            let figure = projector::project(
                (fig.figure_func)(&state),
                &fig.flags,
                (screen_width() as f64 - 300.0, screen_height() as f64),
            );

            draw_figure(&figure, GREEN);
        }

        egui_macroquad::ui(|ctx| {
            debugger.show(ctx);
        });

        egui_macroquad::draw();

        next_frame().await;
    }
}
