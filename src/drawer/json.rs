use std::{fs::File, io::Write, path::PathBuf};

use serde::Serialize;

use crate::projector::Rendered;

#[derive(Serialize)]
struct Data<'r> {
    pub width: usize,
    pub height: usize,
    pub rendered: &'r Vec<Rendered>,
}

/// # Panics
/// Panics whenever there is a filesystem problem
pub fn draw(target: &PathBuf, canvas_size: (usize, usize), rendered: &Vec<Rendered>) {
    let mut content = String::new();
    let data = Data {
        width: canvas_size.0,
        height: canvas_size.1,
        rendered,
    };
    content += (serde_json::to_string(&data).unwrap()).as_str();

    let mut file = File::create(target).unwrap();
    file.write_all(content.as_bytes()).unwrap();
}
