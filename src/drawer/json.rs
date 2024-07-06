use std::{fs::File, io::Write, path::Path};

use serde::Serialize;

use crate::projector::{Output, Rendered};

#[derive(Default)]
pub struct Json {
    pub content: String,
}

/// Data to be serialized.
#[derive(Serialize)]
pub struct Data<'r> {
    pub width: usize,
    pub height: usize,
    pub rendered: &'r Vec<Rendered>,
}

impl Json {
    /// # Errors
    /// Returns an error when there was a problem with creating the output file.
    /// # Panics
    /// Possibly panics during serialization.
    pub fn draw(&mut self, target: &Path, output: &Output) -> Result<(), std::io::Error> {
        let data = Data {
            width: output.canvas_size.0,
            height: output.canvas_size.1,
            rendered: &output.rendered,
        };

        self.content += serde_json::to_string(&data).unwrap().as_str();

        let file = File::create(target);
        
        match file {
            Ok(mut file) => file.write_all(self.content.as_bytes()),
            Err(error) => Err(error),
        }
    }
}
