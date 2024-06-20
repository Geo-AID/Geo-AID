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
