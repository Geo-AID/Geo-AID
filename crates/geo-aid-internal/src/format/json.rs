//! Geo-AID is capable of outputting figures in JSON, according to the schema at top level
//! in the repo. This is the format writer responsible for this.

use std::{fs::File, io::Write, path::Path};

use geo_aid_figure::Figure;

/// The JSON format writer.
#[derive(Default)]
pub struct Json;

impl Json {
    /// Output the figure as JSON.
    ///
    /// # Errors
    /// Returns an error when there was a problem with creating the output file.
    /// # Panics
    /// Possibly panics during serialization.
    pub fn draw(&mut self, target: &Path, figure: &Figure) -> Result<(), std::io::Error> {
        let json = serde_json::to_string(figure).unwrap();

        let file = File::create(target);

        match file {
            Ok(mut file) => file.write_all(json.as_bytes()),
            Err(error) => Err(error),
        }
    }
}
