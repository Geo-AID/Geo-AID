//! Geo-AID is capable of outputting figures in JSON, according to the schema at top level
//! in the repo. This is the format writer responsible for this.

use std::io::{self, Seek, Write};

use geo_aid_figure::Figure;

/// The JSON format writer.
#[derive(Default)]
pub struct Json;

impl Json {
    /// Output the figure as JSON.
    ///
    /// # Panics
    /// Panicking is a bug.
    pub fn draw(figure: &Figure, writer: impl Write + Seek) -> io::Result<()> {
        serde_json::to_writer(writer, figure).map_err(io::Error::from)
    }
}
