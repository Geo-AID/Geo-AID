//! Geo-AID is capable of outputting figures in JSON, according to the schema at top level
//! in the repo. This is the format writer responsible for this.

use geo_aid_figure::Figure;

/// The JSON format writer.
#[derive(Default)]
pub struct Json;

impl Json {
    /// Output the figure as JSON.
    ///
    /// # Panics
    /// Panicking is a bug.
    pub fn draw(figure: &Figure) -> String {
        serde_json::to_string(figure).unwrap()
    }
}
