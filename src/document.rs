use crate::error::GerberParserErrorWithContext;
use crate::gerber_types::{Aperture, Command, CoordinateFormat, Unit};
use ::std::collections::HashMap;
use std::fmt;

/// Representation of a Gerber document
#[derive(Default, Debug)]
pub struct GerberDoc {
    /// unit type, defined once per document
    pub units: Option<Unit>,
    /// format specification for coordinates, defined once per document
    pub format_specification: Option<CoordinateFormat>,
    /// map of apertures which can be used in draw commands later on in the document.
    pub apertures: HashMap<i32, Aperture>,
    /// Everything else - draw commands, comments, attributes, etc.
    pub commands: Vec<Result<Command, GerberParserErrorWithContext>>,
    /// Image Name, 8.1.3. Deprecated, but still used by fusion 360.
    pub image_name: Option<String>,
}

impl GerberDoc {
    #[deprecated(since = "0.2.0", note = "Use default() instead")]
    pub fn new() -> GerberDoc {
        Self::default()
    }

    /// Convert Self into a representation of a gerber document *purely* in terms of elements provided
    /// in the gerber-types rust crate.
    ///
    /// This will ignore any errors encountered during parsing, to access errors, use `get_errors` first.
    pub fn into_commands(self) -> Vec<Command> {
        self.commands
            .into_iter()
            .filter_map(|element| element.ok())
            .collect()
    }

    /// Get a representation of a gerber document *purely* in terms of elements provided
    /// in the gerber-types rust crate.
    ///
    /// Similar to `into_commands()`, but does not consume the Self, and returns references to Commands
    ///
    /// This will ignore any errors encountered during parsing, to access errors, use `get_errors`.
    pub fn as_commands(&self) -> Vec<&Command> {
        self.commands
            .iter()
            .filter_map(|element| element.as_ref().ok())
            .collect()
    }

    pub fn get_errors(&self) -> Vec<&GerberParserErrorWithContext> {
        let mut error_vec: Vec<&GerberParserErrorWithContext> = Vec::new();
        for command in &self.commands {
            if let Err(error) = command {
                error_vec.push(error);
            }
        }
        error_vec
    }
}

impl fmt::Display for GerberDoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "GerberDoc")?;
        writeln!(f, "- units: {:?}", self.units)?;
        match self.format_specification {
            None => {
                writeln!(f, "- no format spec!")?;
            }
            Some(format_spec) => {
                let int_str: String = "_".repeat(format_spec.integer as usize);
                let dec_str: String = "_".repeat(format_spec.decimal as usize);
                writeln!(
                    f,
                    "- format spec: {}.{} ({}|{})",
                    int_str, dec_str, format_spec.integer, format_spec.decimal
                )?;
            }
        }

        writeln!(f, "- apertures: ")?;
        for code in self.apertures.keys() {
            writeln!(f, "\t {}", code)?;
        }
        write!(f, "- commands: {}", &self.commands.len())
    }
}
