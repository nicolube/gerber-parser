//! # Gerber-parser
//!
//! This is a parser in rust for Gerber file. The underlying representation is built on the `gerber-types` crate.
//!
//! Gerber files are the de-facto file format for PCB manufacturing, but are also used in other
//! context, such as in microfabrication. It is an old format with a lot of baggage, but the
//! [specification is well documented](https://www.ucamco.com/en/guest/downloads/gerber-format),
//! there is an [online free viewer available](https://gerber-viewer.ucamco.com/) to check your designs,
//! and the format is plaintext making it easy to work with.
//!
//! There are sibling crates to this crate, as follows:
//!
//! * gerber-types - underlying types for the parser - <https://github.com/MakerPnP/gerber-types>
//! * gerber-viewer - a pure rust gerber viewer - <https://github.com/MakerPnP/gerber-viewer>
//!
//! This crate has matured, the API is simple and should be fairly stable now.  This crate is the primary driver to
//! changes for the gerber-types crate.

#[macro_use]
mod macros;

// Public modules
pub mod util;

// Private modules - we re-export the public types from them below
mod document;
mod error;
mod parser;

// Since this crate is specifically designed to parse into types exposed by `gerber_types`, we re-export it here.
pub use gerber_types;

pub use document::*;
pub use error::*;
pub use parser::*;
