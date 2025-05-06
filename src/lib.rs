//! # Gerber-parser
//!
//! This is a simple parser in rust for Gerber file (or string). The underlying representation
//! is built on the `gerber-types` crate.
//!
//! Gerber files are the de-facto file format for PCB manufacturing, but are also used in other
//! context, such as in microfabrication. It is an old format with a lot of baggage, but the
//! [specification is well documented](https://www.ucamco.com/en/guest/downloads/gerber-format),
//! there is an [online free viewer available](https://gerber-viewer.ucamco.com/) to check your designs,
//! and the format is plaintext making it easy to work with.
//!
//! As the crate is still in the early version, expect significant changes over time, as both this
//! crate and `gerber-types` will need to undergo changes.

// These modules are not public, instead we re-export the public types from them below
mod error;
mod gerber_doc;
mod parser;

// Since this crate is specifically designed to parse into types exposed by `gerber_types`, we re-export it here.
pub use gerber_types;

pub use error::*;
pub use gerber_doc::*;
pub use parser::*;
