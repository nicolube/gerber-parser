# Rust Gerber Parser
[![Build status][build-status-badge]][build-status]
[![Crates.io][crates-io-badge]][crates-io]
[![MakerPnP Discord][discord-badge]][discord]

- [Docs (released)](https://docs.rs/gerber-types/)

This crate implements a `gerber` parser written in rust to be used with the `gerber-types-rs` crate.

## Example

See the `examples` folder of the repository.

## Spec compliance

The gerber specification 2024.05 is the latest version of the Gerber file format that is used for reference.

The original author used f360, the initial implementation was written to support files it generated.
Recently support has been added for the files that DipTrace, KiCad, etc.

⚠️ Note: this package is still in development and does not cover the full Gerber spec

Currently missing:

* `LM`, `LR`, `LS`, `IP` commands (note: these are deprecated in the spec)
* `AB` commands

Partial:

* The `TF` and `TA` commands only support a limited range of arguments; custom attributes will result in an error

## Authors

Nemo Andrea (Original author)
Dominic Clifton (Current maintainer)

## License

[GNU AFFERO GENERAL PUBLIC LICENSE](https://www.gnu.org/licenses/agpl-3.0.en.html)

## History


<!-- Badges -->

[build-status]: https://github.com/makerpnp/gerber-parser/actions/workflows/ci.yml
[build-status-badge]: https://github.com/makerpnp/gerber-parser/workflows/CI/badge.svg
[crates-io]: https://crates.io/crates/gerber_parser
[crates-io-badge]: https://img.shields.io/crates/v/gerber_parser.svg
[discord]: https://discord.gg/ffwj5rKZuf
[discord-badge]: https://img.shields.io/discord/1255867192503832688?label=MakerPnP%20discord&color=%2332c955
