# Rust Gerber Parser
[![Build status][build-status-badge]][build-status]
[![Crates.io][crates-io-badge]][crates-io]
[![MakerPnP Discord][discord-badge]][discord]

- [Docs (released)](https://docs.rs/gerber-parser/)

This crate implements a `gerber` parser written in rust to be used with the `gerber-types-rs` crate.

## Example

See the `examples` folder of the repository, run it as follows:

    $ cargo run --example example1

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

## Related crates

### Gerber Types

A rust crate for definition of gerber types in test.

Crates.io: https://crates.io/crates/gerber-types
Github: https://github.com/MakerPnP/gerber-types

### Gerber Viewer

A rust crate for rendering gerber layers, also uses this crate as a dependency:

Github: https://github.com/MakerPnP/gerber-viewer

## Related projects

For a list of other projects that use this crate you can check the github 'dependents' page.

https://github.com/MakerPnP/gerber-parser/network/dependents


## Authors

Nemo Andrea (Original author)
Dominic Clifton (Current maintainer)

## License

[GNU AFFERO GENERAL PUBLIC LICENSE](https://www.gnu.org/licenses/agpl-3.0.en.html)

## History


<!-- Badges -->

[build-status]: https://github.com/makerpnp/gerber-parser/actions/workflows/ci.yml
[build-status-badge]: https://github.com/makerpnp/gerber-parser/workflows/CI/badge.svg
[crates-io]: https://crates.io/crates/gerber-parser
[crates-io-badge]: https://img.shields.io/crates/v/gerber-parser.svg
[discord]: https://discord.gg/ffwj5rKZuf
[discord-badge]: https://img.shields.io/discord/1255867192503832688?label=MakerPnP%20discord&color=%2332c955
