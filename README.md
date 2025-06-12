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

### Supported gerber features

| Supported | Feature                          | Notes                                          |
|----------|----------------------------------|------------------------------------------------|
| ✅        | Comments (G04)                   |                                                |
| ✅        | Units (MO)                       |                                                |
| ✅        | Format specification (FS)        |                                                |
| ✅        | Aperture definition (AD)         |                                                |
| ✅        | Standard aperture templates      |                                                |
| ✅        | Aperture macros (AM)             |                                                |
| ✅        | Select aperture (Dnn)            |                                                |
| ✅        | Plot state (G01, G02, G03, G75)  |                                                |
| ✅        | Operations (D01, D02, D03)       |                                                |
| ❌        | Transformations (LP, LM, LR, LS) | `LM`, `LR`, `LS` missing                       |
| ✅        | Regions (G36/G37)                |                                                |
| ✅        | Block aperture (AB)              |                                                |
| ✅        | Step repeat (SR)                 |                                                |
| ✅        | End-of-file (M02)                |                                                |
| ✅        | File attributes (TF)             |                                                |
| ✅        | Aperture attributes (TA)         |                                                |
| ✅        | Object attributes (TO)           |                                                |
| ✅        | Delete attribute (TD)            |                                                |
| ✅        | Standard attributes              | Full support, including .N, .P, .C, .CXxxx, etc |
| ✅        | User defined attributes          |                                                |
| ❌        | Comment attributes               | See spec 2024.5 - 5.1.1, 'G04 #@! ...*         |
| ✅         | Legacy/deprecated attributes    | Partial                                        |

Contributions to improve support welcomed!

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

<!-- Badges -->

[build-status]: https://github.com/makerpnp/gerber-parser/actions/workflows/ci.yml
[build-status-badge]: https://github.com/makerpnp/gerber-parser/workflows/CI/badge.svg
[crates-io]: https://crates.io/crates/gerber-parser
[crates-io-badge]: https://img.shields.io/crates/v/gerber-parser.svg
[discord]: https://discord.gg/ffwj5rKZuf
[discord-badge]: https://img.shields.io/discord/1255867192503832688?label=MakerPnP%20discord&color=%2332c955
