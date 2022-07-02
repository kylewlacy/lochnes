# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [0.1.1] - 2022-07-02
### Changes

- Fix generator syntax for newer versions of Rust.
- Pin Lochnes to a specific Rust version using a new [`rust-toolchain.toml`](https://rust-lang.github.io/rustup/overrides.html#the-toolchain-file) file in the project root.
- Upgrade all dependencies.
- Switch from `log` and `stderrlog` to `tracing` and `tracing-subscriber`.

### Security

- Upgrade and tweak dependencies to resolve Dependabot alerts from transitive dependencies.

## [0.1.0] - 2019-10-19
### Added

- Initial release!

[Unreleased]: https://github.com/kylewlacy/lochnes/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/kylewlacy/lochnes/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/kylewlacy/lochnes/releases/tag/v0.1.0
