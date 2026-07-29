# Changelog

<!-- Entries are one line per item and section headings repeat for every release,
     so the line-length and duplicate-heading rules don't fit this file. -->
<!-- markdownlint-disable MD013 MD024 -->

## master (unreleased)

### New Features

- [#2231](https://github.com/flycheck/flycheck/pull/2231): Make the `rust-clippy` checker configurable with `flycheck-rust-clippy-args` (extra `cargo clippy` arguments) and the `flycheck-rust-clippy-tests`, `flycheck-rust-clippy-all-targets` and `flycheck-rust-clippy-all-features` toggles; `flycheck-rust-features` now applies to `rust-clippy` as well (originally proposed in [#2087](https://github.com/flycheck/flycheck/pull/2087) and [#2125](https://github.com/flycheck/flycheck/pull/2125)).
- [#2231](https://github.com/flycheck/flycheck/pull/2231): Add `flycheck-rust-edition` to set the edition (`--edition`) for the `rust` checker when checking single-file crates outside a Cargo project.
- [#2234](https://github.com/flycheck/flycheck/pull/2234): Make the `python-ruff` checker configurable without a config file: `flycheck-python-ruff-select`, `flycheck-python-ruff-extend-select` and `flycheck-python-ruff-ignore` for rule tuning, `flycheck-python-ruff-target-version`, `flycheck-python-ruff-preview`, and `flycheck-python-ruff-args` for arbitrary `ruff check` arguments.
- [#2237](https://github.com/flycheck/flycheck/pull/2237): Expose more `ruby-rubocop` options: `flycheck-rubocop-server` to keep a warm server process alive (`--server`), `flycheck-rubocop-only` and `flycheck-rubocop-except` to focus or suppress cops, and `flycheck-rubocop-args` for arbitrary arguments.
- [#2238](https://github.com/flycheck/flycheck/pull/2238): Add `flycheck-python-mypy-args` for passing arbitrary arguments (such as `--strict` or `--ignore-missing-imports`) to `mypy`.
- [#2239](https://github.com/flycheck/flycheck/pull/2239): Add `flycheck-dockerfile-hadolint-config` to point `dockerfile-hadolint` at a `.hadolint.yaml` file (via `--config`) and `flycheck-dockerfile-hadolint-args` for arbitrary arguments.
- [#2240](https://github.com/flycheck/flycheck/pull/2240): Expose more `terraform-tflint` options: `flycheck-tflint-config` for a `.tflint.hcl` file (`--config`), `flycheck-tflint-enabled-rules`/`flycheck-tflint-disabled-rules` (`--enable-rule`/`--disable-rule`), and `flycheck-tflint-args` for arbitrary arguments.
- [#2241](https://github.com/flycheck/flycheck/pull/2241): Turn `flycheck-stylelint-args` into a real user option, so the four stylelint checkers (`css`/`scss`/`sass`/`less`) accept arbitrary arguments such as `--custom-syntax`.
- [#2242](https://github.com/flycheck/flycheck/pull/2242): Add `flycheck-luacheck-globals` to allow extra globals (`--globals`) and `flycheck-luacheck-args` for arbitrary arguments to `lua-luacheck`.
- [#2243](https://github.com/flycheck/flycheck/pull/2243): Add `flycheck-yamllint-args` for passing arbitrary arguments to `yamllint`.
- [#2245](https://github.com/flycheck/flycheck/pull/2245): Add `flycheck-flake8-args` and `flycheck-pylint-args` for passing arbitrary arguments (such as flake8's `--select`/`--ignore` or pylint's `--disable`/`--load-plugins`) to the `python-flake8` and `python-pylint` checkers.
- [#2246](https://github.com/flycheck/flycheck/pull/2246): Add `flycheck-phpcs-args` and `flycheck-phpmd-args` for passing arbitrary arguments (such as phpcs's `--sniffs`/`--exclude`/`--severity`) to the `php-phpcs` and `php-phpmd` checkers.
