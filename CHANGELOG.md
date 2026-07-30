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
- [#2247](https://github.com/flycheck/flycheck/pull/2247): Add args vars to the `elixir-credo`, `erlang` and `scala-scalastyle` checkers: `flycheck-elixir-credo-args`, `flycheck-erlang-args` and `flycheck-scalastyle-args`.
- [#2248](https://github.com/flycheck/flycheck/pull/2248): Add args vars to the `protobuf-protoc`, `puppet-lint` and `statix` checkers: `flycheck-protoc-args`, `flycheck-puppet-lint-args` and `flycheck-statix-args`.
- [#2249](https://github.com/flycheck/flycheck/pull/2249): Add args vars to the `markdown-markdownlint-cli`, `markdown-markdownlint-cli2` and `textlint` checkers: `flycheck-markdown-markdownlint-cli-args`, `flycheck-markdown-markdownlint-cli2-args` and `flycheck-textlint-args`.

### Bugs fixed

- [#2233](https://github.com/flycheck/flycheck/pull/2233): Fix `flycheck-annotate-mode`'s `below` style trapping vertical motion on the annotated line - `next-line` needed an extra press to get past it and `evil-next-visual-line` could not move past it at all. The multi-line message now hangs off the following line instead of the annotated line's newline.
- [#2235](https://github.com/flycheck/flycheck/pull/2235): Fix `flycheck-verify-setup` erroring with `Wrong type argument: number-or-marker-p, nil` when `eslint` is not installed ([#2232](https://github.com/flycheck/flycheck/issues/2232)); `flycheck-call-checker-process-for-output` no longer chokes on a missing executable either.
- [#2236](https://github.com/flycheck/flycheck/pull/2236): Detect the project root of a `javascript-eslint` buffer from a flat config file (`eslint.config.js` and its `.mjs`/`.cjs`/`.ts` variants), not only the legacy `.eslintrc`/`.eslintignore` that ESLint 9 dropped.
- [#2244](https://github.com/flycheck/flycheck/pull/2244): Apply `flycheck-go-build-tags` to the `go-vet` checker as well, so a tagged build is checked consistently across the Go checkers.
- [#2250](https://github.com/flycheck/flycheck/pull/2250): Fix several option-variable defects: `flycheck-cuda-includes` and `flycheck-tflint-variable-files` used a directory widget for options that are lists of files, `flycheck-annotate-format-function` and `flycheck-annotate-style-functions` were missing `:risky`, and a few options carried a malformed `:package-version`.

### Changes

- [#2250](https://github.com/flycheck/flycheck/pull/2250): Tidy up the configuration for consistency: rename `flycheck-mode-success-indicator` to `flycheck-mode-line-success-indicator`, `flycheck-jsonnet-command-args` to `flycheck-jsonnet-args`, and the markdownlint-cli `-enable-rules`/`-disable-rules` options to `-enabled-rules`/`-disabled-rules` (the old names remain as obsolete aliases), and mark more options `:safe` so they can be set as file- or directory-local variables.
- [#2251](https://github.com/flycheck/flycheck/pull/2251): Improve two stale defaults: `flycheck-gfortran-language-standard` now defaults to nil (GFortran's own default) instead of the 1995 standard, and `flycheck-phpcs-changed-git-base` defaults to `"main"` instead of `"trunk"`.
- [#2252](https://github.com/flycheck/flycheck/pull/2252): Give the config-file options a single, consistent name scheme: the `…rc` variables (`flycheck-flake8rc`, `flycheck-rubocoprc`, `flycheck-stylelintrc`, and the rest) are renamed to a `…-config` suffix (`flycheck-flake8-config`, `flycheck-rubocop-config`, `flycheck-stylelint-config`, …), matching the newer checkers. The old names keep working as obsolete aliases.
