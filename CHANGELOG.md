# Changelog

<!-- Entries are one line per item and section headings repeat for every release,
     so the line-length and duplicate-heading rules don't fit this file. -->
<!-- markdownlint-disable MD013 MD024 -->

## master (unreleased)

### Changes

- [#2376](https://github.com/flycheck/flycheck/pull/2376): Markdown checkers run in Emacs 31’s experimental new `markdown-ts-mode`.
- [#2378](https://github.com/flycheck/flycheck/pull/2378): `flycheck-check-project` works on a project that lives on a remote host, running the checker over TRAMP on the host the project is on instead of refusing.
- [#2373](https://github.com/flycheck/flycheck/pull/2373): `go-staticcheck` errors carry the end position staticcheck reports, so a finding highlights the expression it is about rather than a single point; a finding that spans nothing, such as an unused declaration, stays a point.
- [#2357](https://github.com/flycheck/flycheck/pull/2357): `haskell-ghc` reads GHC 9.10's JSON diagnostics when the compiler has them (probed per binary): errors carry precise spans and GHC-NNNNN ids that `C-c ! e` explains via the Haskell error index, and JSON-reading checkers in general no longer choke on text lines that merely start with a bracket, like GHC's compilation progress.
- [#2356](https://github.com/flycheck/flycheck/pull/2356): `haskell-hlint` reads hlint's JSON ideas: hint names become error ids, spans get right-open end columns, and an idea with a replacement carries a machine-applicable fix, applied with `C-c ! f`.
- [#2355](https://github.com/flycheck/flycheck/pull/2355): `go-vet` reads `go vet -json`: analyzer findings carry the analyzer's name as their id and byte-precise positions with end columns, and a compile error that stops the analyzers is reported at its real position as an error rather than a warning.
- [#2354](https://github.com/flycheck/flycheck/pull/2354): `c/c++-gcc` reads GCC's SARIF output when the compiler supports it well (probed per binary, GCC 15 or newer - the SARIF of 13 and 14 can lose diagnostics - with the text patterns still covering older GCC and the Clang that answers to gcc on macOS): errors carry precise spans, warnings keep their flag ids, and GCC's notes become the error's related locations; the shared SARIF parser now maps related locations for every SARIF checker.
- [#2353](https://github.com/flycheck/flycheck/pull/2353): `puppet-lint` and `actionlint` read their tools' JSON output, giving puppet-lint problems the columns the old patterns never captured and actionlint errors precise end positions.
- [#2352](https://github.com/flycheck/flycheck/pull/2352): `python-mypy` reads mypy's JSON output (requiring mypy 1.11 or newer): errors carry precise end positions (with mypy 1.20+), stub-install hints stay attached to their error, and messages lose the stray leading space the old patterns captured.
- [#2351](https://github.com/flycheck/flycheck/pull/2351): The RuboCop family (`rubocop`, `standardrb`, `cookstyle`) reads the tools' JSON output: ids are bare cop names without the `[Correctable] ` marker (which un-breaks explanations for correctable offenses), errors carry precise end positions, and refactor/info severities are no longer dropped.

### New Features

- [#2386](https://github.com/flycheck/flycheck/pull/2386): Fixes apply to errors in other files of the project: `x` in the error list opens the file an error belongs to and applies the fix there, and `X` in the project scope fixes every file the list shows.
- [#2385](https://github.com/flycheck/flycheck/pull/2385): The native `flycheck-lsp` client checks a file on a remote host, starting the language server on that host over TRAMP instead of declining the buffer.
- [#2381](https://github.com/flycheck/flycheck/pull/2381): New commands `flycheck-lsp-list-servers`, `flycheck-lsp-restart-server` and `flycheck-lsp-shutdown-servers` show the language servers Flycheck is running and stop them, which previously took restarting Emacs.
- [#2380](https://github.com/flycheck/flycheck/pull/2380): New `flycheck-lsp-prefer-server` option: automatic checker selection can use a linter's own resident LSP server in place of the command checker that spawns it afresh on every check, for the linters `flycheck-lsp-checker-servers` names.
- [#2364](https://github.com/flycheck/flycheck/pull/2364): The native `flycheck-lsp` client speaks the pull model of LSP 3.17: a server offering a `diagnosticProvider` is asked for each document's diagnostics, and `flycheck-check-project` asks servers covering their workspace for the diagnostics of every file in it, visited or not.
- [#2362](https://github.com/flycheck/flycheck/pull/2362): New project checkers `cargo-check` and `mypy-project` check a whole Rust workspace or mypy-configured Python project at once via `flycheck-check-project`, reporting through `rust-cargo` and `python-mypy` so identical findings collapse against buffer checks.
- [#2360](https://github.com/flycheck/flycheck/pull/2360): New command `flycheck-check-project` (`C-c ! P`) runs project checkers - tools that check a whole project at once for the cross-file problems buffer checks cannot see - starting with `terraform-validate`; their diagnostics join the error list's project scope and the mode line's project counts.
- [#2358](https://github.com/flycheck/flycheck/pull/2358): New `flycheck-navigation-scope` option: with `project`, `flycheck-next-error` continues into the project's other diagnostics when the buffer runs out of errors, opening the next file at its first error.
- [#2346](https://github.com/flycheck/flycheck/pull/2346): New `flycheck-mode-line-scope` option to make the mode-line counter show the project-wide diagnostics instead of the current buffer's ([#2340](https://github.com/flycheck/flycheck/issues/2340)).
- [#2344](https://github.com/flycheck/flycheck/pull/2344): New `flycheck-fix-edit-new-at-pos` constructor building a fix edit from buffer positions, as `flycheck-error-new-at-pos` does for errors ([#2343](https://github.com/flycheck/flycheck/issues/2343)).
- [#2341](https://github.com/flycheck/flycheck/pull/2341): The error list's project scope now shows the diagnostics a language server pushed for files that are not open in any buffer, with both the Eglot bridge and the native `flycheck-lsp` checker ([#2340](https://github.com/flycheck/flycheck/issues/2340)).

### Bugs fixed

- [#2396](https://github.com/flycheck/flycheck/pull/2396): A language server running on a remote host is stopped there when Flycheck gives up on it, so a server that never answers the `initialize` handshake no longer leaves a copy of itself behind on every attempt.
- [#2395](https://github.com/flycheck/flycheck/pull/2395): A check that is stopped, or superseded by the next one, no longer leaves its checker running on the remote host: Flycheck interrupts the command there, which is what actually stops it, rather than only dropping its own end of the process.
- [#2394](https://github.com/flycheck/flycheck/pull/2394): A checker that reads the buffer from standard input now works on a remote host, where the check used to run for ever without ever reporting: Flycheck writes the buffer to a file there and has the remote shell redirect it, since standard input cannot be closed over a TRAMP connection.
- [#2391](https://github.com/flycheck/flycheck/pull/2391): Setting `flycheck-relevant-error-other-file-minimum-level` to nil shows errors of every level from other files, as it says it does, rather than dropping the `info` ones.
- [#2390](https://github.com/flycheck/flycheck/pull/2390): The error list's group headers are no longer cut off: they share the File column with the file names, which is now made wide enough for them.
- [#2390](https://github.com/flycheck/flycheck/pull/2390): A file in a project the buffer and the checker spell differently, as they do for a symlinked directory or macOS's `/tmp`, is named relative to the project in the error list rather than by its whole path.
- [#2388](https://github.com/flycheck/flycheck/pull/2388): A rustc or Clippy suggestion that only deletes code, like "remove this `mut`", keeps its own message instead of having the empty replacement appended to it.
- [#2383](https://github.com/flycheck/flycheck/pull/2383): A buffer using `flycheck-lsp-prefer-server` contributes its server’s findings to the error list’s project scope and the mode-line project counts, as one using `flycheck-lsp-mode` already did.
- [#2371](https://github.com/flycheck/flycheck/pull/2371): LSP quick fixes work in a buffer on a remote host, where the server names the file as its own host sees it and Flycheck compared that against the buffer's remote name, silently declining every fix; the native `flycheck-lsp` client now declines a remote buffer outright, since it looked the server up on the remote host but started it on this one, and leaves those buffers to the command checkers.
- [#2370](https://github.com/flycheck/flycheck/pull/2370): Remote checking keeps the two machines straight: a related location a checker reports on the remote host now opens there rather than at the same path on the local machine, and the `c/c++-clang`, `c/c++-gcc`, `fortran-gfortran`, `d-dmd`, `haskell-ghc`, `haskell-stack-ghc`, `rst-sphinx` and `emacs-lisp` checkers pass plain remote paths on their command lines instead of TRAMP file names the remote tool cannot resolve.
- [#2368](https://github.com/flycheck/flycheck/pull/2368): A `below`-style annotation no longer takes the following line's number under `display-line-numbers-mode`: the block now stands in for the annotated line's newline, which keeps the numbers right and visual-line motion stepping down over it; moving up onto a line annotated this way, which takes `flycheck-annotate-other-lines-style` set to `below`, now lands at its end rather than at the goal column ([#2367](https://github.com/flycheck/flycheck/issues/2367)).
- [#2366](https://github.com/flycheck/flycheck/pull/2366): The `go-vet` checker vets the buffer's whole package as saved on disk instead of the single file, which `go vet` treats as a package of its own and fills with spurious undefined references to sibling files; outside a module it steps aside, saying why ([#2365](https://github.com/flycheck/flycheck/issues/2365)).
- [#2361](https://github.com/flycheck/flycheck/pull/2361): A checker chain continues through a disabled or unavailable link to the checkers behind it, so disabling one checker no longer silences the rest of its chain ([#2359](https://github.com/flycheck/flycheck/issues/2359)).
- [#2350](https://github.com/flycheck/flycheck/pull/2350): The Eglot bridge composes diagnostic messages from Emacs 32's split Flymake fields itself, so they read identically across Emacs versions instead of picking up stray separators.
- [#2349](https://github.com/flycheck/flycheck/pull/2349): The `rst-sphinx` checker reads the warning-type tags Sphinx 8 appends (e.g. `[ref.envvar]`) as error identifiers instead of leaving them in the message.
- [#2348](https://github.com/flycheck/flycheck/pull/2348): Range queries against the diagnostics of a `flycheck-eglot-mode` buffer now keep `overlays-in`'s strict edges, so a diagnostic merely touching a boundary is no longer served.
- [#2347](https://github.com/flycheck/flycheck/pull/2347): A single-position `flymake-diagnostics` call in a `flycheck-eglot-mode` buffer no longer returns diagnostics from past the position ([#2345](https://github.com/flycheck/flycheck/issues/2345)).
- [#2339](https://github.com/flycheck/flycheck/pull/2339): `perl-perlimports` quick fixes now notice when the buffer changed while the tool ran, instead of applying their edits to shifted positions.
- [#2338](https://github.com/flycheck/flycheck/pull/2338): `emacs-lisp-checkdoc` no longer complains about missing file comments in buffers that have no backing file, such as org indirect edit buffers.

## 39.0 (2026-08-10)

### New Features

- [#2336](https://github.com/flycheck/flycheck/pull/2336): Carry the rule ids `yaml-actionlint`, `r-lintr` and `puppet-lint` already print, so the error list shows which check fired and `C-c ! e` can explain lintr's linters through their reference pages; messages lose the bracketed or parenthesised id they used to carry inline.
- [#2335](https://github.com/flycheck/flycheck/pull/2335): Bring back the `sass` and `scss` checkers, on Dart Sass; the originals went out in 36.0 with the death of Ruby Sass. Compiling catches what stylelint alone does not, such as undefined variables and mixins; compile errors come first, deprecation warnings and `@warn` messages show as warnings, and a buffer that compiles cleanly enough is handed on to the stylelint checkers. Requires Dart Sass 1.74 or newer; on anything older the checkers disable themselves rather than fail every check ([#1549](https://github.com/flycheck/flycheck/issues/1549)).
- [#2334](https://github.com/flycheck/flycheck/pull/2334): Add a `luau-analyze` checker for the Luau language, using the analyzer that ships with Luau (originally proposed in [#2167](https://github.com/flycheck/flycheck/pull/2167)). It runs in the Lua major modes but only on `.luau` files, since the analyzer rejects plain Lua constructs like `goto`, and it checks the buffer through a copy next to the original, so the project's `.luaurc` applies to unsaved contents.
- [#2331](https://github.com/flycheck/flycheck/pull/2331): Add `flycheck-jsonnet-ext-code-files` to bind external code files (`--ext-code-file`) for the `jsonnet` checker (originally proposed in [#1932](https://github.com/flycheck/flycheck/pull/1932)).
- [#2329](https://github.com/flycheck/flycheck/pull/2329): Explain more checkers' diagnostics with `C-c ! e`: `yaml-yamllint`, `terraform-tflint`, `ruby-reek`, `markdown-pymarkdown` and `javascript-oxlint` now open the rule's documentation page.
- [#2322](https://github.com/flycheck/flycheck/pull/2322): `textlint` carries the fixes its fixable rules emit, such as `common-misspellings`, so `C-c ! f` applies them; its eslint-compatible output had the fixes all along, and now a spec keeps it that way.
- [#2319](https://github.com/flycheck/flycheck/pull/2319): `markdown-markdownlint-cli` now carries the fixes markdownlint suggests, so `C-c ! f` and `C-c ! F` apply them; the checker reads markdownlint's JSON output, and a rule configured with `severity: warning` now shows as a warning rather than an error.
- [#2156](https://github.com/flycheck/flycheck/pull/2156): Add OCaml checkers. `ocaml-dune` checks a Dune project with `dune build @check`, so references to sibling modules and to the project's dependencies resolve, and `ocaml` checks a standalone file with `ocamlfind ocamlc`. Flycheck picks between them by whether the file belongs to a Dune project, since the compiler on its own would report every reference to a sibling module as an unbound module.
- [#2157](https://github.com/flycheck/flycheck/pull/2157): Add a `swift` checker using `swiftc -parse`, which parses a file without type-checking it, so it is right for any Swift file whether or not it belongs to a package. Type errors need a language server that knows how the project is built, through `global-flycheck-eglot-mode` or `flycheck-lsp-mode`.
- [#2286](https://github.com/flycheck/flycheck/pull/2286): Make `tcl-nagelfar` configurable: `flycheck-tcl-nagelfar-syntax-databases` lists the databases to load with `-s`, so a project whose procedures span several files stops reporting them as unknown commands, and `flycheck-tcl-nagelfar-args` passes arbitrary arguments (originally proposed in [#1906](https://github.com/flycheck/flycheck/pull/1906)).
- [#2279](https://github.com/flycheck/flycheck/pull/2279): Show LSP diagnostic tags. A language server marks unused imports and unreachable code as *unnecessary* and stale APIs as *deprecated*; those errors are now dimmed and struck through respectively, on top of the usual highlighting, with `flycheck-eglot-mode` and the native `flycheck-lsp` checker ([#2275](https://github.com/flycheck/flycheck/issues/2275)). Errors gain a `tags` slot, so other checkers can set them too. A tag is not a level: an unused import is still a warning.
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
- [#2254](https://github.com/flycheck/flycheck/pull/2254): Add args vars to the Go checkers: `flycheck-go-gofmt-args`, `flycheck-go-vet-args`, `flycheck-go-build-args`, `flycheck-go-test-args` and `flycheck-go-errcheck-args`.
- [#2256](https://github.com/flycheck/flycheck/pull/2256): Make the JavaScript and data-format checkers configurable: `javascript-oxlint` gains `flycheck-javascript-oxlint-config`, `flycheck-javascript-oxlint-deny` and `flycheck-javascript-oxlint-allow` for rules and categories, and an args var; `yaml-actionlint` gains `flycheck-yaml-actionlint-config`; and `javascript-standard`, `json-jq` and `yaml-jsyaml` gain args vars.
- [#2261](https://github.com/flycheck/flycheck/pull/2261): Let a checker step aside when its tool cannot run in the buffer, instead of failing the same way on every check. `python-flake8`, `python-ruff`, `python-pylint`, `python-mypy`, `ruby-rubocop`/`ruby-standard`/`ruby-chef-cookstyle`, `sh-shellcheck` and the four stylelint checkers now recognise the exit statuses their tools use for a bad invocation, an unreadable configuration or a missing dependency, and disable themselves with the reason rather than reporting a suspicious result. A `python-flake8` running under an interpreter that lacks its dependencies now says `ModuleNotFoundError: No module named 'pycodestyle'`, and a stylelint with no configuration file says so. `C-u C-c ! x` re-enables a checker that disabled itself.

- [#2270](https://github.com/flycheck/flycheck/pull/2270): Say so when nothing can check a buffer, instead of leaving an inert `FlyC-` in the mode line with no explanation. Flycheck mentions it once per major mode per session, and distinguishes a mode no checker supports from one whose checkers are all unusable, which usually means the tool is not installed.
- [#2270](https://github.com/flycheck/flycheck/pull/2270): Mark an error whose fix has to be fetched before it is known to exist, such as an LSP `quickfix` code action, with `[fix?]` in the error list. Those errors deliberately carry no fix indicator, since it would promise a fix that may not be there, but nothing suggested `C-c ! f` was worth a try either.

### Bugs fixed

- [#2333](https://github.com/flycheck/flycheck/pull/2333): Stop the C/C++ checkers warning about `#pragma once` in every standalone header ([#2178](https://github.com/flycheck/flycheck/issues/2178)): the buffer is always the compiler's main file, so the warning fired on the very idiom headers are supposed to use. In a source file it still fires, as intended.
- [#2331](https://github.com/flycheck/flycheck/pull/2331): Read go-jsonnet's static errors, which carry no `STATIC ERROR:` prefix and never matched the patterns written against the C++ binary, so the `jsonnet` checker reported nothing on the implementation flycheck's own documentation recommends.
- [#2327](https://github.com/flycheck/flycheck/pull/2327): Read Scala 3's diagnostics. The compiler draws each of them in a box under a `-- [E008] Not Found Error: file:line:column` header, with unconditional coloring, so the `scala` checker has reported nothing on Scala 3 since dotty shipped. Scala 2's plain output is still read ([#2179](https://github.com/flycheck/flycheck/issues/2179)).
- [#2325](https://github.com/flycheck/flycheck/pull/2325): Stop `org-lint` freezing Emacs on large Org setups ([#2161](https://github.com/flycheck/flycheck/issues/2161)): its `invalid-id-link` checker rescans every org-id file in the session on each check, so it sits in the new `flycheck-org-lint-disabled-checkers` by default; set that to nil to run everything org-lint has.
- [#2324](https://github.com/flycheck/flycheck/pull/2324): Keep the errors earlier checkers in a chain reported when a later checker dies by a signal, and say what killed it, instead of clearing the buffer's whole result ([#1881](https://github.com/flycheck/flycheck/issues/1881)).
- [#2323](https://github.com/flycheck/flycheck/pull/2323): Fix `flycheck-next-error` cycling forever between two errors when one's region sits inside the other's, and `flycheck-previous-error` skipping the inner one ([#1781](https://github.com/flycheck/flycheck/issues/1781)). Language servers report overlapping regions routinely, so the 2020 bug bites much harder today.
- [#2299](https://github.com/flycheck/flycheck/pull/2299): Show an annotation straight away when you jump to an error, rather than on the next thing you do ([#2293](https://github.com/flycheck/flycheck/issues/2293)). Annotations are built for the visible part of the buffer from `post-command-hook`, which runs before redisplay, so a jump that sends point off screen was building them while the window still described where it had been.
- [#2299](https://github.com/flycheck/flycheck/pull/2299): Keep a `sideline` annotation off the column the continuation glyph needs, so a message that fits stops spilling onto the next line by one character ([#2292](https://github.com/flycheck/flycheck/issues/2292)). Without a right fringe to draw that glyph in, as on a terminal, the rightmost column is not text's to use.
- [#2297](https://github.com/flycheck/flycheck/pull/2297): Read rebar3's diagnostics again. rebar3 3.24 replaced `file:line:column: message` with a box drawn around the offending line, which none of the patterns matched, so `erlang-rebar3` has been reporting nothing since. The mark under the offending line says which column it means, so the column survives. Older rebar3's plain output is still read.
- [#2297](https://github.com/flycheck/flycheck/pull/2297): Read a parse error from the byte compiler on Emacs 31 and newer, which goes on to name the buffer it was reading and so no longer matched.
- [#2297](https://github.com/flycheck/flycheck/pull/2297): Point the `rst` checker at `rst2pseudoxml`. Docutils 0.21 dropped the `.py` from its front ends, so `rst2pseudoxml.py` has not existed since April 2024 and the checker had nothing to run.
- [#2289](https://github.com/flycheck/flycheck/pull/2289): Report `check-declare` warnings again on Emacs 29 and newer. Emacs 29 put the whole warning on one line where earlier versions broke it across two, which the pattern did not allow, so `flycheck-emacs-lisp-check-declare` has been finding nothing since.
- [#2289](https://github.com/flycheck/flycheck/pull/2289): Stop byte-compiling and checkdoc'ing `Eask` files, which hold data rather than code. The list of such files still named only Cask and Carton, and the `Eldev` exclusion beside it compared a whole path against a bare file name, so it never applied either.
- [#2289](https://github.com/flycheck/flycheck/pull/2289): Drop the program name jq 1.7 puts in front of its diagnostics, so a `json-jq` message reads `Expected value before ','` rather than `jq: parse error: Expected value before ','`.
- [#2303](https://github.com/flycheck/flycheck/pull/2303): Fix Flycheck asking Eglot for diagnostics a hundred times a second while a diagnostic is live, on Emacs 31 with a server that answers `textDocument/diagnostic` ([#2291](https://github.com/flycheck/flycheck/issues/2291)). The two reports Eglot answers with are the two halves of one set, the pulled diagnostics and the pushed ones; taking them as competing answers made each one look like a change, and the check that followed asked again. Flycheck now assembles the reports that arrive together and re-checks only when the result differs from what it already shows. Diagnostics a report did not account for are kept rather than dropped, so the pulled and the pushed set both reach the buffer.
- [#2284](https://github.com/flycheck/flycheck/pull/2284): Fix Emacs exhausting `max-lisp-eval-depth` in an Eglot buffer on Emacs 31 ([#2201](https://github.com/flycheck/flycheck/issues/2201)). Eglot answers one request for diagnostics with two reports, the pulled set and then the pushed one, and [#2278](https://github.com/flycheck/flycheck/pull/2278) treated only the first as the answer; the second looked volunteered and started a check that asked again.
- [#2282](https://github.com/flycheck/flycheck/pull/2282): Refuse a quick fix whose edit names coordinates that cannot mean what they say, rather than editing somewhere the checker never pointed at. A column of zero used to raise an `args-out-of-range`, a negative line silently edited the first line, and an end before its start was quietly swapped. A position past the end of the buffer is still allowed, since a fix for a missing trailing newline names one.
- [#2281](https://github.com/flycheck/flycheck/pull/2281): Fold a multi-line message onto one line in `flycheck-annotate-mode`'s compact styles. Plenty of checkers wrap a message over several lines, and the newline reached the screen: `eol` gave the annotated line extra rows instead of sitting after it, and `sideline` lost the right-edge alignment that defines it. The `below` style still shows messages in full.
- [#2280](https://github.com/flycheck/flycheck/pull/2280): Carry `flycheck-annotate-background`'s line tint through a `below`-style annotation, so the tinted line and the messages under it read as one region instead of a tinted bar, an untinted block and a tinted bar again ([#2276](https://github.com/flycheck/flycheck/issues/2276)). The messages keep their own colours.
- [#2278](https://github.com/flycheck/flycheck/pull/2278): Fix Emacs locking up in an Eglot buffer whose server answers `textDocument/diagnostic` rather than pushing ([#2262](https://github.com/flycheck/flycheck/issues/2262)). Under that model asking Eglot for diagnostics sends a request and returns, so the answer arrived after the guard against re-checking had unwound: the answer started a check, the check asked again, and rust-analyzer was fielding hundreds of requests a second.
- [#2266](https://github.com/flycheck/flycheck/pull/2266): Stop re-running a check for a diagnostics push that changes nothing. Language servers republish an unchanged set freely while they index or build, and every one of those used to cost a full check, overlay rebuild included ([#2262](https://github.com/flycheck/flycheck/issues/2262)). Both the Eglot bridge and the native `flycheck-lsp` checker now compare before re-checking. `flycheck-verify-setup` reports how many pushes a buffer's server sent, how many of them changed anything, and at what rate.
- [#2277](https://github.com/flycheck/flycheck/pull/2277): Stop the `*eldoc*` window still popping open on a jump when any Eldoc source answers asynchronously, such as a language server ([#2201](https://github.com/flycheck/flycheck/issues/2201)). Eldoc waits for every source before displaying, so the display outlived the binding [#2265](https://github.com/flycheck/flycheck/pull/2265) relied on.
- [#2265](https://github.com/flycheck/flycheck/pull/2265): Stop `flycheck-next-error` and other jumps popping the `*eldoc*` window open. Documenting interactively is what refreshes the echo area after a command Eldoc does not recognise, but `eldoc-display-in-buffer` reads the same flag as a request to show the documentation buffer ([#2201](https://github.com/flycheck/flycheck/issues/2201)). Only the echo area sees the request now; the buffer is still kept up to date.
- [#2264](https://github.com/flycheck/flycheck/pull/2264): Make the two LSP bridges compose, so a lint server can run behind Eglot's server: with `flycheck-eglot-mode` and `flycheck-lsp-mode` both on and both `-exclusive` options nil, `eglot-check` now chains to `flycheck-lsp` and both report into the same error list. Previously each bridge chained only to a command checker and never to the other, so whichever mode enabled last silently won. Which of the two leads no longer depends on the order the modes enable in either.
- [#2257](https://github.com/flycheck/flycheck/pull/2257): Fix the stale value of `flycheck-version`, which `M-x flycheck-version` reports when Flycheck was not installed as a package. Development builds now report `39.0-snapshot`.
- [#2259](https://github.com/flycheck/flycheck/pull/2259): Stop marking every LSP diagnostic as fixable. A code-action fix is only fetched when applied, so Flycheck cannot know in advance that one exists, and marking it anyway replaced the familiar double-arrow indicator with the fix indicator on every diagnostic in an Eglot or `flycheck-lsp` buffer. The fix indicator, the error list's `[fix]` badge and the inline fix marker now appear only where a fix is known to exist; `C-c ! f` still asks the server on any diagnostic.
- [#2259](https://github.com/flycheck/flycheck/pull/2259): Explain the mode-line indicator when no error counts sit behind it: `-`, `!`, `.` and `?` now say what they mean in a tooltip, and clicking one runs `flycheck-verify-setup` on the buffer.
- [#2259](https://github.com/flycheck/flycheck/pull/2259): Replace the wall of text Flycheck printed to the echo area when it could not read a checker's output. A crashing linter, such as a misconfigured `python-flake8` printing a traceback, now produces one line naming the checker and pointing at `C-c ! v`, which shows what the checker actually printed. Flycheck no longer suggests filing a bug report against itself for what is nearly always a local setup problem.
- [#2233](https://github.com/flycheck/flycheck/pull/2233): Fix `flycheck-annotate-mode`'s `below` style trapping vertical motion on the annotated line - `next-line` needed an extra press to get past it and `evil-next-visual-line` could not move past it at all. The multi-line message now hangs off the following line instead of the annotated line's newline.
- [#2235](https://github.com/flycheck/flycheck/pull/2235): Fix `flycheck-verify-setup` erroring with `Wrong type argument: number-or-marker-p, nil` when `eslint` is not installed ([#2232](https://github.com/flycheck/flycheck/issues/2232)); `flycheck-call-checker-process-for-output` no longer chokes on a missing executable either.
- [#2236](https://github.com/flycheck/flycheck/pull/2236): Detect the project root of a `javascript-eslint` buffer from a flat config file (`eslint.config.js` and its `.mjs`/`.cjs`/`.ts` variants), not only the legacy `.eslintrc`/`.eslintignore` that ESLint 9 dropped.
- [#2244](https://github.com/flycheck/flycheck/pull/2244): Apply `flycheck-go-build-tags` to the `go-vet` checker as well, so a tagged build is checked consistently across the Go checkers.
- [#2255](https://github.com/flycheck/flycheck/pull/2255): Fix the error message not reaching the echo area after `flycheck-next-error` and other jumps, with the default Eldoc-based display ([#2201](https://github.com/flycheck/flycheck/issues/2201)); Eldoc keeps out of the echo area unless the command that ran is one it knows, so Flycheck now asks it to document interactively.
- [#2250](https://github.com/flycheck/flycheck/pull/2250): Fix several option-variable defects: `flycheck-cuda-includes` and `flycheck-tflint-variable-files` used a directory widget for options that are lists of files, `flycheck-annotate-format-function` and `flycheck-annotate-style-functions` were missing `:risky`, and a few options carried a malformed `:package-version`.

### Changes

- [#2313](https://github.com/flycheck/flycheck/pull/2313): Truncate a `sideline` annotation that cannot fit between the code and the window edge, with an ellipsis, instead of letting it land after the code and wrap onto the next line ([#2312](https://github.com/flycheck/flycheck/issues/2312)).
- [#2311](https://github.com/flycheck/flycheck/pull/2311): Remove the `haml` checker; use `haml-lint`, which reports a template that fails to parse as well as one that parses but reads badly. Haml 6 replaced its command line with subcommands in 2022, so `haml -c --stdin` has been answering `Could not find command "_c"` ever since, and neither replacement will do: `haml parse` reports no line number, and `haml render` runs the template's Ruby.
- [#2304](https://github.com/flycheck/flycheck/pull/2304): Compile without warnings on Emacs 31, which made `when-let`, `if-let`, `revert-buffer-in-progress-p` and a bare `any` in `rx` obsolete. Installing Flycheck there printed a screenful of them, and our own build treats a warning as an error, so the snapshot CI job could not get as far as running the specs.
- [#2287](https://github.com/flycheck/flycheck/pull/2287): Copy every error at point as one kill-ring entry, so a single yank pastes them all, and show the lot in the echo area rather than only the first ([#2096](https://github.com/flycheck/flycheck/pull/2096)). They went in one at a time before, which left the rest behind `M-y`, and a destructive `nreverse` meant the echo area only ever showed one. A message containing a `%` is no longer read as a format string.
- [#2250](https://github.com/flycheck/flycheck/pull/2250): Tidy up the configuration for consistency: rename `flycheck-mode-success-indicator` to `flycheck-mode-line-success-indicator`, `flycheck-jsonnet-command-args` to `flycheck-jsonnet-args`, and the markdownlint-cli `-enable-rules`/`-disable-rules` options to `-enabled-rules`/`-disabled-rules` (the old names remain as obsolete aliases), and mark more options `:safe` so they can be set as file- or directory-local variables.
- [#2251](https://github.com/flycheck/flycheck/pull/2251): Improve two stale defaults: `flycheck-gfortran-language-standard` now defaults to nil (GFortran's own default) instead of the 1995 standard, and `flycheck-phpcs-changed-git-base` defaults to `"main"` instead of `"trunk"`.
- [#2252](https://github.com/flycheck/flycheck/pull/2252): Give the config-file options a single, consistent name scheme: the `…rc` variables (`flycheck-flake8rc`, `flycheck-rubocoprc`, `flycheck-stylelintrc`, and the rest) are renamed to a `…-config` suffix (`flycheck-flake8-config`, `flycheck-rubocop-config`, `flycheck-stylelint-config`, …), matching the newer checkers. The old names keep working as obsolete aliases.
- [#2253](https://github.com/flycheck/flycheck/pull/2253): Pick more modern defaults when several checkers support a language: `python-ruff` is now preferred over `python-flake8`, `markdown-markdownlint-cli2` over `markdown-markdownlint-cli`, and `python-pyright` is reachable (ahead of `python-pycompile`) instead of a silent no-op. Reorder `flycheck-checkers` or use `flycheck-select-checker` to override.
- [#2267](https://github.com/flycheck/flycheck/pull/2267): Rename `flycheck-pylintrc` to `flycheck-pylint-config`, the one config-file option [#2252](https://github.com/flycheck/flycheck/pull/2252) left on the old scheme. The old name keeps working as an obsolete alias.

## 38.3 (2026-07-29)

### New Features

- [#2230](https://github.com/flycheck/flycheck/pull/2230): Explain more checkers' diagnostics with `C-c ! e` (`flycheck-explain-error-at-point`): `dockerfile-hadolint`, `go-staticcheck`, `python-mypy`, `ruby-rubocop`/`ruby-standard`, and the `less`/`scss`/`sass` stylelint checkers. Checker authors can now define a URL-based explainer in one line with the new `flycheck-error-explainer-from-url` helper.

### Bugs fixed

- [#2229](https://github.com/flycheck/flycheck/pull/2229): Keep `flycheck-annotate-mode`'s inline messages on the right line while editing - an edit that left point on its line (such as `open-line`) could strand an annotation on the wrong line until the next check.

## 38.2 (2026-07-29)

### Bugs fixed

- [#2228](https://github.com/flycheck/flycheck/pull/2228): Rename the native LSP checker from `lsp` to `flycheck-lsp`, so it no longer clobbers the `lsp` checker `lsp-mode` registers for its own Flycheck integration - which had left lsp-mode users with an empty error list and no highlighting ([#2226](https://github.com/flycheck/flycheck/issues/2226)).

## 38.1 (2026-07-29)

### Bugs fixed

- [#2227](https://github.com/flycheck/flycheck/pull/2227): Fix `flycheck-annotate-mode` drawing the cursor past the inline annotation, so the line at point read as if the cursor were inside the message (most visibly with the multi-line `below` style) and editing the line was awkward.

## 38.0 (2026-07-29)

### New Features

- [#2202](https://github.com/flycheck/flycheck/pull/2202): Add `flycheck-annotate-mode`, which shows error messages inline next to the code they refer to, in the spirit of VS Code's Error Lens and the inline diagnostics of Neovim, Helix and Zed. Two styles ship out of the box: `eol` appends a compact message after the line, `below` lays the full messages out underneath. Obsoletes the third-party `flycheck-inline` package.
- [#2203](https://github.com/flycheck/flycheck/pull/2203): `flycheck-annotate-mode` can tint each error's whole line with a subtle background in its severity colour (`flycheck-annotate-background`).
- [#2204](https://github.com/flycheck/flycheck/pull/2204): Add a `sideline` annotation style that flushes the compact message to the window's right edge, in the manner of `lsp-ui-sideline`.
- [#2205](https://github.com/flycheck/flycheck/pull/2205): Align `below`-style connectors to the error's real display column, so they line up under tab-indented code and past a line-number gutter.
- [#2206](https://github.com/flycheck/flycheck/pull/2206): Filter inline annotations per tier via `flycheck-annotate-current-line-levels` and `flycheck-annotate-other-lines-levels`, so the focused line and the rest can show different levels.
- [#2225](https://github.com/flycheck/flycheck/pull/2225): Add `global-flycheck-annotate-mode` to turn inline diagnostics on in every buffer Flycheck checks.
- [#2193](https://github.com/flycheck/flycheck/pull/2193): Add a quick-fix API. `C-c ! f` (`flycheck-fix-error-at-point`) and `x` in the error list apply a checker's machine-applicable fix, attached to a `flycheck-error` via the new `:fix` slot. Wired into `javascript-eslint`, the Rust checkers and SARIF-based checkers, whose fixes were previously parsed and discarded.
- [#2195](https://github.com/flycheck/flycheck/pull/2195): `python-ruff` and `sh-shellcheck` now carry the fixes their tools suggest (shellcheck 0.7 or newer required).
- [#2207](https://github.com/flycheck/flycheck/pull/2207): `C-c ! F` (`flycheck-fix-all-errors`) applies every fix in the buffer at once as a single undoable change; `X` does the same from the error list.
- [#2208](https://github.com/flycheck/flycheck/pull/2208): Mark a fixable error's line with a distinct fringe or margin indicator, like an editor's "fix available" lightbulb (`flycheck-fixable-indicator`).
- [#2210](https://github.com/flycheck/flycheck/pull/2210): A `:fix` may be a function that computes the fix on demand, so expensive fixes (such as an LSP code action) cost nothing until applied.
- [#2209](https://github.com/flycheck/flycheck/pull/2209): Integrate with Eglot out of the box: `global-flycheck-eglot-mode` reports an Eglot-managed server's diagnostics through Flycheck via the new `eglot-check` checker. Obsoletes the third-party `flycheck-eglot` package, whose mode names it reuses - uninstall that package first, or the two will clash.
- [#2211](https://github.com/flycheck/flycheck/pull/2211): Offer an Eglot diagnostic's `quickfix` code action as a Flycheck fix, fetched from the server only when you apply it (`flycheck-eglot-code-actions`).
- [#2213](https://github.com/flycheck/flycheck/pull/2213): Add a native `lsp` checker that talks to a diagnostics language server directly over the built-in `jsonrpc` library, without Eglot or `lsp-mode`. Enable `global-flycheck-lsp-mode` and configure a server per major mode in `flycheck-lsp-servers`.
- [#2214](https://github.com/flycheck/flycheck/pull/2214): Ship built-in `flycheck-lsp-servers` entries for RuboCop, Ruff, Biome and Harper, each used only when its program is installed.
- [#2215](https://github.com/flycheck/flycheck/pull/2215): Offer the server's `quickfix` code actions as fixes (`flycheck-lsp-code-actions`), and run the `initialize` handshake asynchronously so starting a server no longer briefly blocks Emacs.
- [#2218](https://github.com/flycheck/flycheck/pull/2218): Read RuboCop's and standardrb's autocorrects, which those servers ship inline in each diagnostic's data, as fixes.
- [#2192](https://github.com/flycheck/flycheck/pull/2192): Show whole-project diagnostics: `P` (`flycheck-error-list-toggle-scope`) aggregates errors across every open Flycheck buffer together with the cross-file errors that checkers like `tsc`, `cargo check` and `mypy` report but the per-buffer view drops.
- [#2196](https://github.com/flycheck/flycheck/pull/2196): Group the error list by file (`M-2`), checker (`M-3`) or level (`M-4`), nest and combine the dimensions, collapse a group with `TAB`, and drive it all from a strip at the top with the mouse.
- [#2212](https://github.com/flycheck/flycheck/pull/2212): Errors can carry secondary source locations (from an LSP diagnostic's `relatedInformation`, which Flymake discards). Visit them with `C-c ! j`; they show up inline and are badged `↳N` in the error list.
- [#2191](https://github.com/flycheck/flycheck/pull/2191): Run syntax checkers over TRAMP: a remote buffer is checked on the remote host, where before it could not run (executables must be installed there). See `flycheck-check-syntax-automatically-remote`.

### Bugs fixed

- [#2217](https://github.com/flycheck/flycheck/pull/2217): Fix a code-action fix returned as a legacy `changes` WorkspaceEdit (as Ruff emits) silently doing nothing; affects the `lsp` checker and the Eglot bridge.
- [#2197](https://github.com/flycheck/flycheck/pull/2197): Stop scanning every error-list row on cursor movement, so keeping a large project-scope list open is much cheaper.

## 37.0 (2026-07-18)

### New Features

- A new syntax check now interrupts a still-running one instead of waiting for it to finish and showing stale results, so slow checkers (cargo, mypy) feel much more responsive. `flycheck-interrupt-running-checks` controls this (default `10`: only checks younger than ten seconds are interrupted).
- Filter the error list by syntax checker (`c`) and by a regexp on the message or ID (`/`), on top of the minimum-level filter (`f`); `F` resets all filters.
- The error counts in the mode line are clickable: `mouse-1` pops up the error list.
- Add `flycheck-parse-sarif`, a ready-made `:error-parser` for the SARIF output format that many analyzers can emit. A zero-width SARIF region is treated as spanning the whole line.
- Add `asciidoc-mode` support to the `asciidoctor` and `textlint` checkers.
- Add `neocaml-opam-mode` support to the `opam` checker.
- [#1787](https://github.com/flycheck/flycheck/pull/1787): Add the `:handle-suspicious` command-checker property, letting a checker turn a suspicious state (a non-zero exit with no parsable errors) into regular errors instead of the generic warning.

### Bugs fixed

- [#1129](https://github.com/flycheck/flycheck/pull/1129): `javascript-eslint` no longer makes a blocking `--print-config` probe before the first check in every buffer (which used to freeze Emacs); a fatal eslint failure is diagnosed from the check's exit status and disables the checker with an echo-area notice. Checker authors can return `disable` or `(disable . reason)` from `:handle-suspicious`.
- [#1946](https://github.com/flycheck/flycheck/pull/1946): Fix `flycheck-lintr-linters` being ignored by recent lintr versions, which require linters to be passed as a named argument.
- [#2159](https://github.com/flycheck/flycheck/pull/2159): Mitigate CVE-2024-53920 in the `emacs-lisp` checker by requiring files to be trusted (via `trusted-content`) on Emacs 30+; `emacs-lisp-checkdoc` stays enabled.
- [#2161](https://github.com/flycheck/flycheck/pull/2161): Fix the `org-lint` checker erroring out on Emacs 31, where `org-lint` reports line numbers as strings.
- [#2163](https://github.com/flycheck/flycheck/pull/2163): Disable native compilation in the `emacs-lisp` checker subprocess so it no longer writes stray `.eln` files to the native-comp cache.
- [#2164](https://github.com/flycheck/flycheck/pull/2164): Recognize unresolved-identifier errors in the `scheme-chicken` checker.
- [#2166](https://github.com/flycheck/flycheck/pull/2166): Fix `awk-gawk` reporting a suspicious checker state for valid scripts.
- [#2169](https://github.com/flycheck/flycheck/pull/2169): Parse the severity-tagged output format introduced by Ruff 0.15.7 (e.g. `error[F401]` instead of `F401`).
- [#2170](https://github.com/flycheck/flycheck/pull/2170): Force English checker output with `LC_MESSAGES=C` instead of `LC_ALL=C`, which broke checkers reading UTF-8 input, such as `hledger`.
- [#2174](https://github.com/flycheck/flycheck/pull/2174): Fix the `haskell-ghc` and `haskell-stack-ghc` checkers passing a broken `-x` flag in `haskell-ts-mode`.
- [#2175](https://github.com/flycheck/flycheck/pull/2175): Compose the error indicator with pre-existing `wrap-prefix` text properties (e.g. from `visual-wrap-prefix-mode`).
- [#2177](https://github.com/flycheck/flycheck/pull/2177): Avoid `\N{...}` character escapes, which break native compilation on Emacs 32.

### Changes

- **(Breaking)** Drop support for Emacs 27; Flycheck now requires Emacs 28.1 or newer.
- The error list pops up in a bottom side window by default (`flycheck-error-list-display-buffer-action`); dismiss it with `q`.
- The File and ID columns of the error list size to their contents instead of truncating at fixed widths.
- Document errors at point through Eldoc by default, composing with other Eldoc sources (e.g. Eglot); restore the old behaviour via `flycheck-display-errors-function`.
- Checkers exceeding `flycheck-checker-error-threshold` are no longer silently disabled; Flycheck shows the most severe errors up to the threshold and flags the truncation in the mode line and error list. Set `flycheck-checker-error-threshold-action` to `disable` for the old behaviour.
- `flycheck-indication-mode` defaults to `auto` (left fringe on graphical displays, left margin on terminals), and widens the margin when needed instead of silently showing nothing.
- `flycheck-verify-setup` and `flycheck-verify-checker` now ask before saving a modified buffer instead of saving it silently.
- `dockerfile-hadolint` now parses hadolint's SARIF output (`--format sarif`) via `flycheck-parse-sarif`, so it no longer breaks when hadolint tweaks its human-readable format.

## 36.0 (2026-02-19)

### New Features

- [#2047](https://github.com/flycheck/flycheck/pull/2047): Add `javascript-oxlint` checker for JavaScript and TypeScript using [oxlint](https://oxc.rs/).
- [#1757](https://github.com/flycheck/flycheck/pull/1757): Add `org-lint` checker for Org mode files. The checker uses Emacs' built-in `org-lint` command to detect issues such as invalid links, dead links, and duplicate IDs.
- [#2132](https://github.com/flycheck/flycheck/pull/2132): Add the `flycheck-shellcheck-infer-shell` option to the `sh-shellcheck` checker.
- [#1977](https://github.com/flycheck/flycheck/pull/1977): Add `flycheck-shellcheck-args` for passing extra command-line arguments to ShellCheck.
- [#1854](https://github.com/flycheck/flycheck/pull/1854): Add `flycheck-shellcheck-enabled-checks` option to enable optional ShellCheck checks via the `--enable` flag.
- [#2139](https://github.com/flycheck/flycheck/pull/2139): Add compatibility with Proselint 0.16.
- [#1574](https://github.com/flycheck/flycheck/pull/1574): Enable `proselint` checker for reStructuredText mode and chain it after the `rst` and `rst-sphinx` checkers.
- [#1874](https://github.com/flycheck/flycheck/pull/1874): Add `flycheck-error-list-after-jump-hook`, run after jumping from the error list to an error location.
- [#2137](https://github.com/flycheck/flycheck/pull/2137): Allow `flycheck-command-map` to be used as a prefix command with `keymap-set` and similar functions.
- [#1833](https://github.com/flycheck/flycheck/pull/1833): Automatically re-check the buffer after `revert-buffer` (e.g. when using `global-auto-revert-mode`).
- [#1134](https://github.com/flycheck/flycheck/pull/1134): Add error explainer for the `python-ruff` checker.
- [#1979](https://github.com/flycheck/flycheck/pull/1979): Show pyright rule names (e.g. `reportGeneralTypeIssues`) as error IDs.
- [#2134](https://github.com/flycheck/flycheck/pull/2134): Include info-level errors in the mode-line indicator (format: `errors|warnings|infos`).

### Bugs fixed

- [#2131](https://github.com/flycheck/flycheck/pull/2131): Mitigate CVE-2024-53920 in the `emacs-lisp` checker subprocess by disabling local eval directives and restricting local variables to safe values during byte-compilation.
- [#2144](https://github.com/flycheck/flycheck/pull/2144): Rewrite `org-lint` checker to run in the current Emacs process instead of a `-Q --batch` subprocess. This eliminates false "Unknown source block language" warnings for languages from external packages.
- [#2043](https://github.com/flycheck/flycheck/pull/2043): Fix `rust` checker temp directory error by using `--emit=metadata --out-dir` instead of `--emit=mir -o /dev/null`. Also fixes the checker on Windows where `/dev/null` does not exist.
- [#1859](https://github.com/flycheck/flycheck/pull/1859): Force C locale (`LC_ALL=C`) for checker processes to ensure English output. Fixes error pattern matching in non-English locales.
- [#1919](https://github.com/flycheck/flycheck/pull/1919): Isolate bidi characters in error message snippets using Unicode directional isolates to prevent formatting corruption.
- [#1856](https://github.com/flycheck/flycheck/pull/1856): Strictly enforce `flycheck-navigation-minimum-level`. Previously, setting the minimum level would still navigate to lower-severity errors when no errors at the minimum level existed.
- [#1918](https://github.com/flycheck/flycheck/pull/1918): Exclude the `*Flycheck error messages*` buffer from `global-flycheck-mode`.
- [#1908](https://github.com/flycheck/flycheck/pull/1908): Increase error list File column width from 6 to 12 characters.
- [#1882](https://github.com/flycheck/flycheck/pull/1882): Fix Go build tags to use comma-separated format instead of repeated `-tags` flags.
- [#2098](https://github.com/flycheck/flycheck/pull/2098): Fix `tex-chktex` error parsing with `--inputfiles`.
- [#2143](https://github.com/flycheck/flycheck/pull/2143): Fix compilation warnings on Emacs 30 (obsolete `rx-constituents` and missing `defcustom` type spec).
- [#2089](https://github.com/flycheck/flycheck/pull/2089): Make `flycheck-protoc-import-path` buffer-local so different protobuf projects can have different import paths.
- [#2032](https://github.com/flycheck/flycheck/pull/2032): Guard Tools menu operations for Emacs configurations that remove the Tools menu (e.g. Doom Emacs).
- [#1805](https://github.com/flycheck/flycheck/pull/1805): Preserve match data in idle trigger timer handler.
- [#1170](https://github.com/flycheck/flycheck/pull/1170): Skip error list highlighting when the error list buffer is not visible, improving performance on every `post-command-hook`.
- [#1153](https://github.com/flycheck/flycheck/pull/1153): Handle `puppet-parser` errors without line numbers (e.g. "Syntax error at end of file").
- [#1886](https://github.com/flycheck/flycheck/pull/1886): Fix continuation indicator appearing on non-wrapped lines by using `wrap-prefix` instead of `line-prefix`.
- [#2062](https://github.com/flycheck/flycheck/pull/2062): Fall back to `python` executable when `python3` is unavailable (e.g. Windows with Anaconda/Miniforge).
- [#2127](https://github.com/flycheck/flycheck/pull/2127): Preserve pre-existing `line-prefix` text properties (e.g. from `org-indent-mode`) when adding flycheck overlays.
- [#2086](https://github.com/flycheck/flycheck/pull/2086): Fix the name of the PyMarkdown config.
- [#2036](https://github.com/flycheck/flycheck/pull/2036): Fix `awk-gawk` checker passing spurious quotes to `gawk --source`.
- [#2092](https://github.com/flycheck/flycheck/pull/2092): Detect parse errors (unbalanced parentheses, invalid read syntax) in `emacs-lisp` byte compilation.
- [#2090](https://github.com/flycheck/flycheck/pull/2090): Fix `python-ruff` checker to use `concise` output format instead of removed `text` format (renamed in ruff 0.2).
- Fix `python-ruff` error ID regex and `invalid-syntax` error handling.
- Fix `rpm-rpmlint` error filter returning unfiltered errors (the `(none)` filename filter was not applied).
- Add `php-ts-mode` to the `php-phpcs-changed` checker.
- [#1926](https://github.com/flycheck/flycheck/pull/1926): Fix `flycheck-cuda-gencodes` customize type (was `file`, now `string`).
- Guard `buffer-file-name` against nil in `yaml-actionlint` predicate, `erlang` enabled check, and `flycheck-rebar3-project-root`.
- Fix `python-pycompile` to verify `python3` actually works instead of just checking it exists.
- Fix proselint version detection breaking checker validation.

### Removed

- Remove `typescript-tslint` checker. TSLint has been deprecated since 2019 in favor of [ESLint with typescript-eslint](https://typescript-eslint.io/).
- Remove `sass` and `scss` checkers. Ruby Sass reached end-of-life in March 2019. Use `sass-stylelint` and `scss-stylelint` instead.
- Remove `sass/scss-sass-lint` checker. sass-lint has been abandoned for over 4 years. Use `sass-stylelint` or `scss-stylelint` instead.
- Remove `scss-lint` checker. scss-lint depends on the dead Ruby Sass engine and is no longer maintained. Use `scss-stylelint` instead.
- Remove `eruby-erubis` and `eruby-ruumba` checkers. Erubis has been abandoned since 2011 and Ruumba since 2020. ERuby support is removed.
- Remove `css-csslint` checker. CSSLint has been abandoned since ~2017. Use `css-stylelint` instead.
- Remove `protobuf-prototool` checker. Prototool was archived by Uber in March 2022.
- Remove `nix-linter` checker. nix-linter has been abandoned by its author, who recommends [statix](https://github.com/nerdypepper/statix) instead.
- Remove `coffee-coffeelint` checker. CoffeeLint has been effectively inactive with known security vulnerabilities.
- Remove `asciidoc` checker. The legacy Python AsciiDoc processor is superseded by Asciidoctor. Use `asciidoctor` instead.
- Remove `json-jsonlint` checker. The original jsonlint has been abandoned since ~2017. Use `json-python-json` or `json-jq` instead.
- Remove `xml-xmlstarlet` checker. XMLStarlet has not had a release since
  2014. Use `xml-xmllint` instead.
- Remove `javascript-jshint` checker. JSHint has been largely superseded by ESLint. Use `javascript-eslint` instead.
- Remove `yaml-ruby` checker. Ruby's YAML parser provides the same functionality as js-yaml. Use `yaml-jsyaml` or `yaml-yamllint` instead.
- Remove `ruby-jruby` checker. JRuby is extremely niche for linting. Use `ruby-rubocop` or `ruby` instead.

### Changes

- Remove dead code: `flycheck-option-symbol`, `flycheck-flake8--find-project-root`, `flycheck-string-or-nil-p`, `flycheck-chunked-process-input` and associated chunking functions.
- Replace deprecated `seq-contains` with `seq-contains-p`.
- Replace `flycheck-string-or-nil-p` with built-in `string-or-null-p`.
- Use `json-parse-buffer` unconditionally (available since Emacs 27.1), removing the `json-read` fallback and the `json` library dependency.
- Use `libxml-available-p` (available since Emacs 27.1) instead of `fboundp` check.
- Use `seq-sort-by` (available since Emacs 27.1) instead of workaround.
- Use `seq-mapcat` instead of `(apply #'append (seq-map ...))`.
- Minor code style improvements: `when (not ...)` → `unless`, `reverse` → `nreverse` for locally-built lists.
- [#2152](https://github.com/flycheck/flycheck/pull/2152): Point package `URL` header to GitHub repository instead of the website.

## 35.0 (2025-04-23)

### New Features

- [#2105](https://github.com/flycheck/flycheck/pull/2105): Add options for configuring the `jsonnet` checker.
- [#1975](https://github.com/flycheck/flycheck/pull/1975): Add support for `--expt-relaxed-constexpr` flag to `cuda` checker.
- [#2055](https://github.com/flycheck/flycheck/pull/2055): Add support for `--expt-extended-lambda` flag to `cuda` checker.
- [#1987](https://github.com/flycheck/flycheck/pull/1987): Add a flag `flycheck-auto-display-errors-after-checking` control whether to display errors automatically after checking.
- [#2035](https://github.com/flycheck/flycheck/pull/2035): Add colors to FlyC mode line and update mode line menu. Introduce `flycheck-mode-success-indicator`.
- [#2059](https://github.com/flycheck/flycheck/pull/2059): Enable checkers for new AUCTeX 14 modes.
- [#2070](https://github.com/flycheck/flycheck/pull/2070): Add a new syntax checker `r` for R with the builtin `parse` function.
- [#2073](https://github.com/flycheck/flycheck/pull/2073): Add new syntax checker `salt-lint` for the salt infrastructure-as-code language.
- [#2071](https://github.com/flycheck/flycheck/pull/2071): Add a new checker `perl-perlimports`, for cleaning up Perl import statements.
- [#1972](https://github.com/flycheck/flycheck/pull/1972): New defcustom `flycheck-clear-displayed-errors-function` to customize how error messages are to be cleared.
- [#2075](https://github.com/flycheck/flycheck/pull/2075): Add the `flycheck-chktex-extra-flags` option to the `tex-chktex` checker.
- [#2107](https://github.com/flycheck/flycheck/pull/2107): Add `-Xcompiler` option for `cuda-nvcc`.
- Add new `markdownlint-cli2` checker.

### Bugs fixed

- [#2057](https://github.com/flycheck/flycheck/pull/2057): Revert the replacement of `flycheck-version` with `lm-version`.
- [#1972](https://github.com/flycheck/flycheck/pull/1972): Refine flycheck-display-errors lifecycle so error messages can be cleared.
- [#2067](https://github.com/flycheck/flycheck/pull/2067): Handle correctly GHC 9.6 error output format.
- [#2079](https://github.com/flycheck/flycheck/pull/2079): Fix ruff `error-patterns` and `error-filter`.

### Changes

- **(Breaking)** [#2066](https://github.com/flycheck/flycheck/pull/2066): Remove support for versions of `stylelint` older than v14.
- Update `error-patterns` for ghdl 4.1.0.
- [#2078](https://github.com/flycheck/flycheck/pull/2078): ruff: `--output-format=text` replaced with `--output-format=concise` due to upstream changes in ruff.

## 34.1 (2024-02-18)

### Bugs fixed

- [#2054](https://github.com/flycheck/flycheck/pull/2054): Remove explicit dep on the built-in package `seq.el`.

## 34.0 (2024-02-14)

### New Features

- New syntax checkers
  - [#2015](https://github.com/flycheck/flycheck/pull/2015): PHP with `phpcs-changed`
  - [#2017](https://github.com/flycheck/flycheck/pull/2017): HAML with `haml-lint`
  - [#2030](https://github.com/flycheck/flycheck/pull/2030): Add `yaml-actionlint` checker for GitHub yaml action workflows.
  - [#2052](https://github.com/flycheck/flycheck/pull/2052): Sass with Stylelint
  - [#2013](https://github.com/flycheck/flycheck/pull/2013): Nix with `statix`
  - [#1935](https://github.com/flycheck/flycheck/pull/1935): Chef (Ruby) with `cookstyle`
  - [#1915](https://github.com/flycheck/flycheck/pull/1915): Markdown with `pymarkdown`
- [#1873](https://github.com/flycheck/flycheck/pull/1873): Add error explainer to `perl-perlcritic`.
- [#1875](https://github.com/flycheck/flycheck/pull/1875): Add error-explainer to `css-stylelint`.
- [#1876](https://github.com/flycheck/flycheck/pull/1876): Add error-explainer for `markdownlint checker`.
- [#2019](https://github.com/flycheck/flycheck/pull/2019): Add support for RELAX NG schema in `xmllint`.

### Bugs Fixed

- [#1793](https://github.com/flycheck/flycheck/pull/1793): Fix `flycheck-ruby-rubocop` on buffers with no backing file.

### Changes

- [#2026](https://github.com/flycheck/flycheck/pull/2026): Update the possible locations for `yamllint`'s configuration file.
- **(Breaking)** [#1697](https://github.com/flycheck/flycheck/pull/1697): Remove the `coq` checker.
- **(Breaking)** [#1935](https://github.com/flycheck/flycheck/pull/1935): Remove the `chef-foodcritic` checker. (it's now replaced by `ruby-chef-cookstyle`)
- **(Breaking)** [#2018](https://github.com/flycheck/flycheck/pull/2018): Remove the `golint` checker.
- **(Breaking)** Remove the `ruby-rubylint` checker.
- [#1704](https://github.com/flycheck/flycheck/pull/1704): The `tslint` checker is deprecated; it will go away in a future release.

------------------------------------------------------------------------

## 33.1 (2024-02-04)

- Bugs Fixed
  - Fixed an usage of the removed `flycheck--format-message` function.

## 33.0 (2024-02-04)

**Note:** The changelog for this release is incomplete.

- New features and improvements
  - The `flycheck-verify-setup` UI now includes buttons to re-enable manually disabled checkers and to try to re-enable automatically disabled checkers (command checkers are automatically disabled when their executable cannot be found). [#1755](https://github.com/flycheck/flycheck/issues/1755)
  - Error explainers can now return URLs (to show a webpage) or functions (to use custom formatting). For example, the Rust checker now renders explanations using `markdown-view-mode`. [#1753](https://github.com/flycheck/flycheck/issues/1753)
  - Enable checkers in many newer TreeSitter-based major modes (think `*-ts-mode`).
- New syntax checkers
  - Python with `ruff`. [#2033](https://github.com/flycheck/flycheck/issues/2033)
- **Breaking changes**
  - Drop support for Emacs 25.
  - The variable `flycheck-current-errors` now contains errors in the order in which they were returned by checkers. In previous versions of Flycheck, this list was sorted by error position and severity. [#1749](https://github.com/flycheck/flycheck/issues/1749)

## 32 (frozen on May 3rd, 2020, released Mar 28, 2022)

- Highlights
  - Many checkers and compiler, such as `ocaml`, `rust`, `eslint`, and others, include end-line and end-column information. Flycheck can now highlight the exact region that they report. Authors of checker definitions can use the new `:end-line` and `:end-column` arguments in `flycheck-error-new`, or the new `end-line` and `end-column` fields in error patterns. [#1400](https://github.com/flycheck/flycheck/issues/1400)
  - Errors that checkers return for other files will now be displayed on the first line of the current buffer instead of begin discarded. The error list indicates which file each error came from, and navigation moves automatically moves between files. This change helps with compiled languages, where an error in another file may cause the current file to be considered invalid. Variables `flycheck-relevant-error-other-file-show` and `flycheck-relevant-error-other-file-minimum-level` control this behavior. [#1427](https://github.com/flycheck/flycheck/issues/1427)
  - Flycheck can now draw error indicators in margins in addition to fringes. Margins can contain arbitrary characters and images, not just monochrome bitmaps, allowing for a better experience on high-DPI screens. `flycheck-indication-mode` controls this behavior, and `flycheck-set-indication-mode` can be used to automatically adjust the fringes and margins. Additionally, Flycheck's will now use high-resolution fringe bitmaps if the fringe is wide enough \[GH-1742, GH-1744\]
  - Error highlighting is now configurable, using the new `flycheck-highlighting-style` variable: instead of applying level-dependent faces (typically with wavy underlines), Flycheck can now insert delimiters around errors, or mix styles depending on how many lines an error covers. Additionally, stipples are added in the fringes to indicate errors that span multiple lines. [#1743](https://github.com/flycheck/flycheck/issues/1743)
- New features and improvements
  - Flycheck can now trigger a syntax check automatically after switching buffers, using the `idle-buffer-switch` option in `flycheck-check-syntax-automatically`. This is useful when errors in a file are due to problems in a separate file. Variables `flycheck-idle-buffer-switch-delay` and `flycheck-buffer-switch-check-intermediate-buffers` control the functionality. [#1297](https://github.com/flycheck/flycheck/issues/1297)
  - Flycheck will now use Emacs' native XML parsing when libXML fails. This behavior can be changed by customizing `flycheck-xml-parser`. [#1349](https://github.com/flycheck/flycheck/issues/1349)
  - `flycheck-verify-setup` now shows more clearly which checkers will run in the buffer, and which are misconfigured. [#1478](https://github.com/flycheck/flycheck/issues/1478)
  - Flycheck now locates checker executables using a customizable function, `flycheck-executable-find`. The default value of this function allows relative paths (set e.g. in file or dir-local variables) in addition to absolute paths and executable names. [#1485](https://github.com/flycheck/flycheck/issues/1485)
  - Checkers that report error positions as a single offset from the start of the file can use the new `flycheck-error-new-at-pos` constructor instead of converting that position to a line and a column. [#1400](https://github.com/flycheck/flycheck/issues/1400)
  - Config-file variables can now be set to a list of file names. This is useful for checkers like mypy which don't run correctly when called from a subdirectory without passing an explicit config file. [#1711](https://github.com/flycheck/flycheck/issues/1711)
  - Thanks to algorithmic improvements in error reporting, Flycheck is now much faster in large buffers. [#1750](https://github.com/flycheck/flycheck/issues/1750)
- New syntax checkers:
  - Awk with `gawk` [#1708](https://github.com/flycheck/flycheck/issues/1708)
  - Bazel with `buildifier` [#1613](https://github.com/flycheck/flycheck/issues/1613)
  - CUDA with `cuda-nvcc` [#1508](https://github.com/flycheck/flycheck/issues/1508)
  - CWL with `schema-salad-tool` [#1361](https://github.com/flycheck/flycheck/issues/1361)
  - Elixir with `credo` [#1062](https://github.com/flycheck/flycheck/issues/1062)
  - JSON with `json-jq` [#1568](https://github.com/flycheck/flycheck/issues/1568)
  - Jsonnet with `jsonnet` [#1345](https://github.com/flycheck/flycheck/issues/1345)
  - MarkdownLint CLI with `markdownlint` [#1366](https://github.com/flycheck/flycheck/issues/1366)
  - mypy with `python-mypy` [#1354](https://github.com/flycheck/flycheck/issues/1354)
  - Nix with `nix-linter` [#1530](https://github.com/flycheck/flycheck/issues/1530)
  - Opam with `opam lint` [#1532](https://github.com/flycheck/flycheck/issues/1532)
  - protobuf-prototool with `prototool` [#1591](https://github.com/flycheck/flycheck/issues/1591)
  - Rust with `rust-clippy` [#1385](https://github.com/flycheck/flycheck/issues/1385)
  - Ruumba with `eruby-ruumba` [#1616](https://github.com/flycheck/flycheck/issues/1616)
  - Staticcheck with `go-staticcheck` [#1541](https://github.com/flycheck/flycheck/issues/1541)
  - terraform with `terraform fmt`, `tflint` [#1586](https://github.com/flycheck/flycheck/issues/1586)
  - Tcl with `nagelfar` [#1365](https://github.com/flycheck/flycheck/issues/1365)
  - Text prose with `textlint` [#1534](https://github.com/flycheck/flycheck/issues/1534)
  - VHDL with `ghdl` [#1160](https://github.com/flycheck/flycheck/issues/1160)
- Checker improvements:
  - `python-pylint` and `python-flake8` are now invoked with `python -c`, to make it easier to change between Python 2 and Python 3. [#1113](https://github.com/flycheck/flycheck/issues/1113)
  - Add `flycheck-perl-module-list` to use specified modules when syntax checking code with the `perl` checker. [#1207](https://github.com/flycheck/flycheck/issues/1207)
  - `rust-cargo` now uses `cargo check` and `cargo test`. [#1289](https://github.com/flycheck/flycheck/issues/1289)
  - Add `flycheck-ghc-stack-project-file` for the `haskell-stack-ghc` checker. [#1316](https://github.com/flycheck/flycheck/issues/1316)
  - Add `flycheck-cppcheck-suppressions-file` to pass a suppressions file to cppcheck. [#1329](https://github.com/flycheck/flycheck/issues/1329)
  - Add `--force-exclusion` flag to `rubocop` command. [#1348](https://github.com/flycheck/flycheck/issues/1348)
  - Flycheck now uses ESLint's JSON output instead of checkstyle XML. [#1350](https://github.com/flycheck/flycheck/issues/1350)
  - Add `flycheck-eslint-args` to pass arguments to `javascript-eslint`. [#1360](https://github.com/flycheck/flycheck/issues/1360)
  - Flycheck will now execute `rubocop` from the directory where a `Gemfile` is located. If a `Gemfile` does not exist, the old behaviour of running the command from the directory where `.rubocop.yml` is found will be used. [#1368](https://github.com/flycheck/flycheck/issues/1368)
  - Add `flycheck-sh-bash-args` to pass arguments to `sh-bash`. [#1439](https://github.com/flycheck/flycheck/issues/1439)
  - `haskell-stack-ghc` will not try to install GHC anymore. [#1443](https://github.com/flycheck/flycheck/issues/1443)
  - Add `flycheck-ghdl-ieee-library` to select which standard IEEE library to use for ghdl. [#1547](https://github.com/flycheck/flycheck/issues/1547)
  - The `javascript-eslint` checker now supports `typescript-mode` by default.
  - Add `flycheck-erlang-rebar3-profile` to select which profile to use when compiling erlang with rebar3. [#1560](https://github.com/flycheck/flycheck/issues/1560)
  - Add `flycheck-relevant-error-other-file-show` to avoid showing errors from other files. [#1579](https://github.com/flycheck/flycheck/issues/1579)
  - The `nix-linter` checker now has an error explainer. [#1586](https://github.com/flycheck/flycheck/issues/1586)
  - The Emacs Lisp checker can now run in buffers not backed by files. [#1695](https://github.com/flycheck/flycheck/issues/1695)
- **Breaking changes**
  - Remove the `javascript-jscs` checker. [#1024](https://github.com/flycheck/flycheck/issues/1024)
  - Remove the `elixir-dogma` checker. [#1450](https://github.com/flycheck/flycheck/issues/1450)
  - `rust-cargo` now requires Rust 1.17 or newer. [#1289](https://github.com/flycheck/flycheck/issues/1289)
  - `rust` now requires 1.18 or newer. [#1501](https://github.com/flycheck/flycheck/issues/1501)
  - Rename `flycheck-cargo-rustc-args` to `flycheck-cargo-check-args`. [#1289](https://github.com/flycheck/flycheck/issues/1289)
  - `rust-cargo` does not use the variable `flycheck-rust-args` anymore. [#1289](https://github.com/flycheck/flycheck/issues/1289)
  - Improve detection of default directory for `haskell-ghc` to consider `hpack` project files. [#1435](https://github.com/flycheck/flycheck/issues/1435)
  - Replace `go tool vet` with `go vet`. [#1548](https://github.com/flycheck/flycheck/issues/1548)
  - Remove the deprecated `go-megacheck` checker, which is replaced by `go-staticcheck`. [#1583](https://github.com/flycheck/flycheck/issues/1583)

## 31 (Oct 07, 2017)

- **Breaking changes**
  - `rust-cargo` now requires Rust 1.15 or newer [#1201](https://github.com/flycheck/flycheck/issues/1201)
  - Remove javascript-gjslint checker
- New syntax checkers:
  - Protobuf with `protoc` [#1125](https://github.com/flycheck/flycheck/issues/1125)
  - systemd-analyze with `systemd-analyze` [#1135](https://github.com/flycheck/flycheck/issues/1135)
  - Nix with `nix-instantiate` [#1164](https://github.com/flycheck/flycheck/issues/1164)
  - Dockerfile with `hadolint` [#1194](https://github.com/flycheck/flycheck/issues/1194)
  - AsciiDoc with `asciidoctor` [#1167](https://github.com/flycheck/flycheck/issues/1167)
  - CSS/SCSS/LESS with `stylelint` [#903](https://github.com/flycheck/flycheck/issues/903)
  - Ruby with `reek` [#1244](https://github.com/flycheck/flycheck/issues/1244)
  - Go with `megacheck` [#1290](https://github.com/flycheck/flycheck/issues/1290)
  - LLVM IR with `llc` [#1302](https://github.com/flycheck/flycheck/issues/1302)
  - Text prose with `proselint` [#1304](https://github.com/flycheck/flycheck/issues/1304)
- New features:
  - Add `flycheck-xml-xmlstarlet-xsd-path` and `flycheck-xml-xmllint-xsd-path` to specify an XSD schema to validate XML documents against [#1272](https://github.com/flycheck/flycheck/issues/1272)
  - Add `flycheck-tslint-args` to pass additional arguments to tslint [#1186](https://github.com/flycheck/flycheck/issues/1186)
  - Add an error explainer to the `rpm-rpmlint` checker using `rpmlint -I` [#1235](https://github.com/flycheck/flycheck/issues/1235)
  - Add `flycheck-emacs-lisp-check-declare` to check function declaration in the `emacs-lisp` checker [#1286](https://github.com/flycheck/flycheck/issues/1286)
  - Add `flycheck-shellcheck-follow-sources` to check included files when using the `sh-shellcheck` checker [#1256](https://github.com/flycheck/flycheck/issues/1256)
- Improvements:
  - Use option `flycheck-go-build-tags` for `go-test`, `go-vet` and `go-errcheck` as well.
  - Add a revert function to `flycheck-verify-setup`, so hitting `g` reloads the buffer.
  - Make sure the erlang compiler is only run on compilable files.
  - `flycheck-tslint` does not crash any more on deprecation notices [#1174](https://github.com/flycheck/flycheck/issues/1174)
  - `rust-cargo` now checks integration tests, examples and benchmarks [#1206](https://github.com/flycheck/flycheck/issues/1206)
  - `rust-cargo` does not use `flycheck-rust-library-path` anymore, as dependencies are taken care of by Cargo [#1206](https://github.com/flycheck/flycheck/issues/1206)
  - `c/c++-gcc` checker now works from GCC 4.4 and up [#1226](https://github.com/flycheck/flycheck/issues/1226)

## 30 (Oct 12, 2016)

- **Breaking changes**
  - Flycheck now requires flake8 3.0 or newer
  - Remove `--config` option in `lua-luacheck` in favour of `luacheck`'s own `.luacheckrc` detection. Therefore `flycheck-luacheckrc` is no longer used [#1057](https://github.com/flycheck/flycheck/issues/1057)
  - `:modes` is now mandatory for syntax checker definitions [#1071](https://github.com/flycheck/flycheck/issues/1071)
  - Remove jade checker [#951](https://github.com/flycheck/flycheck/issues/951) [#1084](https://github.com/flycheck/flycheck/issues/1084)
  - Remove `javascript-eslintrc` and instead rely on eslint's own configuration file search [#1085](https://github.com/flycheck/flycheck/issues/1085)
  - `C-c ! e` explains errors now [#1122](https://github.com/flycheck/flycheck/issues/1122)
- New syntax checkers:
  - Elixir with `dogma` [#969](https://github.com/flycheck/flycheck/issues/969)
  - sass and scss with `sass-lint` [#1070](https://github.com/flycheck/flycheck/issues/1070)
  - Pug [#951](https://github.com/flycheck/flycheck/issues/951) [#1084](https://github.com/flycheck/flycheck/issues/1084)
- New features:
  - Add `flycheck-cargo-rustc-args` to pass multiple arguments to cargo rustc subcommand [#1079](https://github.com/flycheck/flycheck/issues/1079)
  - Add `:error-explainer` to `flycheck-define-checker` and `flycheck-explain-error-at-point` to display explanations of errors [#1122](https://github.com/flycheck/flycheck/issues/1122)
  - Add an error explainer to the `rust` and `rust-cargo` checkers using `rustc --explain` [#1122](https://github.com/flycheck/flycheck/issues/1122)
  - Add `:enabled` property to `flycheck-define-checker` [#1089](https://github.com/flycheck/flycheck/issues/1089)
- Improvements:
  - Do not use `javascript-eslint` if eslint cannot find a valid configuration [#1085](https://github.com/flycheck/flycheck/issues/1085)
  - Automatically disable syntax checkers which are not installed instead of checking executable before each syntax check [#1116](https://github.com/flycheck/flycheck/issues/1116)
  - Add patterns for syntax errors to `scheme-chicken` [#1123](https://github.com/flycheck/flycheck/issues/1123)

## 29 (Aug 28, 2016)

- **Breaking changes**
  - Change `flycheck-eslint-rulesdir` (string) to `flycheck-eslint-rules-directories` (list of strings) [#1016](https://github.com/flycheck/flycheck/issues/1016)
  - Require rust 1.7 or newer for `rust` and `rust-cargo` [#1036](https://github.com/flycheck/flycheck/issues/1036)
- New syntax checkers:
  - Slim with `slim-lint` [#1013](https://github.com/flycheck/flycheck/issues/1013)
  - CHICKEN Scheme with `csc` [#987](https://github.com/flycheck/flycheck/issues/987)
- New features:
  - Add `:working-directory` option to `flycheck-define-command-checker` [#973](https://github.com/flycheck/flycheck/issues/973) [#1012](https://github.com/flycheck/flycheck/issues/1012)
  - `flycheck-go-build-install-deps` turns on dependency installation for `go test` as well as `go build` [#1003](https://github.com/flycheck/flycheck/issues/1003)
- Improvements:
  - Add default directory for `haskell-stack-ghc` and `haskell-ghc` checkers [#1007](https://github.com/flycheck/flycheck/issues/1007)
  - `rust` and `rust-cargo` checkers now support the new error format of rust 1.12 [#1016](https://github.com/flycheck/flycheck/issues/1016)
  - `flycheck-verify-checker` and `flycheck-verify-setup` now include information about configuration files of syntax checkers [#1021](https://github.com/flycheck/flycheck/issues/1021) [#1038](https://github.com/flycheck/flycheck/issues/1038)

## 28 (Jun 05, 2016)

- **Breaking changes**:
  - Rename `luacheck` to `lua-luacheck` to comply with our naming conventions
  - Remove `flycheck-cppcheck-language-standard` in favour of `flycheck-cppcheck-standards` which is a list of standards [#960](https://github.com/flycheck/flycheck/issues/960)
- New features:
  - Add option to set binary name for `rust-cargo` [#958](https://github.com/flycheck/flycheck/issues/958)
  - Add `flycheck-cppcheck-standards` to pass multiple code standards to cppcheck [#960](https://github.com/flycheck/flycheck/issues/960)
  - Add `flycheck-cppcheck-suppressions` to suppress warnings for cppcheck [#960](https://github.com/flycheck/flycheck/issues/960)
- Improvements:
  - Check Racket syntax in Geiser Mode [#979](https://github.com/flycheck/flycheck/issues/979)
- Bug fixes
  - Do not signal errors when tslint reports no output [#981](https://github.com/flycheck/flycheck/issues/981)
  - Do not generate invalid temporary filenames on Windows [#983](https://github.com/flycheck/flycheck/issues/983)

## 27 (May 08, 2016)

- **Breaking changes**
  - Require PHP Code Sniffer 2.6 or newer for `php-phpcs` [#921](https://github.com/flycheck/flycheck/issues/921)
- New syntax checkers:
  - Go with `go-unconvert` [#905](https://github.com/flycheck/flycheck/issues/905)
  - Markdown with `mdl` [#839](https://github.com/flycheck/flycheck/issues/839) [#916](https://github.com/flycheck/flycheck/issues/916)
  - TypeScript with `tslint` [#947](https://github.com/flycheck/flycheck/issues/947) [#949](https://github.com/flycheck/flycheck/issues/949)
- Improvements:
  - Pass checkdoc settings from Emacs to `emacs-lisp-checkdoc` [#741](https://github.com/flycheck/flycheck/issues/741) [#937](https://github.com/flycheck/flycheck/issues/937)
- Bug fixes:
  - Fix parsing of syntax errors in triple-quoted strings for `python-pycompile` [#948](https://github.com/flycheck/flycheck/issues/948)
  - Correctly handle rules based on the current file name in `php-phpcs` [#921](https://github.com/flycheck/flycheck/issues/921)

## 26 (Apr 27, 2016)

Flycheck now has a [Code of Conduct](https://www.flycheck.org/en/latest/community/conduct.html) which defines the acceptable behaviour and the moderation guidelines for the Flycheck community. [#819](https://github.com/flycheck/flycheck/issues/819)

Flycheck also provides a [Gitter channel](https://gitter.im/flycheck/flycheck) now for questions and discussions about development. [#820](https://github.com/flycheck/flycheck/issues/820)

The native Texinfo manual is again replaced with a [Sphinx](https://sphinx-doc.org) based documentation. We hope that this change makes the manual easier to edit and to maintain and more welcoming for new contributors. The downside is that we can not longer include a Info manual in Flycheck’s MELPA packages.

From this release onward Flycheck will use a single continuously increasing version number. Breaking changes may occur at any point.

- **Breaking changes**:
  - Remove `flycheck-copy-messages-as-kill`, obsolete since Flycheck 0.22
  - Remove `flycheck-perlcritic-verbosity`, obsolete since Flycheck 0.22
  - Replace `flycheck-completion-system` with `flycheck-completing-read-function` [#870](https://github.com/flycheck/flycheck/issues/870)
  - JSON syntax checkers now require `json-mode` and do not check in Javascript Mode anymore
  - Prefer eslint over jshint for Javascript
  - Obsolete `flycheck-info` in favour of the new `flycheck-manual` command
- New syntax checkers:
  - Processing [#793](https://github.com/flycheck/flycheck/issues/793) [#812](https://github.com/flycheck/flycheck/issues/812)
  - Racket [#799](https://github.com/flycheck/flycheck/issues/799) [#873](https://github.com/flycheck/flycheck/issues/873)
- New features:
  - Add `flycheck-puppet-lint-rc` to customise the location of the puppetlint configuration file [#846](https://github.com/flycheck/flycheck/issues/846)
  - Add `flycheck-puppet-lint-disabled-checks` to disable specific checks of puppetlint [#824](https://github.com/flycheck/flycheck/issues/824)
  - New library `flycheck-buttercup` to support writing [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) specs for Flycheck
  - Add `flycheck-perlcriticrc` to set a configuration file for Perl::Critic [#851](https://github.com/flycheck/flycheck/issues/851)
  - Add `flycheck-jshint-extract-javascript` to extract Javascript from HTML [#825](https://github.com/flycheck/flycheck/issues/825)
  - Add `flycheck-cppcheck-language-standard` to set the language standard for cppcheck [#862](https://github.com/flycheck/flycheck/issues/862)
  - Add `flycheck-mode-line-prefix` to customise the prefix of Flycheck’s mode line lighter [#879](https://github.com/flycheck/flycheck/issues/879) [#880](https://github.com/flycheck/flycheck/issues/880)
  - Add `flycheck-go-vet-shadow` to check for shadowed variables with `go vet` [#765](https://github.com/flycheck/flycheck/issues/765) [#897](https://github.com/flycheck/flycheck/issues/897)
  - Add `flycheck-ghc-stack-use-nix` to enable Nix support for Stack GHC [#913](https://github.com/flycheck/flycheck/issues/913)
- Improvements:
  - Map error IDs from flake8-pep257 to Flycheck error levels
  - Explicitly display errors at point with `C-c ! h` [#834](https://github.com/flycheck/flycheck/issues/834)
  - Merge message and checker columns in the error list to remove redundant ellipsis [#828](https://github.com/flycheck/flycheck/issues/828)
  - Indicate disabled checkers in verification buffers [#749](https://github.com/flycheck/flycheck/issues/749)
  - Do not enable Flycheck Mode in `fundamental-mode` buffers [#883](https://github.com/flycheck/flycheck/issues/883)
  - Write `go test` output to a temporary files [#887](https://github.com/flycheck/flycheck/issues/887)
  - Check whether `lintr` is actually installed [#911](https://github.com/flycheck/flycheck/issues/911)
- Bug fixes:
  - Fix folding of C/C++ errors from included files [#783](https://github.com/flycheck/flycheck/issues/783)
  - Fix verification of SCSS-Lint checkstyle reporter
  - Don’t fall back to `rust` if `rust-cargo` should be used [#817](https://github.com/flycheck/flycheck/issues/817)
  - Don’t change current buffer when closing the error message buffer [#648](https://github.com/flycheck/flycheck/issues/648)
  - Never display error message buffer in current window [#822](https://github.com/flycheck/flycheck/issues/822)
  - Work around a caching issue in Rubocop [#844](https://github.com/flycheck/flycheck/issues/844)
  - Fix checkdoc failure with some Emacs Lisp syntax [#833](https://github.com/flycheck/flycheck/issues/833) [#845](https://github.com/flycheck/flycheck/issues/845) [#898](https://github.com/flycheck/flycheck/issues/898)
  - Correctly parse Haskell module name with exports right after the module name [#848](https://github.com/flycheck/flycheck/issues/848)
  - Don’t hang when sending buffers to node.js processes on Windows [#794](https://github.com/flycheck/flycheck/issues/794)[#850](https://github.com/flycheck/flycheck/issues/850)
  - Parse suggestions from `hlint` [#874](https://github.com/flycheck/flycheck/issues/874)
  - Go errcheck handles multiple `$GOPATH` entries correctly now [#580](https://github.com/flycheck/flycheck/issues/580)[#906](https://github.com/flycheck/flycheck/issues/906)
  - Properly handle Go build failing in a directory with multiple packages [#676](https://github.com/flycheck/flycheck/issues/676) [#904](https://github.com/flycheck/flycheck/issues/904)
  - Make cppcheck recognise C++ header files [#909](https://github.com/flycheck/flycheck/issues/909)
  - Don’t run phpcs on empty buffers [#907](https://github.com/flycheck/flycheck/issues/907)

## 0.25.1 (Nov 16, 2015)

- Fix undefined function call in `emacs-lisp` syntax checker [#791](https://github.com/flycheck/flycheck/issues/791)

## 0.25 (Nov 14, 2015)

- **Breaking changes**:
  - `scala-scalastyle` now expects a `scalastyle` executable in `exec-path` [#763](https://github.com/flycheck/flycheck/issues/763)
  - Drop support for legacy Ruby YAML implementations prior to Ruby 1.9.3 in `ruby-yaml`
  - Remove racket syntax checker due to possible code execution [#786](https://github.com/flycheck/flycheck/issues/786)
- New syntax checkers:
  - JSON with Python’s built-in `json` module [#758](https://github.com/flycheck/flycheck/issues/758)
  - Rust with `cargo rustc` [#772](https://github.com/flycheck/flycheck/issues/772)
- New features:
  - Add `flycheck-help-echo-function` to customize the Help Echo of Flycheck overlays [#730](https://github.com/flycheck/flycheck/issues/730)
  - Use symbolic error IDs in Pylint [#714](https://github.com/flycheck/flycheck/issues/714)
  - Add `flycheck-pylint-use-symbolic-id` to disable symbolic IDs for Pylint [#714](https://github.com/flycheck/flycheck/issues/714)
  - Add `flycheck-command-wrapper-function` to wrap syntax checker commands before execution [#629](https://github.com/flycheck/flycheck/issues/629) [#752](https://github.com/flycheck/flycheck/issues/752)
  - Add `flycheck-executable-find` to customise how Flycheck searches executables [#752](https://github.com/flycheck/flycheck/issues/752)
  - Add `flycheck-hlint-args` to pass additional arguments to hlint [#713](https://github.com/flycheck/flycheck/issues/713)[#762](https://github.com/flycheck/flycheck/issues/762)
  - Add `flycheck-go-build-tags` and `flycheck-go-install-deps` to specify tags and automatically install dependencies with `go build` [#674](https://github.com/flycheck/flycheck/issues/674)
  - Add :standard-input property to syntax checkers to read source from standard input [#673](https://github.com/flycheck/flycheck/issues/673) [#728](https://github.com/flycheck/flycheck/issues/728)
  - Add support for JSX modes to some Javascript checkers [#778](https://github.com/flycheck/flycheck/issues/778)
- Improvements:
  - Improve mode line display for buffers with only info messages [#733](https://github.com/flycheck/flycheck/issues/733)
  - Merge messages of all errors under cursor for Help Echo [#730](https://github.com/flycheck/flycheck/issues/730)
  - Align multi-line messages in error list [#732](https://github.com/flycheck/flycheck/issues/732) [#731](https://github.com/flycheck/flycheck/issues/731)
  - Cache GHC output for faster syntax checking and better template haskell support [#708](https://github.com/flycheck/flycheck/issues/708)
  - Fall back to `rst` if `rst-sphinx` is disabled [#745](https://github.com/flycheck/flycheck/issues/745) [#746](https://github.com/flycheck/flycheck/issues/746)
  - New uniform fringe indicators [#750](https://github.com/flycheck/flycheck/issues/750)
  - Demote AsciiDoc deprecation warnings to `info` level
- Bug fixes:
  - Fix error patterns of `coq` [#742](https://github.com/flycheck/flycheck/issues/742)
  - Support GFortran 5 [#751](https://github.com/flycheck/flycheck/issues/751)
  - Fix stack overflow when parsing hlint errors [#760](https://github.com/flycheck/flycheck/issues/760)
  - Fix error columns of jsonlint

## 0.24 (Aug 15, 2015)

- **Breaking changes**:
  - Remove Elixir syntax checker due to code execution [#630](https://github.com/flycheck/flycheck/issues/630)
  - Drop support for Emacs 24.1 and 24.2
- New syntax checkers:
  - Javascript with `jscs` [#634](https://github.com/flycheck/flycheck/issues/634) and `standard` [#644](https://github.com/flycheck/flycheck/issues/644)
  - Jade [#686](https://github.com/flycheck/flycheck/issues/686)
  - SQL with `sqllint` [#691](https://github.com/flycheck/flycheck/issues/691)
  - Groovy [#716](https://github.com/flycheck/flycheck/issues/716)
  - Haskell with `stack ghc` [#711](https://github.com/flycheck/flycheck/issues/711)
- New features:
  - The error list can now be filtered by error level by pressing f
  - Add `flycheck-error-list-minimum-level` to restrict error levels displayed in the error list [#698](https://github.com/flycheck/flycheck/issues/698) [#701](https://github.com/flycheck/flycheck/issues/701)
  - Add `flycheck-perl-include-path` to set include directories for Perl [#621](https://github.com/flycheck/flycheck/issues/621)
  - Add `flycheck-rust-args` to pass additional arguments to `rustc`
  - Add `flycheck-dmd-args` to pass additional arguments to `dmd` [#655](https://github.com/flycheck/flycheck/issues/655)
  - Add `flycheck-erlang-include-path` [#668](https://github.com/flycheck/flycheck/issues/668) and `flycheck-erlang-library-path` [#696](https://github.com/flycheck/flycheck/issues/696) for Erlang
  - Add `flycheck-verilator-include-path` to set include directories for Verilator [#684](https://github.com/flycheck/flycheck/issues/684)
  - Add `flycheck-cppcheck-include-path` to set include directories for cppcheck [#687](https://github.com/flycheck/flycheck/issues/687)
  - Add support for Hlint configuration file [#682](https://github.com/flycheck/flycheck/issues/682)
  - Add Hlint options for ignore rules, language extensions and hint packages [#682](https://github.com/flycheck/flycheck/issues/682)
- Improvements:
  - Show chained checkers in Help buffers for syntax checkers [#571](https://github.com/flycheck/flycheck/issues/571)
  - Map custom error levels to compilation mode levels [#700](https://github.com/flycheck/flycheck/issues/700)
  - `flycheck-verify-setup` now includes the manually selected checker if any [#705](https://github.com/flycheck/flycheck/issues/705)
  - `flycheck-select-checker` now shows a verification buffer if the selected checker cannot be used [#705](https://github.com/flycheck/flycheck/issues/705)
  - Add `flycheck-verify-checker` to check whether a specific syntax checker can be used in a buffer [#705](https://github.com/flycheck/flycheck/issues/705)
- Bug fixes:
  - Fix offset of column numbers in ESLint [#640](https://github.com/flycheck/flycheck/issues/640)
  - Properly parse indentation errors from Python 2.7 [#635](https://github.com/flycheck/flycheck/issues/635)
  - Don’t choke if `default-directory` does not exist [#625](https://github.com/flycheck/flycheck/issues/625)
  - Fix error parsing for Puppet 4
  - Fix duplicate checkdoc errors on Emacs 25
  - Fix level of `info` messages in `flycheck-compile` [#669](https://github.com/flycheck/flycheck/issues/669)
  - Allow custom `:verify` functions for command checkers [#672](https://github.com/flycheck/flycheck/issues/672)
  - Fix error when `flycheck-scalastylerc` was set to a non-existing file
  - Fix error column offsets in `scala-scalastyle`
  - Do not use `r-lintr` in non-R buffers [#607](https://github.com/flycheck/flycheck/issues/607)
  - Enforce output format of `flake8` [#704](https://github.com/flycheck/flycheck/issues/704)
  - Parse error ids from luacheck 0.11
  - Fix patterns for Puppet environment names [#694](https://github.com/flycheck/flycheck/issues/694)
  - Properly locate configuration files from jshint and jscs [#703](https://github.com/flycheck/flycheck/issues/703)
  - Fix column offsets in eslint 1.0 [#718](https://github.com/flycheck/flycheck/issues/718)

## 0.23 (Apr 6, 2015)

- **Breaking changes**:
  - New manual in native Texinfo format, to achieve higher quality Info manuals
  - Remove `make` syntax checker due to various issues [#572](https://github.com/flycheck/flycheck/issues/572) [#573](https://github.com/flycheck/flycheck/issues/573)
  - Remove `zsh` support from `sh-shellcheck`, since Shellcheck does not support Zsh anymore
  - Remove `global-flycheck-mode` from customization interface [#595](https://github.com/flycheck/flycheck/issues/595)
- New syntax checkers:
  - R with `lintr` [#512](https://github.com/flycheck/flycheck/issues/512)
  - Lua with `luacheck` [#591](https://github.com/flycheck/flycheck/issues/591) [#609](https://github.com/flycheck/flycheck/issues/609)
  - SCSS with `scss-lint` [#582](https://github.com/flycheck/flycheck/issues/582) [#598](https://github.com/flycheck/flycheck/issues/598)
- New features:
  - Add `flycheck-disable-checker` to disable a syntax checker in the current buffer
  - Add `flycheck-global-modes` to control in which modes `global-flycheck-mode` turns on `flycheck-mode`
  - Add `pedantic` and `pedantic-errors` options to Clang and GCC [#543](https://github.com/flycheck/flycheck/issues/543)
  - Add `flycheck-foodcritic-tags` to select tags for Foodcritic [#560](https://github.com/flycheck/flycheck/issues/560)
- Improvements:
  - `chef-foodcritic` handles relative paths correctly now [#556](https://github.com/flycheck/flycheck/issues/556)
  - Global Flycheck Mode enables Flycheck Mode even if there is no syntax checker for the buffer yet [#568](https://github.com/flycheck/flycheck/issues/568)
  - `handlebars` now supports Web Mode [#605](https://github.com/flycheck/flycheck/issues/605)
  - Extract error IDs from `rustc`
  - Don’t cache last syntax checker in buffer anymore
- Bug fixes:
  - Fix void variable error when trying to use `flycheck-compile` with a non-command checker [#563](https://github.com/flycheck/flycheck/issues/563)
  - Fix faulty mode line reporting [#564](https://github.com/flycheck/flycheck/issues/564)
  - Automatically initialize packages when checking `user-init-file`
  - Properly initialize hook variables [#593](https://github.com/flycheck/flycheck/issues/593)
  - Fix handling of file names with symbolic links for some checkers [#561](https://github.com/flycheck/flycheck/issues/561)
  - Parse multiline type errors from `rustc` [#592](https://github.com/flycheck/flycheck/issues/592)

## 0.22 (Dec 23, 2014)

- **Breaking changes**:
  - Never allow use of disabled checkers anymore, even with `flycheck-select-checker`
  - Error parsers **must** set the `:buffer` and `:checker` slots of `flycheck-error` now
  - The internals of syntax checker definitions have changed again. **All packages depending on Flycheck must be recompiled!** [#524](https://github.com/flycheck/flycheck/issues/524)
  - `flycheck-error-list-refresh` is not an interactive command anymore
  - Replace `flycheck-perlcritic-verbosity` with `flycheck-perlcritic-severity`
  - Replace `flycheck-copy-messages-as-kill` with `flycheck-copy-errors-as-kill` [#529](https://github.com/flycheck/flycheck/issues/529)
  - Remove `flycheck-google-messages` command
  - Options and config file variables are not buffer-local anymore [#546](https://github.com/flycheck/flycheck/issues/546)
- New syntax checkers:
  - Python with `py_compile` [#484](https://github.com/flycheck/flycheck/issues/484)
- New features:
  - `flycheck-ert.el` library to write unit tests for Flycheck extensions
  - Add `flycheck-define-generic-checker` to define syntax checkers over arbitrary Emacs Lisp functions [#169](https://github.com/flycheck/flycheck/issues/169) [#524](https://github.com/flycheck/flycheck/issues/524)
  - Add `flycheck-define-command-checker` as non-macro variant of `flycheck-define-checker` [#524](https://github.com/flycheck/flycheck/issues/524)
  - Add support for IDs of errors [#529](https://github.com/flycheck/flycheck/issues/529)
  - Add special `id` sexp to parse error ids with `:error-patterns` [#529](https://github.com/flycheck/flycheck/issues/529)
  - Parse error IDs from Checkstyle XML [#259](https://github.com/flycheck/flycheck/issues/259)
  - `flycheck-copy-errors-as-kill` can put error ids into kill ring now [#529](https://github.com/flycheck/flycheck/issues/529)
  - Parse error IDs from many error checkers [#259](https://github.com/flycheck/flycheck/issues/259)
  - Verify Flycheck setup in a buffer with `flycheck-verify-setup` [#338](https://github.com/flycheck/flycheck/issues/338)
  - Add options for arbitrary arguments to some syntax checkers [#542](https://github.com/flycheck/flycheck/issues/542)
  - Add `flycheck-flake8-error-level-alist` to customize error levels from flake8 [#454](https://github.com/flycheck/flycheck/issues/454)
- Improvements:
  - Automatically disable syntax checkers that report too many errors [#476](https://github.com/flycheck/flycheck/issues/476)
  - Reduce filesystem access when parsing errors to improve parsing speed
  - Add explicit `load-path` inheritance to `flycheck-emacs-lisp-load-path`, via new `inherit` value [#511](https://github.com/flycheck/flycheck/issues/511)
  - Parse help messages from `rustc` [#517](https://github.com/flycheck/flycheck/issues/517)
  - `g` in the error list checks the source buffer again [#532](https://github.com/flycheck/flycheck/issues/532)
  - `haskell-ghc` supports literate Haskell now [#535](https://github.com/flycheck/flycheck/issues/535)
- Bug fixes:
  - Properly parse notes in `sh-shellcheck` [#508](https://github.com/flycheck/flycheck/issues/508)
  - Fix shell quoting in `flycheck-compile` [#522](https://github.com/flycheck/flycheck/issues/522) [#523](https://github.com/flycheck/flycheck/issues/523)
  - Fix faulty properties of customize options which broke `customize-changed` and related functions
  - Fix use deprecated option in `coffee-coffeelint`
  - Fix error columns of `python-pylint` and `tex-chktex` [#536](https://github.com/flycheck/flycheck/issues/536)
  - Correctly compute error level of errors on included files in `c/c++-clang` and `c/c++-gcc` [#451](https://github.com/flycheck/flycheck/issues/451)

## 0.21 (Oct 26, 2014)

- **Breaking changes**:
  - `html-tidy` is not enabled in Web Mode anymore [#464](https://github.com/flycheck/flycheck/issues/464)
  - `d-dmd` now requires DMD 2.066 or newer [#460](https://github.com/flycheck/flycheck/issues/460)
  - `:next-checkers` now requires the maximum permissible level instead of a custom predicate [#472](https://github.com/flycheck/flycheck/issues/472)
  - Remove `flycheck-error-list-highlight-at-point` face and related functionality [#490](https://github.com/flycheck/flycheck/issues/490)
- New syntax checkers:
  - Coq
  - RPM spec files with `rpmlint` [#480](https://github.com/flycheck/flycheck/issues/480) [#481](https://github.com/flycheck/flycheck/issues/481)
- New features:
  - Add `null-device` symbol for syntax checker commands
  - Add `flycheck-display-error-messages-unless-error-list` for `flycheck-error-display-function`
  - Add `flycheck-error-list-after-refresh-hook` to run after the error list refreshes
  - Add `flycheck-navigation-minimum-level` to restrict error levels available for navigation [#398](https://github.com/flycheck/flycheck/issues/398) [#485](https://github.com/flycheck/flycheck/issues/485)
  - The error list can be sorted by message and syntax checker name now [#500](https://github.com/flycheck/flycheck/issues/500)
  - Add `flycheck-error-list-checker-name` face to customize the appearance of the syntax checker name in the error list [#500](https://github.com/flycheck/flycheck/issues/500)
  - Add `flycheck-shellcheck-excluded-warnings` to exclude warnings from ShellCheck reports [#499](https://github.com/flycheck/flycheck/issues/499)
  - Add `flycheck-add-mode` to add a new major mode to a syntax checker [#506](https://github.com/flycheck/flycheck/issues/506)
  - Add `flycheck-gcc-openmp` to enable OpenMP for GCC in C/C++ [#507](https://github.com/flycheck/flycheck/issues/507)
- Improvements:
  - Improve GCC syntax checking by expanding templates [#459](https://github.com/flycheck/flycheck/issues/459)
  - `d-dmd` reports errors with columns now [#460](https://github.com/flycheck/flycheck/issues/460)
  - Remove Projectile-based config file search [#461](https://github.com/flycheck/flycheck/issues/461)
  - Do not change point when navigating in the error list [#487](https://github.com/flycheck/flycheck/issues/487)
  - ShellCheck warnings now include the corresponding warning code
- Bug fixes:
  - Expand `default-directory` before using it, to handle abbreviated paths gracefully [#434](https://github.com/flycheck/flycheck/issues/434)
  - Restore mouse support in the error list [#468](https://github.com/flycheck/flycheck/issues/468)
  - `less` now correctly resolves relative paths in `data-uri` [#471](https://github.com/flycheck/flycheck/issues/471)
  - `go-errcheck` now properly uses package names as syntax checker arguments
  - `c/c++-clang` now handles empty error messages [#497](https://github.com/flycheck/flycheck/issues/497)

## 0.20 (Aug 12, 2014)

- **Breaking changes**:
  - The internal names of syntax checker properties changed. **All packages depending on Flycheck must be recompiled!**
  - `flycheck-substitute-argument` always returns a list now
  - The special meaning of a trailing `=` in `(option …)` and `(config-file …)` is removed. Both arguments must now explicitly specify `concat` to prepend the option as string.
- New syntax checkers:
  - C/C++ with GCC [#408](https://github.com/flycheck/flycheck/issues/408)
  - Scala with scalastyle [#425](https://github.com/flycheck/flycheck/issues/425)
  - Fortran with GFortran [#414](https://github.com/flycheck/flycheck/issues/414) [#450](https://github.com/flycheck/flycheck/issues/450)
  - Ada with GNAT [#414](https://github.com/flycheck/flycheck/issues/414) [#457](https://github.com/flycheck/flycheck/issues/457)
- New features:
  - Add `flycheck-clang-no-exceptions` and `flycheck-gcc-no-exceptions` to flag exceptions as errors in C++ [#412](https://github.com/flycheck/flycheck/issues/412)
  - Add `flycheck-rust-crate-root` to resolve inter-crate references in `rust` [#417](https://github.com/flycheck/flycheck/issues/417)
  - Add `flycheck-clang-blocks` to enable the block syntax in Clang [#420](https://github.com/flycheck/flycheck/issues/420)
  - `read-flycheck-checker` now accepts a default value
  - Add `flycheck-status-changed-functions` to react on status changes
  - Make the mode line lighter of Flycheck customizable with `flycheck-mode-line`
  - Add `flycheck-rubylintrc` to support configuration files for `ruby-rubylint` [#424](https://github.com/flycheck/flycheck/issues/424)
  - Add `flycheck-rust-crate-type` to make the Crate type customizable [#446](https://github.com/flycheck/flycheck/issues/446)
  - The mode line of the error list is now customizable with `flycheck-error-list-mode-line` [#454](https://github.com/flycheck/flycheck/issues/454)
  - Pressing `n` or `p` in the error list now shows the error at point in a separate window [#452](https://github.com/flycheck/flycheck/issues/452) [#454](https://github.com/flycheck/flycheck/issues/454)
  - Pressing `RET` in the error list now jumps to the error at point [#454](https://github.com/flycheck/flycheck/issues/454)
  - The error list can now be sorted by error level by clicking on the corresponding list header, or by pressing `S` with point on the column text [#454](https://github.com/flycheck/flycheck/issues/454)
  - Error levels defined with `flycheck-define-error-level` can now have a numeric severity used for sorting [#454](https://github.com/flycheck/flycheck/issues/454)
- Improvements:
  - Use proper temporary files in `python-flake8` [#421](https://github.com/flycheck/flycheck/issues/421)
  - Demote errors from `package-initialize` in the `emacs-lisp` checker [#423](https://github.com/flycheck/flycheck/issues/423)
  - `flycheck-select-checker` now uses the last used syntax checker as default when reading from minibuffer
  - `flycheck-compile` now prompts for the syntax checker to run as `compile` command [#428](https://github.com/flycheck/flycheck/issues/428)
  - The `rust` syntax checker shows info messages now [#439](https://github.com/flycheck/flycheck/issues/439)
  - The `sass` and `scss` syntax checkers now use a temporary directory for their cache [#443](https://github.com/flycheck/flycheck/issues/443) [#454](https://github.com/flycheck/flycheck/issues/454)
  - Change the default of `flycheck-eslintrc` to `nil` [#447](https://github.com/flycheck/flycheck/issues/447)
  - Show the menu on the mode line lighter [#365](https://github.com/flycheck/flycheck/issues/365)
  - Greatly improve Flycheck's menu
  - `n` and `p` now navigate the error list by errors, not by lines [#452](https://github.com/flycheck/flycheck/issues/452)[#444](https://github.com/flycheck/flycheck/issues/444)
  - `c/c++-clang` does not use in-place temporary files anymore [#456](https://github.com/flycheck/flycheck/issues/456)
- Bug fixes:
  - Properly support `unload-feature` now
- Other changes:
  - Remove dependencies on f.el and s.el

## 0.19 (Jun 12, 2014)

- Flycheck now has an official logo [#331](https://github.com/flycheck/flycheck/issues/331)
- **Breaking changes**:
  - The `ruby-rubylint` syntax checker now requires Ruby Lint 2.0 or newer. [#405](https://github.com/flycheck/flycheck/issues/405)
- New syntax checkers:
  - Go with `errcheck` [#393](https://github.com/flycheck/flycheck/issues/393)
- New features:
  - Add `flycheck-keymap-prefix` to change the prefix key for Flycheck keybindings [#381](https://github.com/flycheck/flycheck/issues/381)
  - Make the prefix of Flycheck's temporary files customizable with `flycheck-temp-prefix` [#387](https://github.com/flycheck/flycheck/issues/387)
  - Add `:error-filter` property for syntax checkers to apply a custom function to modify or filter errors after parsing [#397](https://github.com/flycheck/flycheck/issues/397)
  - Add `flycheck-rust-check-tests` to disable syntax checking of test code in Rust [#406](https://github.com/flycheck/flycheck/issues/406)
  - Add `flycheck-cppcheck-inconclusive` to enable cppcheck tests that might give false positives [#407](https://github.com/flycheck/flycheck/issues/407)
- Improvements:
  - Collapse redundant whitespace in messages from `emacs-lisp` [#397](https://github.com/flycheck/flycheck/issues/397)
  - Dedent messages from `haskell-ghc` [#397](https://github.com/flycheck/flycheck/issues/397)
  - Fold errors in included files into the error messages of the corresponding include in `c/c++-clang` [#397](https://github.com/flycheck/flycheck/issues/397)
  - The `ruby-rubylint` syntax checker now supports ruby-lint 2.0 and newer [#405](https://github.com/flycheck/flycheck/issues/405)
- Bug fixes:
  - When stopping Flycheck, correctly kill running processes and cleanup their temporary files [#334](https://github.com/flycheck/flycheck/issues/334)
  - Do not choke on files without extensions in `haskell-ghc`
  - Fix spurious warning when a syntax checker reports errors, but not for the file being checked [#391](https://github.com/flycheck/flycheck/issues/391)
  - Do not signal errors in Go Mode, when `go` is not available

## 0.18 (Mar 24, 2014)

- **Breaking changes**:
  - The POSIX script syntax checkers `sh-bash` and `sh-dash` were renamed to `sh-posix-bash` and `sh-posix-dash` respectively. The `bash` and `zsh` syntax checkers were renamed to `sh-bash` and `sh-zsh` respectively. Thus, all shell script syntax checkers now live in the `sh-` prefix.
  - `rst-sphinx` requires Sphinx 1.2 or newer now.
  - `rustc` requires Rust 0.10 (not yet released at the time of writing) or newer now [#353](https://github.com/flycheck/flycheck/issues/353)
- New syntax checkers:
  - Perl with Perl Critic [#88](https://github.com/flycheck/flycheck/issues/88)
  - Replace GNU Make with POSIX Make [#322](https://github.com/flycheck/flycheck/issues/322)
  - Shellcheck [#267](https://github.com/flycheck/flycheck/issues/267)
  - Go with `golint` [#328](https://github.com/flycheck/flycheck/issues/328)
  - Go with `go tool vet` [#329](https://github.com/flycheck/flycheck/issues/329)
- New features:
  - Add `flycheck-rust-library-path` to specify library locations for `rust`
  - Add `flycheck-dmd-include-path` to change the include path of `d-dmd` [#344](https://github.com/flycheck/flycheck/issues/344)
- Improvements:
  - `flycheck-parse-checkstyle` supports `info` level messages now
  - Correctly parse multiline error messages of `go-build` and `go-test`
  - `rst-sphinx` supports custom nodes without explicit writer support now, by using the `pseudoxml` builder.
  - Avoid warnings about missing main functions in `rust`
  - Properly resolve relative filenames in `.. include::` directives in `rst`
  - Use `--unix_mode` option in `javascript-gjslint` to get the file name [#348](https://github.com/flycheck/flycheck/issues/348)
  - Puppet Lint messages now include the name of the corresponding check
  - `rustc` supports upcoming Rust 0.10 now [#353](https://github.com/flycheck/flycheck/issues/353)
  - Flycheck now handles Clang errors from included files [#367](https://github.com/flycheck/flycheck/issues/367)

## 0.17 (Feb 1, 2014)

- The manual was ported to Sphinx and is now located at <http://flycheck.readthedocs.org> [#274](https://github.com/flycheck/flycheck/issues/274)
- **Breaking changes**:
  - The default `flycheck-completion-system` was changed to nil, i.e. the built-in `completing-read`, for compliance with Emacs' defaults. To restore the previous behaviour, add `(eval-after-load 'flycheck '(setq flycheck-completion-system 'ido))` to your `init.el`.
  - `flycheck-count-errors` counts errors of all levels now, and returns an alist mapping error symbols to error counts.
- New syntax checkers:
  - RST (ReStructuredText) using Sphinx
  - GNU Make [#321](https://github.com/flycheck/flycheck/issues/321)
- New features:
  - Extend syntax checkers with `flycheck-add-next-checkers` [#266](https://github.com/flycheck/flycheck/issues/266)
- Improvements:
  - Immediately re-check the buffer when it was changed during a syntax check [#301](https://github.com/flycheck/flycheck/issues/301)
  - Do not defer syntax checker after idle change timeout [#305](https://github.com/flycheck/flycheck/issues/305)
  - Do not use the generic `rst` syntax checker in Sphinx projects anymore, to avoid false positives by Sphinx-only markup
  - Check for more than just syntax errors in `rust` [#314](https://github.com/flycheck/flycheck/issues/314)
  - `chef-foodcritic` supports `enh-ruby-mode` now
- Bug fixes
  - Do not attach syntax checker processes to the buffer anymore [#298](https://github.com/flycheck/flycheck/issues/298)
  - Do not visit the file to check in `emacs-lisp` and `emacs-lisp-checkdoc` to avoid unintended side effects [#319](https://github.com/flycheck/flycheck/issues/319)

## 0.16 (Jan 11, 2014)

- **Breaking changes**:
  - Argument substitution is no longer performed on syntax checker executables. The executable must be a string.
  - Split out `haskell-hdevtools` into a separate package. See [flycheck-hdevtools](https://github.com/flycheck/flycheck-hdevtools) [#275](https://github.com/flycheck/flycheck/issues/275)
  - Drop support for coffeelint 0.x
  - The error list is reimplemented on top of Tabulated List Mode. This greatly changes the appearance and behaviour of the error list [#230](https://github.com/flycheck/flycheck/issues/230)
- New syntax checkers:
  - Ruby with `ruby-lint` [#250](https://github.com/flycheck/flycheck/issues/250)
  - Handlebars [#270](https://github.com/flycheck/flycheck/issues/270)
  - YAML with `yaml-jsyaml` [#253](https://github.com/flycheck/flycheck/issues/253)
  - Chef recipes with `foodcritic` [#255](https://github.com/flycheck/flycheck/issues/255)
  - AsciiDoc [#276](https://github.com/flycheck/flycheck/issues/276)
  - CFEngine [#271](https://github.com/flycheck/flycheck/issues/271)
  - Racket [#277](https://github.com/flycheck/flycheck/issues/277)
  - Texinfo
  - Verilog [#296](https://github.com/flycheck/flycheck/issues/296)
  - Javascript with `eslint` [#291](https://github.com/flycheck/flycheck/issues/291)
  - ERuby [#285](https://github.com/flycheck/flycheck/issues/285)
- New features:
  - Define variables to override the executables of syntax checkers [#272](https://github.com/flycheck/flycheck/issues/272)
  - Interactively set the executable of a syntax checker with `flycheck-set-checker-executable` [#272](https://github.com/flycheck/flycheck/issues/272)
  - Disable syntax checkers easily with `flycheck-disabled-checkers` [#269](https://github.com/flycheck/flycheck/issues/269)
  - Add support for the Compass CSS framework in the `sass` and `scss` checkers, with `flycheck-sass-compass` and `flycheck-scss-compass` respectively [#268](https://github.com/flycheck/flycheck/issues/268)
  - Disable style checks in `ruby-rubocop` with `flycheck-rubocop-lint-only` [#287](https://github.com/flycheck/flycheck/issues/287)
  - Add support for Microsoft extensions in `c/c++-clang` via `flycheck-clang-ms-extensions` [#283](https://github.com/flycheck/flycheck/issues/283)
  - New faces `flycheck-error-list-info`, `flycheck-error-list-warning`, `flycheck-error-list-error`, `flycheck-error-list-line-number` and `flycheck-error-list-column-number` [#230](https://github.com/flycheck/flycheck/issues/230)
  - Add `flycheck-ghc-no-user-package-database` to disable the user package database for `haskell-ghc`
  - Add `flycheck-ghc-package-databases` to add additional package databases to `haskell-ghc`
  - Add `flycheck-ghc-search-path` to add additional directories to the search path of `haskell-ghc`
- Improvements:
  - Demote Rubocop convention messages to `info` level
  - Stop Flycheck before the buffer is reverted [#282](https://github.com/flycheck/flycheck/issues/282)
  - Properly resolve local module imports in `haskell-ghc`
- Bug fixes:
  - Make relative imports work with `python-pylint` [#280](https://github.com/flycheck/flycheck/issues/280)
  - Fix parsing of errors in `scss` and `sass`

## 0.15 (Nov 15, 2013)

- Flycheck has a new home at <https://github.com/flycheck/flycheck>, the online manual moved to <http://flycheck.github.io>.
- **Breaking changes**:
  - Do not add the current directory to the `emacs-lisp` syntax checker load path
  - `flycheck-list-errors` cannot list errors at point anymore. It does not accept a prefix argument anymore, and takes zero arguments now [#214](https://github.com/flycheck/flycheck/issues/214)
  - `flycheck-display-errors-in-list` is gone. The error list automatically highlights the error at point now [#214](https://github.com/flycheck/flycheck/issues/214)
  - Remove obsolete `flycheck-declare-checker`
- New syntax checkers:
  - YAML [#236](https://github.com/flycheck/flycheck/issues/236)
  - Javascript with `gjslint` [#245](https://github.com/flycheck/flycheck/issues/245)
  - Slim [#246](https://github.com/flycheck/flycheck/issues/246)
  - PHP using `phpmd` [#249](https://github.com/flycheck/flycheck/issues/249)
- New features:
  - Support IDO or [Grizzl](https://github.com/grizzl/grizzl) as completion systems for `flycheck-select-checker` at `C-c ! s`
  - Disable standard error navigation with `flycheck-standard-error-navigation` [#202](https://github.com/flycheck/flycheck/issues/202)
  - Add `flycheck-clang-language-standard` to choose the language standard for C/C++ syntax checking [#207](https://github.com/flycheck/flycheck/issues/207)
  - Add `flycheck-clang-definitions` to set additional definitions for C/C++ syntax checking [#207](https://github.com/flycheck/flycheck/issues/207)
  - Add `flycheck-clang-no-rtti` to disable RTTI for C/C++ syntax checking [#207](https://github.com/flycheck/flycheck/issues/207)
  - Add new option cell `option-flag` for boolean flags in syntax checker commands
  - Add `flycheck-clang-includes` to include additional files for C/C++ syntax checking [#207](https://github.com/flycheck/flycheck/issues/207)
  - Add configuration file variable `flycheck-pylintrc` for Pylint
  - New faces `flycheck-error-list-highlight-at-point` and `flycheck-error-list-highlight` to highlight the errors at point and at the current line respectively in the error list [#214](https://github.com/flycheck/flycheck/issues/214)
  - The error list now automatically updates to show the errors of the current buffer [#214](https://github.com/flycheck/flycheck/issues/214)
  - Define new error levels with `flycheck-define-error-level` [#212](https://github.com/flycheck/flycheck/issues/212)
  - Add `flycheck-clang-standard-library` to choose the standard library for C/C++ syntax checking [#234](https://github.com/flycheck/flycheck/issues/234)
  - Customize the delay for displaying errors via `flycheck-display-errors-delay` [#243](https://github.com/flycheck/flycheck/issues/243)
  - Add `info` level for informational annotations by syntax checkers [#215](https://github.com/flycheck/flycheck/issues/215)
  - Add a new symbol `temporary-file-name` to pass temporary file names to syntax checkers [#259](https://github.com/flycheck/flycheck/issues/259)
- Improvements:
  - The error list now refreshes automatically after each syntax check [#214](https://github.com/flycheck/flycheck/issues/214)
  - The errors at point are now automatically highlighted in the error list [#214](https://github.com/flycheck/flycheck/issues/214)
  - `emacs-lisp-checkdoc` does not longer check `.dir-locals.el` files
  - Do not automatically check syntax in encrypted files [#222](https://github.com/flycheck/flycheck/issues/222)
  - Parse notes from `c/c++-clang` into info level messages [#215](https://github.com/flycheck/flycheck/issues/215)
  - Parse convention warnings from `pylint` to info level [#204](https://github.com/flycheck/flycheck/issues/204)
  - Demote naming warnings from `python-flake8` to info level [#215](https://github.com/flycheck/flycheck/issues/215)
  - Support `enh-ruby-mode` in Ruby syntax checkers [#256](https://github.com/flycheck/flycheck/issues/256)
  - Parse columns from `python-pylint` errors
  - Do not compress temporary files for syntax checks if the original file was compressed
- Bug fixes:
  - Find local includes in the Clang syntax checker [#225](https://github.com/flycheck/flycheck/issues/225)
  - Do not emit spurious flawed definition warning in the `rst` syntax checker
  - Handle abbreviated file names in `luac` output, by simply ignoring them [#251](https://github.com/flycheck/flycheck/issues/251)
  - Correctly redirect the output binary of the `go-build` syntax checker [#259](https://github.com/flycheck/flycheck/issues/259)
  - Fix Cppcheck parsing with the built-in Emacs XML parser [#263](https://github.com/flycheck/flycheck/issues/263)

## 0.14.1 (Aug 16, 2013)

- Bug fixes:
  - Add a missing dependency [#194](https://github.com/flycheck/flycheck/issues/194)

## 0.14 (Aug 15, 2013)

- **Breaking changes**:
  - Introduce `flycheck-define-checker` and obsolete `flycheck-declare-checker` [#163](https://github.com/flycheck/flycheck/issues/163)
  - Remove the obsolete `flycheck-error-face` and `flycheck-warning-face`
  - Do not initialize packages by default in `emacs-lisp` syntax checker for non-configuration files [#176](https://github.com/flycheck/flycheck/issues/176)
  - Change the default `flycheck-highlighting-mode` to `symbols` [#179](https://github.com/flycheck/flycheck/issues/179)
  - Drop support for Pylint 0.x in `python-pylint` [#184](https://github.com/flycheck/flycheck/issues/184)
- New features:
  - List errors at point only with prefix arg to `flycheck-list-errors` [#166](https://github.com/flycheck/flycheck/issues/166)
  - Add new display function `flycheck-display-errors-in-list` to display errors at point in the error list [#166](https://github.com/flycheck/flycheck/issues/166)
  - New `option-list` argument cell to pass option lists to a syntax checker
  - New `flycheck-emacs-lisp-load-path` option to customize the `load-path` used by the `emacs-lisp` syntax checker [#174](https://github.com/flycheck/flycheck/issues/174)
  - New `flycheck-emacs-lisp-initialize-packages` option to initialize packages in the `emacs-lisp` syntax checker [#176](https://github.com/flycheck/flycheck/issues/176)
  - New `flycheck-emacs-lisp-package-user-dir` option to configure the package directory for the `emacs-lisp` syntax checker [#176](https://github.com/flycheck/flycheck/issues/176)
  - New option filter `flycheck-option-comma-separated-list` for options with comma separated lists as values
  - New highlighting mode `symbols` to highlight the symbol pointed to by an error [#179](https://github.com/flycheck/flycheck/issues/179)
- New syntax checkers:
  - LESS [#160](https://github.com/flycheck/flycheck/issues/160)
  - Haskell with `ghc`, `hdevtools` and `hlint` [#162](https://github.com/flycheck/flycheck/issues/162)
  - C/C++ with `cppcheck` [#170](https://github.com/flycheck/flycheck/issues/170)
  - C/C++ with `clang` [#172](https://github.com/flycheck/flycheck/issues/172)
  - CoffeeScript with `coffee`
  - XML with `xmllint` [#180](https://github.com/flycheck/flycheck/issues/180)
  - D with `dmd` [#167](https://github.com/flycheck/flycheck/issues/167)
- Improvements:
  - Support Web Mode in `html-tidy` syntax checker [#157](https://github.com/flycheck/flycheck/issues/157)
  - Support Rubocop 0.9 and drop support for older Rubocop releases [#159](https://github.com/flycheck/flycheck/issues/159)
  - Include the message ID in error messages from `python-pylint`
- Bug fixes:
  - Fix warnings about flawed definitions in `emacs-lisp` and `emacs-lisp-checkdoc`, caused by faulty formatting of sexps
  - Refresh error lists when pressing `g` [#166](https://github.com/flycheck/flycheck/issues/166)
  - Do not obscure active minibuffer input when displaying errors in the echo area [#175](https://github.com/flycheck/flycheck/issues/175)
  - Fix universal prefix argument for `flycheck-next-error` at `C-c ! n`
  - Correctly parse output of `coffeelint` 0.5.7 [#192](https://github.com/flycheck/flycheck/issues/192)
  - Correctly parse output of `pylint` 1.0 [#184](https://github.com/flycheck/flycheck/issues/184)

## 0.13 (Jun 28, 2013)

- **Breaking changes**:
  - Obsolete `flycheck-warning-face` and `flycheck-error-face` in favor `flycheck-warning` and `flycheck-error` respectively
  - Obsolete `:predicate` forms in favor of `:predicate` functions
  - `flycheck-def-config-file-var` does not automatically mark variables as safe anymore
- New features:
  - Make fringe indicator faces customizable independently with `flycheck-fringe-error` and `flycheck-fringe-warning`
  - Improve the default faces by using underlines instead of foreground colors, if possible
  - Customizable error processing with `flycheck-process-error-functions` [#141](https://github.com/flycheck/flycheck/issues/141)
  - Make the delay before starting a syntax check customizable via `flycheck-idle-change-delay` [#144](https://github.com/flycheck/flycheck/issues/144)
  - Make display of errors under point customizable via `flycheck-display-errors-function` [#156](https://github.com/flycheck/flycheck/issues/156)
- Improvements
  - Always highlight errors on top of warnings now
  - Do not trigger syntax checks in the middle of commands [#141](https://github.com/flycheck/flycheck/issues/141)
  - Add the current directory to load path in the `emacs-lisp` syntax checker
  - Do not longer use the `emacs-lisp-checkdoc` syntax checker in Scratch buffers
  - Do not flush temporary files onto disk [#149](https://github.com/flycheck/flycheck/issues/149)
  - Syntax checkers may have error patterns and error parser now
  - Predicate forms are now wrapped into functions and compiled into functions during byte compilation
  - Copy each message separately in `flycheck-copy-messages-as-kill`
  - Mark some customizable variables as safe for file variable usage, most notably `flycheck-indication-mode`, `flycheck-highlighting-mode` and `flycheck-idle-change-delay`.
- Bug fixes:
  - Fix error when searching for a configuration file outside a Projectile project
  - Do not start a syntax check before the `flycheck-mode-hook` was run
  - Do not start automatic syntax checks if Flycheck Mode is disabled
  - Defer the initial syntax check until after the current interactive command [#143](https://github.com/flycheck/flycheck/issues/143)
  - Correctly clean up information about running processes
  - Fix compatibility with Emacs 24.2 and earlier [#150](https://github.com/flycheck/flycheck/issues/150)
  - Fix version information on Emacs trunk builds

## 0.12 (May 18, 2013)

- New syntax checkers:
  - Ruby using `jruby` [#136](https://github.com/flycheck/flycheck/issues/136)
  - Puppet [#138](https://github.com/flycheck/flycheck/issues/138)
- New features:
  - Highlight error expressions by default, with the new `sexps` highlighting mode
  - Automatically check syntax some time after the last change in the buffer [#140](https://github.com/flycheck/flycheck/issues/140)
  - Add `flycheck-version` to determine the installed Flycheck version
  - Add `flycheck-list-errors`, mapped to `C-c ! l`, to list all errors in a separate buffer
- Improvements:
  - Defer syntax checks while a buffer is reverted, to avoid race conditions
- Bug fixes:
  - Correctly parse syntax errors from JRuby [#136](https://github.com/flycheck/flycheck/issues/136)

## 0.11 (May 01, 2013)

- New syntax checkers:
  - Scala [#124](https://github.com/flycheck/flycheck/issues/124)
- New features:
  - Customizable error indication with control of the fringe side, via `flycheck-indication-mode`
  - Customizable automatic syntax checking, via `flycheck-check-syntax-automatically` [#128](https://github.com/flycheck/flycheck/issues/128)
  - Customizable configuration file search, via `flycheck-locate-config-file-functions` [#133](https://github.com/flycheck/flycheck/issues/133)
  - Find configuration files in [Projectile](https://github.com/bbatsov/projectile) projects
  - Add `flycheck-before-syntax-check-hook` and `flycheck-syntax-check-failed-hook`
- Improvements:
  - The `ruby` syntax checker now differentiates warnings from errors [#123](https://github.com/flycheck/flycheck/issues/123)
  - Faces are now in a separate customization group
- Bug fixes:
  - Add missing customization group for syntax checker options

## 0.10 (Apr 21, 2013)

- Flycheck uses `cl-lib` now. This library is built-in as of GNU Emacs 24.3. For earlier releases of GNU Emacs 24 an additional compatibility library will be installed from GNU ELPA.
- New syntax checkers:
  - POSIX Shell script using `bash` [#112](https://github.com/flycheck/flycheck/issues/112)
  - Ruby using `rubocop` [#113](https://github.com/flycheck/flycheck/issues/113)
  - Elixir [#108](https://github.com/flycheck/flycheck/issues/108)
  - Erlang [#122](https://github.com/flycheck/flycheck/issues/122)
- Removed syntax checkers:
  - Python using Pyflakes. Use the superior Flake8 syntax checker [#115](https://github.com/flycheck/flycheck/issues/115)
- New features:
  - Add `flycheck-copy-messages-as-kill`, mapped to `C-c ! C-w`, to copy all error messages under point into kill ring
  - Add `flycheck-google-messages`, mapped to `C-c ! /`, to google for error messages under point. Needs the [Google This](https://github.com/Malabarba/emacs-google-this) library
  - Syntax checkers can redirect output to a temporary directory now using the `temporary-directory` argument symbol
- Improvements:
  - Call option filters for `nil` values, too
  - Improve error parsing in Bash syntax checker [#112](https://github.com/flycheck/flycheck/issues/112)
  - Error navigation does not cross restrictions in narrowed buffers anymore
  - Try to preserve the non-directory part of the buffer's file name when substituting the `source` symbol [#99](https://github.com/flycheck/flycheck/issues/99)
- Bug fixes:
  - Fix error highlighting and navigation in narrowed buffers
  - Use a hopefully more reliable way to parse output of PHP CodeSniffer [#118](https://github.com/flycheck/flycheck/issues/118)

## 0.9 (Apr 13, 2013)

- New syntax checkers:
  - SCSS using `scss` [#103](https://github.com/flycheck/flycheck/issues/103)
  - RST (ReStructuredText) using Docutils
  - Go using `go build` and `go test` [#107](https://github.com/flycheck/flycheck/issues/107)
- Improvements:
  - Quit the error message window when navigating away from error locations

## 0.8 (Apr 9, 2013)

- New syntax checkers:
  - Go using `gofmt` [#91](https://github.com/flycheck/flycheck/issues/91)
  - Rust using `rustc` [#101](https://github.com/flycheck/flycheck/issues/101)
- New features:
  - Add a global Flycheck mode. `(global-flycheck-mode)` is now the recommended way to enable Flycheck [#29](https://github.com/flycheck/flycheck/issues/29)
  - Add support for syntax checker options [#72](https://github.com/flycheck/flycheck/issues/72)
  - Add option for the coding standard used by the `php-phpcs` syntax checker
  - Add options for the maximum McCabe complexity and the maximum line length to `python-flake8`
- Improvements:
  - Support McCabe warnings in `python-flake8`
  - Support warnings from `flake8` 2
  - Show long error messages in a popup buffer [#94](https://github.com/flycheck/flycheck/issues/94)
  - Show all error messages at point [#96](https://github.com/flycheck/flycheck/issues/96)
  - Add support for naming warings from `flake8` 2 [#98](https://github.com/flycheck/flycheck/issues/98)
  - Flycheck mode is not longer enabled for buffers whose names start with a space
  - Improve highlighting to reduce screen flickering [#100](https://github.com/flycheck/flycheck/issues/100)

## 0.7.1 (Feb 23, 2013)

- Bug fixes:
  - Do not signal errors from `flycheck-mode` [#87](https://github.com/flycheck/flycheck/issues/87)
  - Correctly fall back to `$HOME` when searching configuration files
  - Correctly ascend to parent directory when searching configuration files
- API changes:
  - Rename `config` cell to `config-file`
  - Allow to pass the result of `config-file` cells as single argument
  - Add support for evaluating Lisp forms in syntax checker commands [#86](https://github.com/flycheck/flycheck/issues/86)

## 0.7 (Feb 14, 2013)

- New features:
  - Navigate to source of syntax checker declarations from syntax checker help
  - Add online Info manual [#60](https://github.com/flycheck/flycheck/issues/60)
- Improvements:
  - Use pipes instead of TTYs to read output from syntax checkers
  - Defer syntax checks for invisible buffers [#80](https://github.com/flycheck/flycheck/issues/80)
  - Immediately display error messages after error navigation [#62](https://github.com/flycheck/flycheck/issues/62)
- Bug fixes:
  - Never select deleted buffers
  - Do not let the debugger interfere with necessary cleanup actions
  - Do not attempt to parse empty XML trees [#78](https://github.com/flycheck/flycheck/issues/78)
  - Fix infinite recursion on Windows [#81](https://github.com/flycheck/flycheck/issues/81)

## 0.6.1 (Jan 30, 2013)

- Fix package dependencies

## 0.6 (Jan 29, 2013)

- New syntax checkers:
  - Emacs Lisp with `checkdoc-current-buffer` [#53](https://github.com/flycheck/flycheck/issues/53)
  - PHP with PHP CodeSniffer [#72](https://github.com/flycheck/flycheck/issues/72)
- Removed syntax checkers:
  - Javascript with `jsl`
- New features:
  - Error navigation with `next-error` and `previous-error` [#26](https://github.com/flycheck/flycheck/issues/26)
  - Fringe icons instead of error indicators [#33](https://github.com/flycheck/flycheck/issues/33)
  - Menu entry for Flycheck [#59](https://github.com/flycheck/flycheck/issues/59)
  - Customizable error highlighting, taking the column number into account [#35](https://github.com/flycheck/flycheck/issues/35)
  - Configuration files for syntax checkers
  - Add configuration file support to the syntax checkers `coffee-coffeelint`, `html-tidy`, `javascript-jshint`, `pyton-flake8` and `tex-chktex`
  - Allow to compile a buffer with a syntax checker for testing purposes [#58](https://github.com/flycheck/flycheck/issues/58)
  - Use multiple syntax checkers during a syntax check [#31](https://github.com/flycheck/flycheck/issues/31)
  - Add dedicated help for syntax checkers [#52](https://github.com/flycheck/flycheck/issues/52)
- Improvements:
  - Match error patterns in order of declaration [#55](https://github.com/flycheck/flycheck/issues/55)
- Bug fixes:
  - Inherit highlighting faces from built-in faces [#24](https://github.com/flycheck/flycheck/issues/24)
  - Correct error patterns of the HTML syntax checker [#36](https://github.com/flycheck/flycheck/issues/36)
  - Detect syntax errors in the `python-flake8` syntax checker [#42](https://github.com/flycheck/flycheck/issues/42)
  - Fix various regressions after introducing unit tests
  - Inhibit syntax checking during package installation [#45](https://github.com/flycheck/flycheck/issues/45)
  - Disable syntax checking in Tramp buffers [#54](https://github.com/flycheck/flycheck/issues/54)
  - Preserve whitespace in error messages [#65](https://github.com/flycheck/flycheck/issues/65)
- API changes:
  - Replace syntax checker variables with syntax checker declarations [#41](https://github.com/flycheck/flycheck/issues/41)
  - Support parsing errors with arbitrary functions instead of error patterns [#38](https://github.com/flycheck/flycheck/issues/38)
  - Add an error parser for Checkstyle-like XML output [#38](https://github.com/flycheck/flycheck/issues/38)

## 0.5 (Dec 28, 2012)

- New syntax checkers:
  - SASS [#15](https://github.com/flycheck/flycheck/issues/15)
  - Perl [#21](https://github.com/flycheck/flycheck/issues/21)
  - XML
  - Lua [#30](https://github.com/flycheck/flycheck/issues/30)
- New features:
  - Support manual buffer-local selection of syntax checker [#25](https://github.com/flycheck/flycheck/issues/25)
  - Add customizable error indicators [#28](https://github.com/flycheck/flycheck/issues/28)
  - Echo error messages at point without 3rd-party libraries like [flymake-cursor](https://www.emacswiki.org/emacs/FlymakeCursor) [#27](https://github.com/flycheck/flycheck/issues/27)
- Improvements:
  - Remember the last automatically selected syntax checker [#24](https://github.com/flycheck/flycheck/issues/24)
- Bug fixes:
  - Fix syntax checking of buffers without backing files [#19](https://github.com/flycheck/flycheck/issues/19)
- API changes:
  - Replace underlying Flymake API with a custom syntax checking implementation [#15](https://github.com/flycheck/flycheck/issues/15)

## 0.4 (Nov 21, 2012)

- Rename the project to Flycheck [#5](https://github.com/flycheck/flycheck/issues/5)

- New syntax checkers

  - HAML [#9](https://github.com/flycheck/flycheck/issues/9)
  - CSS [#9](https://github.com/flycheck/flycheck/issues/9)
  - Javascript with `jsl` [#9](https://github.com/flycheck/flycheck/issues/9)
  - Javascript with `jshint` [#16](https://github.com/flycheck/flycheck/issues/16)
  - JSON [#12](https://github.com/flycheck/flycheck/issues/12)
  - LaTeX with `lacheck`

- Bug fixes:

  > - Fix type error when checking compressed Emacs Lisp [#10](https://github.com/flycheck/flycheck/issues/10)

## 0.3 (Nov 21, 2012)

- Replace `flymake-mode` with a custom syntax checking minor mode [#4](https://github.com/flycheck/flycheck/issues/4)

## 0.2 (Oct 25, 2012)

- New syntax checkers:
  - PHP
- API changes:
  - Simplify syntax checker declarations [#2](https://github.com/flycheck/flycheck/issues/2)

## 0.1 (Oct 11, 2012)

Initial release as flymake-checkers

- New syntax checkers:
  - TeX/LaTeX
  - Shell scripts
  - Python
  - Ruby
  - Coffeescript
  - Emacs Lisp
