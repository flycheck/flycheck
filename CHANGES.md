master (in development)
=======================

- Improvements:

  - Map error IDs from flake8-pep257 to Flycheck error levels

- Bug fixes:

  - Fix folding of C/C++ errors from included files [GH-783]
  - Fix verification of SCSS-Lint checkstyle reporter

0.25.1 (Nov 16, 2015)
=====================

- Fix undefined function call in `emacs-lisp` syntax checker [GH-791]

0.25 (Nov 14, 2015)
===================

- **Breaking changes**:

  - `scala-scalastyle` now expects a `scalastyle` executable in `exec-path`
    [GH-763]
  - Drop support for legacy Ruby YAML implementations prior to Ruby 1.9.3 in
    `ruby-yaml`
  - Remove racket syntax checker due to possible code execution [GH-786]

- New syntax checkers:

  - JSON with Python’s built-in `json` module [GH-758]
  - Rust with `cargo rustc` [GH-772]

- New features:

  - Add `flycheck-help-echo-function` to customize the Help Echo of Flycheck
    overlays [GH-730]
  - Use symbolic error IDs in Pylint [GH-714]
  - Add `flycheck-pylint-use-symbolic-id` to disable symbolic IDs for Pylint
    [GH-714]
  - Add `flycheck-command-wrapper-function` to wrap syntax checker commands
    before execution [GH-629] [GH-752]
  - Add `flycheck-executable-find` to customise how Flycheck searches
    executables [GH-752]
  - Add `flycheck-hlint-args` to pass additional arguments to hlint [GH-713]
    [GH-762]
  - Add `flycheck-go-build-tags` and `flycheck-go-install-deps` to specify tags
    and automatically install dependencies with `go build` [GH-674]
  - Add :standard-input property to syntax checkers to read source from standard
    input [GH-673] [GH-728]
  - Add support for JSX modes to some Javascript checkers [GH-778]

- Improvements:

  - Improve mode line display for buffers with only info messages [GH-733]
  - Merge messages of all errors under cursor for Help Echo [GH-730]
  - Align multi-line messages in error list [GH-732] [GH-731]
  - Cache GHC output for faster syntax checking and better template haskell
    support [GH-708]
  - Fall back to `rst` if `rst-sphinx` is disabled [GH-745] [GH-746]
  - New uniform fringe indicators [GH-750]
  - Demote AsciiDoc deprecation warnings to `info` level

- Bug fixes:

  - Fix error patterns of `coq` [GH-742]
  - Support GFortran 5 [GH-751]
  - Fix stack overflow when parsing hlint errors [GH-760]
  - Fix error columns of jsonlint

0.24 (Aug 15, 2015)
===================

- **Breaking changes**:

  - Remove Elixir syntax checker due to code execution [GH-630]
  - Drop support for Emacs 24.1 and 24.2

- New syntax checkers:

  - Javascript with `jscs` [GH-634] and `standard` [GH-644]
  - Jade [GH-686]
  - SQL with `sqllint` [GH-691]
  - Groovy [GH-716]
  - Haskell with `stack ghc` [GH-711]

- New features:

  - The error list can now be filtered by error level by pressing <kbd>f</kbd>
  - Add `flycheck-error-list-minimum-level` to restrict error levels displayed
    in the error list [GH-698] [GH-701]
  - Add `flycheck-perl-include-path` to set include directories for Perl
    [GH-621]
  - Add `flycheck-rust-args` to pass additional arguments to `rustc`
  - Add `flycheck-dmd-args` to pass additional arguments to `dmd` [GH-655]
  - Add `flycheck-erlang-include-path` [GH-668] and
    `flycheck-erlang-library-path` [GH-696] for Erlang
  - Add `flycheck-verilator-include-path` to set include directories for
    Verilator [GH-684]
  - Add `flycheck-cppcheck-include-path` to set include directories for cppcheck
    [GH-687]
  - Add support for Hlint configuration file [GH-682]
  - Add Hlint options for ignore rules, language extensions and hint packages
    [GH-682]

- Improvements:

  - Show chained checkers in Help buffers for syntax checkers [GH-571]
  - Map custom error levels to compilation mode levels [GH-700]
  - `flycheck-verify-setup` now includes the manually selected checker if any
    [GH-705]
  - `flycheck-select-checker` now shows a verification buffer if the selected
    checker cannot be used [GH-705]
  - Add `flycheck-verify-checker` to check whether a specific syntax checker can
    be used in a buffer [GH-705]

- Bug fixes:

  - Fix offset of column numbers in ESLint [GH-640]
  - Properly parse indentation errors from Python 2.7 [GH-635]
  - Don’t choke if `default-directory` does not exist [GH-625]
  - Fix error parsing for Puppet 4
  - Fix duplicate checkdoc errors on Emacs 25
  - Fix level of `info` messages in `flycheck-compile` [GH-669]
  - Allow custom `:verify` functions for command checkers [GH-672]
  - Fix error when `flycheck-scalastylerc` was set to a non-existing file
  - Fix error column offsets in `scala-scalastyle`
  - Do not use `r-lintr` in non-R buffers [GH-607]
  - Enforce output format of `flake8` [GH-704]
  - Parse error ids from luacheck 0.11
  - Fix patterns for Puppet environment names [GH-694]
  - Properly locate configuration files from jshint and jscs [GH-703]
  - Fix column offsets in eslint 1.0 [GH-718]

0.23 (Apr 6, 2015)
==================

- **Breaking changes**:

  - New manual in native Texinfo format, to achieve higher quality Info manuals
  - Remove `make` syntax checker due to various issues [GH-572] [GH-573]
  - Remove `zsh` support from `sh-shellcheck`, since Shellcheck does not support
    Zsh anymore
  - Remove `global-flycheck-mode` from customization interface [GH-595]

- New syntax checkers:

  - R with `lintr` [GH-512]
  - Lua with `luacheck` [GH-591] [GH-609]
  - SCSS with `scss-lint` [GH-582] [GH-598]

- New features:

  - Add `flycheck-disable-checker` to disable a syntax checker in the current
    buffer
  - Add `flycheck-global-modes` to control in which modes `global-flycheck-mode`
    turns on `flycheck-mode`
  - Add `pedantic` and `pedantic-errors` options to Clang and GCC [GH-543]
  - Add `flycheck-foodcritic-tags` to select tags for Foodcritic [GH-560]

- Improvements:

  - `chef-foodcritic` handles relative paths correctly now [GH-556]
  - Global Flycheck Mode enables Flycheck Mode even if there is no syntax
    checker for the buffer yet [GH-568]
  - `handlebars` now supports Web Mode [GH-605]
  - Extract error IDs from `rustc`
  - Don’t cache last syntax checker in buffer anymore

- Bug fixes:

  - Fix void variable error when trying to use `flycheck-compile` with a
    non-command checker [GH-563]
  - Fix faulty mode line reporting [GH-564]
  - Automatically initialize packages when checking `user-init-file`
  - Properly initialize hook variables [GH-593]
  - Fix handling of file names with symbolic links for some checkers [GH-561]
  - Parse multiline type errors from `rustc` [GH-592]

0.22 (Dec 23, 2014)
===================

- **Breaking changes**:

  - Never allow use of disabled checkers anymore, even with
    `flycheck-select-checker`
  - Error parsers **must** set the `:buffer` and `:checker` slots of
    `flycheck-error` now
  - The internals of syntax checker definitions have changed again.  **All
    packages depending on Flycheck must be recompiled!** [GH-524]
  - `flycheck-error-list-refresh` is not an interactive command anymore
  - Replace `flycheck-perlcritic-verbosity` with `flycheck-perlcritic-severity`
  - Replace `flycheck-copy-messages-as-kill` with `flycheck-copy-errors-as-kill`
    [GH-529]
  - Remove `flycheck-google-messages` command
  - Options and config file variables are not buffer-local anymore [GH-546]

- New syntax checkers:

  - Python with `py_compile` [GH-484]

- New features:

  - `flycheck-ert.el` library to write unit tests for Flycheck extensions
  - Add `flycheck-define-generic-checker` to define syntax checkers over
    arbitrary Emacs Lisp functions [GH-169] [GH-524]
  - Add `flycheck-define-command-checker` as non-macro variant of
    `flycheck-define-checker` [GH-524]
  - Add support for IDs of errors [GH-529]
  - Add special `id` sexp to parse error ids with `:error-patterns` [GH-529]
  - Parse error IDs from Checkstyle XML [GH-259]
  - `flycheck-copy-errors-as-kill` can put error ids into kill ring now [GH-529]
  - Parse error IDs from many error checkers [GH-259]
  - Verify Flycheck setup in a buffer with `flycheck-verify-setup` [GH-338]
  - Add options for arbitrary arguments to some syntax checkers [GH-542]
  - Add `flycheck-flake8-error-level-alist` to customize error levels from
    flake8 [GH-454]

- Improvements:

  - Automatically disable syntax checkers that report too many errors [GH-476]
  - Reduce filesystem access when parsing errors to improve parsing speed
  - Add explicit `load-path` inheritance to `flycheck-emacs-lisp-load-path`, via
    new `inherit` value [GH-511]
  - Parse help messages from `rustc` [GH-517]
  - `g` in the error list checks the source buffer again [GH-532]
  - `haskell-ghc` supports literate Haskell now [GH-535]

- Bug fixes:

  - Properly parse notes in `sh-shellcheck` [GH-508]
  - Fix shell quoting in `flycheck-compile` [GH-522] [GH-523]
  - Fix faulty properties of customize options which broke `customize-changed`
    and related functions
  - Fix use deprecated option in `coffee-coffeelint`
  - Fix error columns of `python-pylint` and `tex-chktex` [GH-536]
  - Correctly compute error level of errors on included files in `c/c++-clang`
    and `c/c++-gcc` [GH-451]

0.21 (Oct 26, 2014)
===================

- **Breaking changes**:

  - `html-tidy` is not enabled in Web Mode anymore [GH-464]
  - `d-dmd` now requires DMD 2.066 or newer [GH-460]
  - `:next-checkers` now requires the maximum permissible level instead of a
    custom predicate [GH-472]
  - Remove `flycheck-error-list-highlight-at-point` face and related
    functionality [GH-490]

- New syntax checkers:

  - Coq
  - RPM spec files with `rpmlint` [GH-480] [GH-481]

- New features:

  - Add `null-device` symbol for syntax checker commands
  - Add `flycheck-display-error-messages-unless-error-list` for
    `flycheck-error-display-function`
  - Add `flycheck-error-list-after-refresh-hook` to run after the error list
    refreshes
  - Add `flycheck-navigation-minimum-level` to restrict error levels available
    for navigation [GH-398] [GH-485]
  - The error list can be sorted by message and syntax checker name now [GH-500]
  - Add `flycheck-error-list-checker-name` face to customize the appearance of
    the syntax checker name in the error list [GH-500]
  - Add `flycheck-shellcheck-excluded-warnings` to exclude warnings from
    ShellCheck reports [GH-499]
  - Add `flycheck-add-mode` to add a new major mode to a syntax checker [GH-506]
  - Add `flycheck-gcc-openmp` to enable OpenMP for GCC in C/C++ [GH-507]

- Improvements:

  - Improve GCC syntax checking by expanding templates [GH-459]
  - `d-dmd` reports errors with columns now [GH-460]
  - Remove Projectile-based config file search [GH-461]
  - Do not change point when navigating in the error list [GH-487]
  - ShellCheck warnings now include the corresponding warning code

- Bug fixes:

  - Expand `default-directory` before using it, to handle abbreviated paths
    gracefully [GH-434]
  - Restore mouse support in the error list [GH-468]
  - `less` now correctly resolves relative paths in `data-uri` [GH-471]
  - `go-errcheck` now properly uses package names as syntax checker arguments
  - `c/c++-clang` now handles empty error messages [GH-497]

0.20 (Aug 12, 2014)
===================

- **Breaking changes**:

  - The internal names of syntax checker properties changed.  **All packages
    depending on Flycheck must be recompiled!**
  - `flycheck-substitute-argument` always returns a list now
  - The special meaning of a trailing `=` in `(option …)` and `(config-file …)`
    is removed.  Both arguments must now explicitly specify `concat` to prepend
    the option as string.

- New syntax checkers:

  - C/C++ with GCC [GH-408]
  - Scala with scalastyle [GH-425]
  - Fortran with GFortran [GH-414] [GH-450]
  - Ada with GNAT [GH-414] [GH-457]

- New features:

  - Add `flycheck-clang-no-exceptions` and `flycheck-gcc-no-exceptions` to
    flag exceptions as errors in C++ [GH-412]
  - Add `flycheck-rust-crate-root` to resolve inter-crate references in `rust`
    [GH-417]
  - Add `flycheck-clang-blocks` to enable the block syntax in Clang [GH-420]
  - `read-flycheck-checker` now accepts a default value
  - Add `flycheck-status-changed-functions` to react on status changes
  - Make the mode line lighter of Flycheck customizable with
    `flycheck-mode-line`
  - Add `flycheck-rubylintrc` to support configuration files for
    `ruby-rubylint` [GH-424]
  - Add `flycheck-rust-crate-type` to make the Crate type customizable [GH-446]
  - The mode line of the error list is now customizable with
    `flycheck-error-list-mode-line` [GH-454]
  - Pressing `n` or `p` in the error list now shows the error at point in a
    separate window [GH-452] [GH-454]
  - Pressing `RET` in the error list now jumps to the error at point [GH-454]
  - The error list can now be sorted by error level by clicking on the
    corresponding list header, or by pressing `S` with point on the column text
    [GH-454]
  - Error levels defined with `flycheck-define-error-level` can now have a
    numeric severity used for sorting [GH-454]

- Improvements:

  - Use proper temporary files in `python-flake8` [GH-421]
  - Demote errors from `package-initialize` in the `emacs-lisp` checker [GH-423]
  - `flycheck-select-checker` now uses the last used syntax checker as default
    when reading from minibuffer
  - `flycheck-compile` now prompts for the syntax checker to run as `compile`
    command [GH-428]
  - The `rust` syntax checker shows info messages now [GH-439]
  - The `sass` and `scss` syntax checkers now use a temporary directory for
    their cache [GH-443] [GH-454]
  - Change the default of `flycheck-eslintrc` to `nil` [GH-447]
  - Show the menu on the mode line lighter [GH-365]
  - Greatly improve Flycheck's menu
  - `n` and `p` now navigate the error list by errors, not by lines [GH-452]
    [GH-444]
  - `c/c++-clang` does not use in-place temporary files anymore [GH-456]

- Bug fixes:

  - Properly support `unload-feature` now

- Other changes:

  - Remove dependencies on f.el and s.el

0.19 (Jun 12, 2014)
===================

- Flycheck now has an official logo [GH-331]

- **Breaking changes**:

  - The `ruby-rubylint` syntax checker now requires Ruby Lint 2.0 or
    newer. [GH-405]

- New syntax checkers:

  - Go with `errcheck` [GH-393]

- New features:

  - Add `flycheck-keymap-prefix` to change the prefix key for Flycheck
    keybindings [GH-381]
  - Make the prefix of Flycheck's temporary files customizable with
    `flycheck-temp-prefix` [GH-387]
  - Add `:error-filter` property for syntax checkers to apply a custom function
    to modify or filter errors after parsing [GH-397]
  - Add `flycheck-rust-check-tests` to disable syntax checking of test code in
    Rust [GH-406]
  - Add `flycheck-cppcheck-inconclusive` to enable cppcheck tests that might
    give false positives [GH-407]

- Improvements:

  - Collapse redundant whitespace in messages from `emacs-lisp` [GH-397]
  - Dedent messages from `haskell-ghc` [GH-397]
  - Fold errors in included files into the error messages of the corresponding
    include in `c/c++-clang` [GH-397]
  - The `ruby-rubylint` syntax checker now supports ruby-lint 2.0 and
    newer [GH-405]

- Bug fixes:

  - When stopping Flycheck, correctly kill running processes and cleanup their
    temporary files [GH-334]
  - Do not choke on files without extensions in `haskell-ghc`
  - Fix spurious warning when a syntax checker reports errors, but not for the
    file being checked [GH-391]
  - Do not signal errors in Go Mode, when `go` is not available

0.18 (Mar 24, 2014)
===================

- **Breaking changes**:

  - The POSIX script syntax checkers `sh-bash` and `sh-dash` were renamed to
    `sh-posix-bash` and `sh-posix-dash` respectively.  The `bash` and `zsh`
    syntax checkers were renamed to `sh-bash` and `sh-zsh` respectively.  Thus,
    all shell script syntax checkers now live in the `sh-` prefix.
  - `rst-sphinx` requires Sphinx 1.2 or newer now.
  - `rustc` requires Rust 0.10 (not yet released at the time of writing) or
    newer now [GH-353]

- New syntax checkers:

  - Perl with Perl Critic [GH-88]
  - Replace GNU Make with POSIX Make [GH-322]
  - Shellcheck [GH-267]
  - Go with `golint` [GH-328]
  - Go with `go tool vet` [GH-329]

- New features:

  - Add `flycheck-rust-library-path` to specify library locations for `rust`
  - Add `flycheck-dmd-include-path` to change the include path of `d-dmd`
    [GH-344]

- Improvements:

  - `flycheck-parse-checkstyle` supports `info` level messages now
  - Correctly parse multiline error messages of `go-build` and `go-test`
  - `rst-sphinx` supports custom nodes without explicit writer support now, by
    using the `pseudoxml` builder.
  - Avoid warnings about missing main functions in `rust`
  - Properly resolve relative filenames in `.. include::` directives in `rst`
  - Use `--unix_mode` option in `javascript-gjslint` to get the file name
    [GH-348]
  - Puppet Lint messages now include the name of the corresponding check
  - `rustc` supports upcoming Rust 0.10 now [GH-353]
  - Flycheck now handles Clang errors from included files [GH-367]

0.17 (Feb 1, 2014)
==================

- The manual was ported to [Sphinx](http://sphinx-doc.org) and is now located at
  <http://flycheck.readthedocs.org> [GH-274]

- **Breaking changes**:

  - The default `flycheck-completion-system` was changed to nil, i.e. the
    built-in `completing-read`, for compliance with Emacs' defaults.  To restore
    the previous behaviour, add `(eval-after-load 'flycheck '(setq
    flycheck-completion-system 'ido))` to your `init.el`.
  - `flycheck-count-errors` counts errors of all levels now, and returns an
    alist mapping error symbols to error counts.

- New syntax checkers:

  - RST (ReStructuredText) using Sphinx
  - GNU Make [GH-321]

- New features:

  - Extend syntax checkers with `flycheck-add-next-checkers` [GH-266]

- Improvements:

  - Immediately re-check the buffer when it was changed during a syntax check
    [GH-301]
  - Do not defer syntax checker after idle change timeout [GH-305]
  - Do not use the generic `rst` syntax checker in Sphinx projects anymore, to
    avoid false positives by Sphinx-only markup
  - Check for more than just syntax errors in `rust` [GH-314]
  - `chef-foodcritic` supports `enh-ruby-mode` now

- Bug fixes

  - Do not attach syntax checker processes to the buffer anymore [GH-298]
  - Do not visit the file to check in `emacs-lisp` and `emacs-lisp-checkdoc` to
    avoid unintended side effects [GH-319]

0.16 (Jan 11, 2014)
===================

- **Breaking changes**:

  - Argument substitution is no longer performed on syntax checker executables.
    The executable must be a string.
  - Split out `haskell-hdevtools` into a separate package.  See
    [flycheck-hdevtools][] [GH-275]
  - Drop support for coffeelint 0.x
  - The error list is reimplemented on top of Tabulated List Mode.  This greatly
    changes the appearance and behaviour of the error list [GH-230]

- New syntax checkers:

  - Ruby with `ruby-lint` [GH-250]
  - Handlebars [GH-270]
  - YAML with `yaml-jsyaml` [GH-253]
  - Chef recipes with `foodcritic` [GH-255]
  - AsciiDoc [GH-276]
  - CFEngine [GH-271]
  - Racket [GH-277]
  - Texinfo
  - Verilog [GH-296]
  - Javascript with `eslint` [GH-291]
  - ERuby [GH-285]

- New features:

  - Define variables to override the executables of syntax checkers [GH-272]
  - Interactively set the executable of a syntax checker with
    `flycheck-set-checker-executable` [GH-272]
  - Disable syntax checkers easily with `flycheck-disabled-checkers` [GH-269]
  - Add support for the Compass CSS framework in the `sass` and `scss` checkers,
    with `flycheck-sass-compass` and `flycheck-scss-compass` respectively
    [GH-268]
  - Disable style checks in `ruby-rubocop` with `flycheck-rubocop-lint-only`
    [GH-287]
  - Add support for Microsoft extensions in `c/c++-clang` via
    `flycheck-clang-ms-extensions` [GH-283]
  - New faces `flycheck-error-list-info`, `flycheck-error-list-warning`,
    `flycheck-error-list-error`, `flycheck-error-list-line-number` and
    `flycheck-error-list-column-number` [GH-230]
  - Add `flycheck-ghc-no-user-package-database` to disable the user package
    database for `haskell-ghc`
  - Add `flycheck-ghc-package-databases` to add additional package databases to
    `haskell-ghc`
  - Add `flycheck-ghc-search-path` to add additional directories to the search
    path of `haskell-ghc`

- Improvements:

  - Demote Rubocop convention messages to `info` level
  - Stop Flycheck before the buffer is reverted [GH-282]
  - Properly resolve local module imports in `haskell-ghc`

- Bug fixes:

  - Make relative imports work with `python-pylint` [GH-280]
  - Fix parsing of errors in `scss` and `sass`

[flycheck-hdevtools]: https://github.com/flycheck/flycheck-hdevtools

0.15 (Nov 15, 2013)
===================

- Flycheck has a new home at <https://github.com/flycheck/flycheck>, the online
  manual moved to <http://flycheck.github.io>.

- **Breaking changes**:

  - Do not add the current directory to the `emacs-lisp` syntax checker load
    path
  - `flycheck-list-errors` cannot list errors at point anymore.  It does not
    accept a prefix argument anymore, and takes zero arguments now [GH-214]
  - `flycheck-display-errors-in-list` is gone.  The error list automatically
    highlights the error at point now [GH-214]
  - Remove obsolete `flycheck-declare-checker`

- New syntax checkers:

  - YAML [GH-236]
  - Javascript with `gjslint` [GH-245]
  - Slim [GH-246]
  - PHP using `phpmd` [GH-249]

- New features:

  - Support IDO or [Grizzl][] as completion systems for
    `flycheck-select-checker` at `C-c ! s`
  - Disable standard error navigation with `flycheck-standard-error-navigation`
    [GH-202]
  - Add `flycheck-clang-language-standard` to choose the language standard for
    C/C++ syntax checking [GH-207]
  - Add `flycheck-clang-definitions` to set additional definitions for C/C++
    syntax checking [GH-207]
  - Add `flycheck-clang-no-rtti` to disable RTTI for C/C++ syntax checking
    [GH-207]
  - Add new option cell `option-flag` for boolean flags in syntax checker
    commands
  - Add `flycheck-clang-includes` to include additional files for C/C++ syntax
    checking [GH-207]
  - Add configuration file variable `flycheck-pylintrc` for Pylint
  - New faces `flycheck-error-list-highlight-at-point` and
    `flycheck-error-list-highlight` to highlight the errors at point and at the
    current line respectively in the error list [GH-214]
  - The error list now automatically updates to show the errors of the current
    buffer [GH-214]
  - Define new error levels with `flycheck-define-error-level` [GH-212]
  - Add `flycheck-clang-standard-library` to choose the standard library for
    C/C++ syntax checking [GH-234]
  - Customize the delay for displaying errors via
    `flycheck-display-errors-delay` [GH-243]
  - Add `info` level for informational annotations by syntax checkers [GH-215]
  - Add a new symbol `temporary-file-name` to pass temporary file names to
    syntax checkers [GH-259]

- Improvements:

  - The error list now refreshes automatically after each syntax check [GH-214]
  - The errors at point are now automatically highlighted in the error list
    [GH-214]
  - `emacs-lisp-checkdoc` does not longer check `.dir-locals.el` files
  - Do not automatically check syntax in encrypted files [GH-222]
  - Parse notes from `c/c++-clang` into info level messages [GH-215]
  - Parse convention warnings from `pylint` to info level [GH-204]
  - Demote naming warnings from `python-flake8` to info level [GH-215]
  - Support `enh-ruby-mode` in Ruby syntax checkers [GH-256]
  - Parse columns from `python-pylint` errors
  - Do not compress temporary files for syntax checks if the original file was
    compressed

- Bug fixes:

  - Find local includes in the Clang syntax checker [GH-225]
  - Do not emit spurious flawed definition warning in the `rst` syntax checker
  - Handle abbreviated file names in `luac` output, by simply ignoring them
    [GH-251]
  - Correctly redirect the output binary of the `go-build` syntax checker
    [GH-259]
  - Fix Cppcheck parsing with the built-in Emacs XML parser [GH-263]

[grizzl]: https://github.com/d11wtq/grizzl

0.14.1 (Aug 16, 2013)
=====================

- Bug fixes:

  - Add a missing dependency [GH-194]

0.14 (Aug 15, 2013)
===================

- **Breaking changes**:

  - Introduce `flycheck-define-checker` and obsolete `flycheck-declare-checker`
    [GH-163]
  - Remove the obsolete `flycheck-error-face` and `flycheck-warning-face`
  - Do not initialize packages by default in `emacs-lisp` syntax checker for
    non-configuration files [GH-176]
  - Change the default `flycheck-highlighting-mode` to `symbols` [GH-179]
  - Drop support for Pylint 0.x in `python-pylint` [GH-184]

- New features:

  - List errors at point only with prefix arg to `flycheck-list-errors` [GH-166]
  - Add new display function `flycheck-display-errors-in-list` to display errors
    at point in the error list [GH-166]
  - New `option-list` argument cell to pass option lists to a syntax checker
  - New `flycheck-emacs-lisp-load-path` option to customize the `load-path` used
    by the `emacs-lisp` syntax checker [GH-174]
  - New `flycheck-emacs-lisp-initialize-packages` option to initialize packages
    in the `emacs-lisp` syntax checker [GH-176]
  - New `flycheck-emacs-lisp-package-user-dir` option to configure the package
    directory for the `emacs-lisp` syntax checker [GH-176]
  - New option filter `flycheck-option-comma-separated-list` for options with
    comma separated lists as values
  - New highlighting mode `symbols` to highlight the symbol pointed to by an
    error [GH-179]

- New syntax checkers:

  - LESS [GH-160]
  - Haskell with `ghc`, `hdevtools` and `hlint` [GH-162]
  - C/C++ with `cppcheck` [GH-170]
  - C/C++ with `clang` [GH-172]
  - CoffeeScript with `coffee`
  - XML with `xmllint` [GH-180]
  - D with `dmd` [GH-167]

- Improvements:

  - Support Web Mode in `html-tidy` syntax checker [GH-157]
  - Support Rubocop 0.9 and drop support for older Rubocop releases [GH-159]
  - Include the message ID in error messages from `python-pylint`

- Bug fixes:

  - Fix warnings about flawed definitions in `emacs-lisp` and
    `emacs-lisp-checkdoc`, caused by faulty formatting of sexps
  - Refresh error lists when pressing `g` [GH-166]
  - Do not obscure active minibuffer input when displaying errors in the echo
    area [GH-175]
  - Fix universal prefix argument for `flycheck-next-error` at `C-c ! n`
  - Correctly parse output of `coffeelint` 0.5.7 [GH-192]
  - Correctly parse output of `pylint` 1.0 [GH-184]

0.13 (Jun 28, 2013)
===================

- **Breaking changes**:

  - Obsolete `flycheck-warning-face` and `flycheck-error-face` in favor
    `flycheck-warning` and `flycheck-error` respectively
  - Obsolete `:predicate` forms in favor of `:predicate` functions
  - `flycheck-def-config-file-var` does not automatically mark variables as safe
    anymore

- New features:

  - Make fringe indicator faces customizable independently with
    `flycheck-fringe-error` and `flycheck-fringe-warning`
  - Improve the default faces by using underlines instead of foreground colors,
    if possible
  - Customizable error processing with `flycheck-process-error-functions`
    [GH-141]
  - Make the delay before starting a syntax check customizable via
    `flycheck-idle-change-delay` [GH-144]
  - Make display of errors under point customizable via
    `flycheck-display-errors-function` [GH-156]

- Improvements

  - Always highlight errors on top of warnings now
  - Do not trigger syntax checks in the middle of commands [GH-141]
  - Add the current directory to load path in the `emacs-lisp` syntax checker
  - Do not longer use the `emacs-lisp-checkdoc` syntax checker in Scratch
    buffers
  - Do not flush temporary files onto disk [GH-149]
  - Syntax checkers may have error patterns and error parser now
  - Predicate forms are now wrapped into functions and compiled into functions
    during byte compilation
  - Copy each message separately in `flycheck-copy-messages-as-kill`
  - Mark some customizable variables as safe for file variable usage, most
    notably `flycheck-indication-mode`, `flycheck-highlighting-mode` and
    `flycheck-idle-change-delay`.

- Bug fixes:

  - Fix error when searching for a configuration file outside a Projectile
    project
  - Do not start a syntax check before the `flycheck-mode-hook` was run
  - Do not start automatic syntax checks if Flycheck Mode is disabled
  - Defer the initial syntax check until after the current interactive command
    [GH-143]
  - Correctly clean up information about running processes
  - Fix compatibility with Emacs 24.2 and earlier [GH-150]
  - Fix version information on Emacs trunk builds

0.12 (May 18, 2013)
===================

- New syntax checkers:

  - Ruby using `jruby` [GH-136]
  - Puppet [GH-138]

- New features:

  - Highlight error expressions by default, with the new `sexps` highlighting
    mode
  - Automatically check syntax some time after the last change in the buffer
    [GH-140]
  - Add `flycheck-version` to determine the installed Flycheck version
  - Add `flycheck-list-errors`, mapped to `C-c ! l`, to list all errors in a
    separate buffer

- Improvements:

  - Defer syntax checks while a buffer is reverted, to avoid race conditions

- Bug fixes:

  - Correctly parse syntax errors from JRuby [GH-136]

0.11 (May 01, 2013)
===================

- New syntax checkers:

  - Scala [GH-124]

- New features:

  - Customizable error indication with control of the fringe side, via
    `flycheck-indication-mode`
  - Customizable automatic syntax checking, via
    `flycheck-check-syntax-automatically` [GH-128]
  - Customizable configuration file search, via
    `flycheck-locate-config-file-functions` [GH-133]
  - Find configuration files in [Projectile][] projects
  - Add `flycheck-before-syntax-check-hook` and
    `flycheck-syntax-check-failed-hook`

- Improvements:

  - The `ruby` syntax checker now differentiates warnings from errors [GH-123]
  - Faces are now in a separate customization group

- Bug fixes:

  - Add missing customization group for syntax checker options

[Projectile]: https://github.com/bbatsov/projectile

0.10 (Apr 21, 2013)
===================

- Flycheck uses `cl-lib` now.  This library is built-in as of GNU Emacs 24.3.
  For earlier releases of GNU Emacs 24 an additional compatibility library will
  be installed from GNU ELPA.

- New syntax checkers:

  - POSIX Shell script using `bash` [GH-112]
  - Ruby using `rubocop` [GH-113]
  - Elixir [GH-108]
  - Erlang [GH-122]

- Removed syntax checkers:

  - Python using Pyflakes.  Use the superior Flake8 syntax checker [GH-115]

- New features:

  - Add `flycheck-copy-messages-as-kill`, mapped to `C-c ! C-w`, to copy all
    error messages under point into kill ring
  - Add `flycheck-google-messages`, mapped to `C-c ! /`, to google for error
    messages under point.  Needs the [Google This][] library
  - Syntax checkers can redirect output to a temporary directory now using the
    `temporary-directory` argument symbol

- Improvements:

  - Call option filters for `nil` values, too
  - Improve error parsing in Bash syntax checker [GH-112]
  - Error navigation does not cross restrictions in narrowed buffers anymore
  - Try to preserve the non-directory part of the buffer's file name when
    substituting the `source` symbol [GH-99]

- Bug fixes:

  - Fix error highlighting and navigation in narrowed buffers
  - Use a hopefully more reliable way to parse output of PHP CodeSniffer
    [GH-118]

[Google This]: https://github.com/Bruce-Connor/emacs-google-this

0.9 (Apr 13, 2013)
==================

- New syntax checkers:

  - SCSS using `scss` [GH-103]
  - RST (ReStructuredText) using Docutils
  - Go using `go build` and `go test` [GH-107]

- Improvements:

  - Quit the error message window when navigating away from error locations

0.8 (Apr 9, 2013)
=================

- New syntax checkers:

  - Go using `gofmt` [GH-91]
  - Rust using `rustc` [GH-101]

- New features:

  - Add a global Flycheck mode.  `(global-flycheck-mode)` is now the recommended
    way to enable Flycheck [GH-29]
  - Add support for syntax checker options [GH-72]
  - Add option for the coding standard used by the `php-phpcs` syntax
    checker
  - Add options for the maximum McCabe complexity and the maximum line
    length to `python-flake8`

- Improvements:

  - Support McCabe warnings in `python-flake8`
  - Support warnings from `flake8` 2
  - Show long error messages in a popup buffer [GH-94]
  - Show all error messages at point [GH-96]
  - Add support for naming warings from `flake8` 2 [GH-98]
  - Flycheck mode is not longer enabled for buffers whose names start with a
    space
  - Improve highlighting to reduce screen flickering [GH-100]

0.7.1 (Feb 23, 2013)
====================

- Bug fixes:

  - Do not signal errors from `flycheck-mode` [GH-87]
  - Correctly fall back to `$HOME` when searching configuration files
  - Correctly ascend to parent directory when searching configuration files

- API changes:

  - Rename `config` cell to `config-file`
  - Allow to pass the result of `config-file` cells as single argument
  - Add support for evaluating Lisp forms in syntax checker commands [GH-86]

0.7 (Feb 14, 2013)
==================

- New features:

  - Navigate to source of syntax checker declarations from syntax checker help
  - Add online Info manual [GH-60]

- Improvements:

  - Use pipes instead of TTYs to read output from syntax checkers
  - Defer syntax checks for invisible buffers [GH-80]
  - Immediately display error messages after error navigation [GH-62]

- Bug fixes:

  - Never select deleted buffers
  - Do not let the debugger interfere with necessary cleanup actions
  - Do not attempt to parse empty XML trees [GH-78]
  - Fix infinite recursion on Windows [GH-81]

0.6.1 (Jan 30, 2013)
====================

- Fix package dependencies

0.6 (Jan 29, 2013)
==================

- New syntax checkers:

  - Emacs Lisp with `checkdoc-current-buffer` [GH-53]
  - PHP with PHP CodeSniffer [GH-72]

- Removed syntax checkers:

  - Javascript with `jsl`

- New features:

  - Error navigation with `next-error` and `previous-error` [GH-26]
  - Fringe icons instead of error indicators [GH-33]
  - Menu entry for Flycheck [GH-59]
  - Customizable error highlighting, taking the column number into account
    [GH-35]
  - Configuration files for syntax checkers
  - Add configuration file support to the syntax checkers `coffee-coffeelint`,
    `html-tidy`, `javascript-jshint`, `pyton-flake8` and `tex-chktex`
  - Allow to compile a buffer with a syntax checker for testing purposes [GH-58]
  - Use multiple syntax checkers during a syntax check [GH-31]
  - Add dedicated help for syntax checkers [GH-52]

- Improvements:

  - Match error patterns in order of declaration [GH-55]

- Bug fixes:

  - Inherit highlighting faces from built-in faces [GH-24]
  - Correct error patterns of the HTML syntax checker [GH-36]
  - Detect syntax errors in the `python-flake8` syntax checker [GH-42]
  - Fix various regressions after introducing unit tests
  - Inhibit syntax checking during package installation [GH-45]
  - Disable syntax checking in Tramp buffers [GH-54]
  - Preserve whitespace in error messages [GH-65]

- API changes:

  - Replace syntax checker variables with syntax checker declarations [GH-41]
  - Support parsing errors with arbitrary functions instead of error patterns
    [GH-38]
  - Add an error parser for Checkstyle-like XML output [GH-38]

0.5 (Dec 28, 2012)
==================

- New syntax checkers:

  - SASS [GH-15]
  - Perl [GH-21]
  - XML
  - Lua [GH-30]

- New features:

  - Support manual buffer-local selection of syntax checker [GH-25]
  - Add customizable error indicators [GH-28]
  - Echo error messages at point without 3rd-party libraries like
    [flymake-cursor][] [GH-27]

- Improvements:

  - Remember the last automatically selected syntax checker [GH-24]

- Bug fixes:

  - Fix syntax checking of buffers without backing files [GH-19]

- API changes:

  - Replace underlying Flymake API with a custom syntax checking implementation
    [GH-15]

[flymake-cursor]: http://www.emacswiki.org/emacs/FlymakeCursor

0.4 (Nov 21, t2012)
------------------

- Rename the project to Flycheck [GH-5]
- New syntax checkers

  - HAML [GH-9]
  - CSS [GH-9]
  - Javascript with `jsl` [GH-9]
  - Javascript with `jshint` [GH-16]
  - JSON [GH-12]
  - LaTeX with `lacheck`

- Bug fixes:

  - Fix type error when checking compressed Emacs Lisp [GH-10]

0.3 (Nov 21, 2012)
==================

- Replace `flymake-mode` with a custom syntax checking minor mode [GH-4]

0.2 (Oct 25, 2012)
==================

- New syntax checkers:

  - PHP

- API changes:

  - Simplify syntax checker declarations [GH-2]

0.1 (Oct 11, 2012)
==================

Initial release as flymake-checkers

- New syntax checkers:

  - TeX/LaTeX
  - Shell scripts
  - Python
  - Ruby
  - Coffeescript
  - Emacs Lisp
