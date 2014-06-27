.. default-role:: code

master (in development)
-----------------------

- **Breaking changes**:

  - The internal names of syntax checker properties changed.  **All packages
    depending on Flycheck must be recompiled!**

- New syntax checkers:

  - #408: C/C++ with GCC

- New features:

  - #412: Add `flycheck-clang-no-exceptions` and `flycheck-gcc-no-exceptions` to
    flag exceptions as errors in C++
  - #417: Add `flycheck-rust-crate-root` to resolve inter-crate references in
    `rust`

- Improvements:

  - #421: Use proper temporary files in `python-flake8`

0.19 (Jun 12, 2014)
-------------------

- #331: Flycheck now has an official logo

- **Breaking changes**:

  - #405: The `ruby-rubylint` syntax checker now requires Ruby Lint 2.0 or
    newer.

- New syntax checkers:

  - #393: Go with `errcheck`

- New features:

  - #381: Add `flycheck-keymap-prefix` to change the prefix key for Flycheck
    keybindings
  - #387: Make the prefix of Flycheck's temporary files customizable with
    `flycheck-temp-prefix`
  - #397: Add `:error-filter` property for syntax checkers to apply a custom
    function to modify or filter errors after parsing
  - #406: Add `flycheck-rust-check-tests` to disable syntax checking of test
    code in Rust
  - #407: Add `flycheck-cppcheck-inconclusive` to enable cppcheck tests that
    might give false positives

- Improvements:

  - #397: Collapse redundant whitespace in messages from `emacs-lisp`
  - #397: Dedent messages from `haskell-ghc`
  - #397: Fold errors in included files into the error messages of the
    corresponding include in `c/c++-clang`
  - #405: The `ruby-rubylint` syntax checker now supports ruby-lint 2.0 and
    newer.

- Bug fixes:

  - #334: When stopping Flycheck, correctly kill running processes and cleanup
    their temporary files
  - Do not choke on files without extensions in `haskell-ghc`
  - #391: Fix spurious warning when a syntax checker reports errors, but not for
    the file being checked
  - Do not signal errors in Go Mode, when `go` is not available

0.18 (Mar 24, 2014)
-------------------

- **Breaking changes**:

  - The POSIX script syntax checkers `sh-bash` and `sh-dash` were renamed to
    `sh-posix-bash` and `sh-posix-dash` respectively.  The `bash` and `zsh`
    syntax checkers were renamed to `sh-bash` and `sh-zsh` respectively.  Thus,
    all shell script syntax checkers now live in the `sh-` prefix.
  - `rst-sphinx` requires Sphinx 1.2 or newer now.
  - #353: `rustc` requires Rust 0.10 (not yet released at the time of writing)
    or newer now

- New syntax checkers:

  - #88: Perl with Perl Critic
  - #322: Replace GNU Make with POSIX Make
  - #267: Shellcheck
  - #328: Go with `golint`
  - #329: Go with `go tool vet`

- New features:

  - Add `flycheck-rust-library-path` to specify library locations for `rust`
  - #344: Add `flycheck-dmd-include-path` to change the include path of `d-dmd`

- Improvements:

  - `flycheck-parse-checkstyle` supports `info` level messages now
  - Correctly parse multiline error messages of `go-build` and `go-test`
  - `rst-sphinx` supports custom nodes without explicit writer support now, by
    using the `pseudoxml` builder.
  - Avoid warnings about missing main functions in `rust`
  - Properly resolve relative filenames in `.. include::` directives in `rst`
  - #348: Use `--unix_mode` option in `javascript-gjslint` to get the file name
  - Puppet Lint messages now include the name of the corresponding check
  - #353: `rustc` supports upcoming Rust 0.10 now
  - #367: Flycheck now handles Clang errors from included files

0.17 (Feb 1, 2014)
------------------

- #274: The manual was ported to Sphinx_ and is now located at
  http://flycheck.readthedocs.org

- **Breaking changes**:

  - The default `flycheck-completion-system` was changed to nil, i.e. the
    built-in `completing-read`, for compliance with Emacs' defaults.  To restore
    the previous behaviour, add `(eval-after-load 'flycheck '(setq
    flycheck-completion-system 'ido))` to your `init.el`.
  - `flycheck-count-errors` counts errors of all levels now, and returns an
    alist mapping error symbols to error counts.

- New syntax checkers:

  - RST (ReStructuredText) using Sphinx
  - #321: GNU Make

- New features:

  - #266: Extend syntax checkers with `flycheck-add-next-checkers`

- Improvements:

  - #301: Immediately re-check the buffer when it was changed during a syntax
    check
  - #305: Do not defer syntax checker after idle change timeout
  - Do not use the generic `rst` syntax checker in Sphinx projects anymore, to
    avoid false positives by Sphinx-only markup
  - #314: Check for more than just syntax errors in `rust`
  - `chef-foodcritic` supports `enh-ruby-mode` now

- Bug fixes

  - #298: Do not attach syntax checker processes to the buffer anymore
  - #319: Do not visit the file to check in `emacs-lisp` and
    `emacs-lisp-checkdoc` to avoid unintended side effects

.. _Sphinx: http://sphinx-doc.org

0.16 (Jan 11, 2014)
-------------------

- **Breaking changes**:

  - Argument substitution is no longer performed on syntax checker executables.
    The executable must be a string.
  - #275: Split out `haskell-hdevtools` into a separate package.  See
    flycheck-hdevtools_
  - Drop support for coffeelint 0.x
  - #230: The error list is reimplemented on top of Tabulated List Mode.  This
    greatly changes the appearance and behaviour of the error list.

- New syntax checkers:

  - #250: Ruby with `ruby-lint`
  - #270: Handlebars
  - #253: YAML with `yaml-jsyaml`
  - #255: Chef recipes with `foodcritic`
  - #276: AsciiDoc
  - #271: CFEngine
  - #277: Racket
  - Texinfo
  - #296: Verilog
  - #291: Javascript with `eslint`
  - #285: ERuby

- New features:

  - #272: Define variables to override the executables of syntax checkers
  - #272: Interactively set the executable of a syntax checker with
    `flycheck-set-checker-executable`
  - #269: Disable syntax checkers easily with `flycheck-disabled-checkers`
  - #268: Add support for the Compass CSS framework in the `sass` and `scss`
    checkers, with `flycheck-sass-compass` and `flycheck-scss-compass`
    respectively
  - #287: Disable style checks in `ruby-rubocop` with
    `flycheck-rubocop-lint-only`
  - #283: Add support for Microsoft extensions in `c/c++-clang` via
    `flycheck-clang-ms-extensions`
  - #230: New faces `flycheck-error-list-info`, `flycheck-error-list-warning`,
    `flycheck-error-list-error`, `flycheck-error-list-line-number` and
    `flycheck-error-list-column-number`
  - Add `flycheck-ghc-no-user-package-database` to disable the user package
    database for `haskell-ghc`
  - Add `flycheck-ghc-package-databases` to add additional package databases to
    `haskell-ghc`
  - Add `flycheck-ghc-search-path` to add additional directories to the search
    path of `haskell-ghc`

- Improvements:

  - Demote Rubocop convention messages to `info` level
  - #282: Stop Flycheck before the buffer is reverted
  - Properly resolve local module imports in `haskell-ghc`

- Bug fixes:

  - #280: Make relative imports work with `python-pylint`
  - Fix parsing of errors in `scss` and `sass`

.. _flycheck-hdevtools: https://github.com/flycheck/flycheck-hdevtools

0.15 (Nov 15, 2013)
-------------------

- Flycheck has a new home at https://github.com/flycheck/flycheck,
  the online manual moved to http://flycheck.github.io.

- **Breaking changes**:

  - Do not add the current directory to the `emacs-lisp` syntax checker load
    path
  - #214: `flycheck-list-errors` cannot list errors at point anymore.  It does
    not accept a prefix argument anymore, and takes zero arguments now
  - #214: `flycheck-display-errors-in-list` is gone.  The error list
    automatically highlights the error at point now
  - Remove obsolete `flycheck-declare-checker`

- New syntax checkers:

  - #236: YAML
  - #245: Javascript with `gjslint`
  - #246: Slim
  - #249: PHP using `phpmd`

- New features:

  - Support IDO or Grizzl_ as completion systems for `flycheck-select-checker`
    at `C-c ! s`
  - #202: Disable standard error navigation with
    `flycheck-standard-error-navigation`
  - #207: Add `flycheck-clang-language-standard` to choose the language standard
    for C/C++ syntax checking
  - #207: Add `flycheck-clang-definitions` to set additional definitions for
    C/C++ syntax checking
  - #207: Add `flycheck-clang-no-rtti` to disable RTTI for C/C++ syntax checking
  - Add new option cell `option-flag` for boolean flags in syntax checker
    commands
  - #207: Add `flycheck-clang-includes` to include additional files for C/C++
    syntax checking
  - Add configuration file variable `flycheck-pylintrc` for Pylint
  - #214: New faces `flycheck-error-list-highlight-at-point` and
    `flycheck-error-list-highlight` to highlight the errors at point and at the
    current line respectively in the error list
  - #214: The error list now automatically updates to show the errors of the
    current buffer
  - #212: Define new error levels with `flycheck-define-error-level`
  - #234: Add `flycheck-clang-standard-library` to choose the standard library
    for C/C++ syntax checking
  - #243: Customize the delay for displaying errors via
    `flycheck-display-errors-delay`
  - #215: Add `info` level for informational annotations by syntax checkers
  - #259: Add a new symbol `temporary-file-name` to pass temporary file names to
    syntax checkers

- Improvements:

  - #214: The error list now refreshes automatically after each syntax check
  - #214: The errors at point are now automatically highlighted in the error
    list
  - `emacs-lisp-checkdoc` does not longer check `.dir-locals.el` files
  - #222: Do not automatically check syntax in encrypted files
  - #215: Parse notes from `c/c++-clang` into info level messages
  - #204: Parse convention warnings from `pylint` to #215: info level
  - #215: Demote naming warnings from `python-flake8` to info level
  - #256: Support `enh-ruby-mode` in Ruby syntax checkers
  - Parse columns from `python-pylint` errors
  - Do not compress temporary files for syntax checks if the original file was
    compressed

- Bug fixes:

  - #225: Find local includes in the Clang syntax checker
  - Do not emit spurious flawed definition warning in the `rst` syntax checker
  - #251: Handle abbreviated file names in `luac` output, by simply ignoring
    them
  - #259: Correctly redirect the output binary of the `go-build` syntax checker
  - #263: Fix Cppcheck parsing with the built-in Emacs XML parser

.. _grizzl: https://github.com/d11wtq/grizzl

0.14.1 (Aug 16, 2013)
---------------------

- Bug fixes:

  - #194: Add a missing dependency

0.14 (Aug 15, 2013)
-------------------

- **Breaking changes**:

  - #163: Introduce `flycheck-define-checker` and obsolete
    `flycheck-declare-checker`
  - Remove the obsolete `flycheck-error-face` and `flycheck-warning-face`
  - #176: Do not initialize packages by default in `emacs-lisp` syntax checker
    for non-configuration files
  - #179: Change the default `flycheck-highlighting-mode` to `symbols`
  - #184: Drop support for Pylint 0.x in `python-pylint`

- New features:

  - #166: List errors at point only with prefix arg to `flycheck-list-errors`
  - #166: Add new display function `flycheck-display-errors-in-list` to display
    errors at point in the error list
  - New `option-list` argument cell to pass option lists to a syntax checker
  - #174: New `flycheck-emacs-lisp-load-path` option to customize the `load-path`
    used by the `emacs-lisp` syntax checker
  - #176: New `flycheck-emacs-lisp-initialize-packages` option to initialize
    packages in the `emacs-lisp` syntax checker
  - #176: New `flycheck-emacs-lisp-package-user-dir` option to configure the
    package directory for the `emacs-lisp` syntax checker
  - New option filter `flycheck-option-comma-separated-list` for options with
    comma separated lists as values
  - #179: New highlighting mode `symbols` to highlight the symbol pointed to by an
    error

- New syntax checkers:

  - #160: LESS
  - #162: Haskell with `ghc`, `hdevtools` and `hlint`
  - #170: C/C++ with `cppcheck`
  - #172: C/C++ with `clang`
  - CoffeeScript with `coffee`
  - #180: XML with `xmllint`
  - #167: D with `dmd`

- Improvements:

  - #157: Support Web Mode in `html-tidy` syntax checker
  - #159: Support Rubocop 0.9 and drop support for older Rubocop releases
  - Include the message ID in error messages from `python-pylint`

- Bug fixes:

  - Fix warnings about flawed definitions in `emacs-lisp` and
    `emacs-lisp-checkdoc`, caused by faulty formatting of sexps
  - #166: Refresh error lists when pressing `g`
  - #175: Do not obscure active minibuffer input when displaying errors in the
    echo area
  - Fix universal prefix argument for `flycheck-next-error` at `C-c ! n`
  - #192: Correctly parse output of `coffeelint` 0.5.7
  - #184: Correctly parse output of `pylint` 1.0

0.13 (Jun 28, 2013)
-------------------

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
  - #141: Customizable error processing with `flycheck-process-error-functions`
  - #144: Make the delay before starting a syntax check customizable via
    `flycheck-idle-change-delay`
  - #156: Make display of errors under point customizable via
    `flycheck-display-errors-function`

- Improvements

  - Always highlight errors on top of warnings now
  - #141: Do not trigger syntax checks in the middle of commands
  - Add the current directory to load path in the `emacs-lisp` syntax checker
  - Do not longer use the `emacs-lisp-checkdoc` syntax checker in Scratch
    buffers
  - #149: Do not flush temporary files onto disk
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
  - #143: Defer the initial syntax check until after the current interactive
    command
  - Correctly clean up information about running processes
  - #150: Fix compatibility with Emacs 24.2 and earlier
  - Fix version information on Emacs trunk builds

0.12 (May 18, 2013)
-------------------

- New syntax checkers:

  - #136: Ruby using `jruby`
  - #138: Puppet

- New features:

  - Highlight error expressions by default, with the new `sexps` highlighting
    mode
  - #140: Automatically check syntax some time after the last change in the
    buffer
  - Add `flycheck-version` to determine the installed Flycheck version
  - Add `flycheck-list-errors`, mapped to `C-c ! l`, to list all errors in a
    separate buffer

- Improvements:

  - Defer syntax checks while a buffer is reverted, to avoid race conditions

- Bug fixes:

  - #136: Correctly parse syntax errors from JRuby

0.11 (May 01, 2013)
-------------------

- New syntax checkers:

  - #124: Scala

- New features:

  - Customizable error indication with control of the fringe side, via
    `flycheck-indication-mode`
  - #128: Customizable automatic syntax checking, via
    `flycheck-check-syntax-automatically`
  - #133: Customizable configuration file search, via
    `flycheck-locate-config-file-functions`
  - Find configuration files in Projectile_ projects
  - Add `flycheck-before-syntax-check-hook` and
    `flycheck-syntax-check-failed-hook`

- Improvements:

  - #123: The `ruby` syntax checker now differentiates warnings from errors
  - Faces are now in a separate customization group

- Bug fixes:

  - Add missing customization group for syntax checker options

.. _Projectile: https://github.com/bbatsov/projectile

0.10 (Apr 21, 2013)
-------------------

- Flycheck uses `cl-lib` now.  This library is built-in as of GNU Emacs 24.3.
  For earlier releases of GNU Emacs 24 an additional compatibility library will
  be installed from GNU ELPA.

- New syntax checkers:

  - #112: POSIX Shell script using `bash`
  - #113: Ruby using `rubocop`
  - #108: Elixir
  - #122: Erlang

- Removed syntax checkers:

  - #115: Python using Pyflakes.  Use the superior Flake8 syntax checker

- New features:

  - Add `flycheck-copy-messages-as-kill`, mapped to `C-c ! C-w`, to copy all
    error messages under point into kill ring
  - Add `flycheck-google-messages`, mapped to `C-c ! /`, to google for error
    messages under point.  Needs the `Google This`_ library
  - Syntax checkers can redirect output to a temporary directory now using the
    `temporary-directory` argument symbol

- Improvements:

  - Call option filters for `nil` values, too
  - #112: Improve error parsing in Bash syntax checker
  - Error navigation does not cross restrictions in narrowed buffers anymore
  - #99: Try to preserve the non-directory part of the buffer's file name when
    substituting the `source` symbol

- Bug fixes:

  - Fix error highlighting and navigation in narrowed buffers
  - #118: Use a hopefully more reliable way to parse output of PHP CodeSniffer

.. _google This: https://github.com/Bruce-Connor/emacs-google-this

0.9 (Apr 13, 2013)
------------------

- New syntax checkers:

  - #103: SCSS using `scss`
  - RST (ReStructuredText) using Docutils
  - #107: Go using `go build` and `go test`

- Improvements:

  - Quit the error message window when navigating away from error locations

0.8 (Apr 9, 2013)
-----------------

- New syntax checkers:

  - #91: Go using `gofmt`
  - #101: Rust using `rustc`

- New features:

  - #29: Add a global Flycheck mode.  `(global-flycheck-mode)`
    is now the recommended way to enable Flycheck
  - #72: Add support for syntax checker options
  - Add option for the coding standard used by the `php-phpcs` syntax
    checker
  - Add options for the maximum McCabe complexity and the maximum line
    length to `python-flake8`

- Improvements:

  - Support McCabe warnings in `python-flake8`
  - Support warnings from `flake8` 2
  - #94: Show long error messages in a popup buffer
  - #96: Show all error messages at point
  - #98: Add support for naming warings from `flake8` 2
  - Flycheck mode is not longer enabled for buffers whose names start with a
    space
  - #100: Improve highlighting to reduce screen flickering

0.7.1 (Feb 23, 2013)
--------------------

- Bug fixes:

  - #87: Do not signal errors from `flycheck-mode`
  - Correctly fall back to `$HOME` when searching configuration files
  - Correctly ascend to parent directory when searching configuration files

- API changes:

  - Rename `config` cell to `config-file`
  - Allow to pass the result of `config-file` cells as single argument
  - #86: Add support for evaluating Lisp forms in syntax checker commands

0.7 (Feb 14, 2013)
------------------

- New features:

  - Navigate to source of syntax checker declarations from syntax checker help
  - #60: Add online Info manual

- Improvements:

  - Use pipes instead of TTYs to read output from syntax checkers
  - #80: Defer syntax checks for invisible buffers
  - #62: Immediately display error messages after error navigation

- Bug fixes:

  - Never select deleted buffers
  - Do not let the debugger interfere with necessary cleanup actions
  - #78: Do not attempt to parse empty XML trees
  - #81: Fix infinite recursion on Windows

0.6.1 (Jan 30, 2013)
--------------------

- Fix package dependencies

0.6 (Jan 29, 2013)
------------------

- New syntax checkers:

  - #53: Emacs Lisp with `checkdoc-current-buffer`
  - #72: PHP with PHP CodeSniffer

- Removed syntax checkers:

  - Javascript with `jsl`

- New features:

  - #26: Error navigation with `next-error` and `previous-error`
  - #33: Fringe icons instead of error indicators
  - #59: Menu entry for Flycheck
  - #35: Customizable error highlighting, taking the column number into account
  - Configuration files for syntax checkers
  - Add configuration file support to the syntax checkers `coffee-coffeelint`,
    `html-tidy`, `javascript-jshint`, `pyton-flake8` and `tex-chktex`
  - #58: Allow to compile a buffer with a syntax checker for testing purposes
  - #31: Use multiple syntax checkers during a syntax check
  - #52: Add dedicated help for syntax checkers

- Improvements:

  - #55: Match error patterns in order of declaration

- Bug fixes:

  - #24: Inherit highlighting faces from built-in faces
  - #36: Correct error patterns of the HTML syntax checker
  - #42: Detect syntax errors in the `python-flake8` syntax checker
  - Fix various regressions after introducing unit tests
  - #45: Inhibit syntax checking during package installation
  - #54: Disable syntax checking in Tramp buffers
  - #65: Preserve whitespace in error messages

- API changes:

  - #41: Replace syntax checker variables with syntax checker declarations
  - #38: Support parsing errors with arbitrary functions instead of error
    patterns
  - #38: Add an error parser for Checkstyle-like XML output

0.5 (Dec 28, 2012)
------------------

- New syntax checkers:

  - #15: SASS
  - #21: Perl
  - XML
  - #30: Lua

- New features:

  - #25: Support manual buffer-local selection of syntax checker
  - #28: Add customizable error indicators
  - #27: Echo error messages at point without 3rd-party libraries like
    flymake-cursor

- Improvements:

  - #24: Remember the last automatically selected syntax checker

- Bug fixes:

  - #19: Fix syntax checking of buffers without backing files

- API changes:

  - #15: Replace underlying Flymake API with a custom syntax checking
    implementation

.. _flymake-cursor: http://www.emacswiki.org/emacs/FlymakeCursor

0.4 (Nov 21, 2012)
------------------

- #5: Rename the project to Flycheck
- New syntax checkers

  - #9: HAML
  - #9: CSS
  - #9: Javascript with `jsl`
  - #16: Javascript with `jshint`
  - #12: JSON
  - LaTeX with `lacheck`

- Bug fixes:

  - #10: Fix type error when checking compressed Emacs Lisp


0.3 (Nov 21, 2012)
------------------

- #4: Replace `flymake-mode` with a custom syntax checking minor mode

0.2 (Oct 25, 2012)
------------------

- New syntax checkers:

  - PHP

- API changes:

  - #2: Simplify syntax checker declarations

0.1 (Oct 11, 2012)
------------------

Initial release as flymake-checkers

- New syntax checkers:

  - TeX/LaTeX
  - Shell scripts
  - Python
  - Ruby
  - Coffeescript
  - Emacs Lisp
