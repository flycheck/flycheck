33-cvs (in development)
=======================

- New features and improvements

  - The ``flycheck-verify-setup`` UI now includes buttons to re-enable manually
    disabled checkers and to try to re-enable automatically disabled checkers
    (command checkers are automatically disabled when their executable cannot be
    found). [GH-1755]
  - Error explainers can now return URLs (to show a webpage) or functions (to
    use custom formatting).  For example, the Rust checker now renders
    explanations using ``markdown-view-mode``. [GH-1753]

- **Breaking changes**

  - The variable ``flycheck-current-errors`` now contains errors in the order in
    which they were returned by checkers.  In previous versions of Flycheck,
    this list was sorted by error position and severity. [GH-1749]

32-cvs (frozen on May 3rd, 2020)
================================

- Highlights

  - Many checkers and compiler, such as ``ocaml``, ``rust``, ``eslint``, and
    others, include end-line and end-column information.  Flycheck can now
    highlight the exact region that they report.  Authors of checker definitions
    can use the new ``:end-line`` and ``:end-column`` arguments in
    ``flycheck-error-new``, or the new ``end-line`` and ``end-column`` fields in
    error patterns. [GH-1400]

  - Errors that checkers return for other files will now be displayed on the
    first line of the current buffer instead of begin discarded.  The error list
    indicates which file each error came from, and navigation moves
    automatically moves between files.  This change helps with compiled
    languages, where an error in another file may cause the current file to be
    considered invalid.  Variables ``flycheck-relevant-error-other-file-show``
    and ``flycheck-relevant-error-other-file-minimum-level`` control this
    behavior. [GH-1427]

  - Flycheck can now draw error indicators in margins in addition to fringes.
    Margins can contain arbitrary characters and images, not just monochrome
    bitmaps, allowing for a better experience on high-DPI screens.
    ``flycheck-indication-mode`` controls this behavior, and
    ``flycheck-set-indication-mode`` can be used to automatically adjust the
    fringes and margins.  Additionally, Flycheck's will now use high-resolution
    fringe bitmaps if the fringe is wide enough [GH-1742, GH-1744]

  - Error highlighting is now configurable, using the new
    ``flycheck-highlighting-style`` variable: instead of applying
    level-dependent faces (typically with wavy underlines), Flycheck can now
    insert delimiters around errors, or mix styles depending on how many lines
    an error covers.  Additionally, stipples are added in the fringes to
    indicate errors that span multiple lines. [GH-1743]

- New features and improvements

  - Flycheck can now trigger a syntax check automatically after switching
    buffers, using the ``idle-buffer-switch`` option in
    ``flycheck-check-syntax-automatically``.  This is useful when errors in a
    file are due to problems in a separate file.  Variables
    ``flycheck-idle-buffer-switch-delay`` and
    ``flycheck-buffer-switch-check-intermediate-buffers`` control the
    functionality. [GH-1297]
  - Flycheck will now use Emacs' native XML parsing when libXML fails.  This
    behavior can be changed by customizing ``flycheck-xml-parser``. [GH-1349]
  - ``flycheck-verify-setup`` now shows more clearly which checkers
    will run in the buffer, and which are misconfigured. [GH-1478]
  - Flycheck now locates checker executables using a customizable function,
    ``flycheck-executable-find``.  The default value of this function allows
    relative paths (set e.g. in file or dir-local variables) in addition to
    absolute paths and executable names. [GH-1485]
  - Checkers that report error positions as a single offset from the start of
    the file can use the new ``flycheck-error-new-at-pos`` constructor instead
    of converting that position to a line and a column. [GH-1400]
  - Config-file variables can now be set to a list of file names.  This is
    useful for checkers like mypy which don't run correctly when called from a
    subdirectory without passing an explicit config file. [GH-1711]
  - Thanks to algorithmic improvements in error reporting, Flycheck is now much
    faster in large buffers. [GH-1750]

- New syntax checkers:

  - Awk with ``gawk`` [GH-1708]
  - Bazel with ``bazel-buildifier`` [GH-1613]
  - CUDA with ``cuda-nvcc`` [GH-1508]
  - CWL with ``schema-salad-tool`` [GH-1361]
  - Elixir with ``credo`` [GH-1062]
  - JSON with ``json-jq`` [GH-1568]
  - Jsonnet with ``jsonnet`` [GH-1345]
  - MarkdownLint CLI with ``markdownlint`` [GH-1366]
  - mypy with ``python-mypy`` [GH-1354]
  - Nix with ``nix-linter`` [GH-1530]
  - Opam with ``opam lint`` [GH-1532]
  - protobuf-prototool with ``prototool`` [GH-1591]
  - Rust with ``rust-clippy`` [GH-1385]
  - Ruumba with ``eruby-ruumba`` [GH-1616]
  - Staticcheck with ``go-staticheck`` [GH-1541]
  - terraform with ``terraform fmt``, ``tflint`` [GH-1586]
  - Tcl with ``nagelfar`` [GH-1365]
  - Text prose with ``textlint`` [GH-1534]
  - VHDL with ``ghdl`` [GH-1160]

- Checker improvements:

  - ``python-pylint`` and ``python-flake8`` are now invoked with ``python -c``,
    to make it easier to change between Python 2 and Python 3. [GH-1113]
  - Add ``flycheck-perl-module-list`` to use specified modules when
    syntax checking code with the ``perl`` checker. [GH-1207]
  - ``rust-cargo`` now uses ``cargo check`` and ``cargo test``. [GH-1289]
  - Add ``flycheck-ghc-stack-project-file`` for the
    ``haskell-stack-ghc`` checker. [GH-1316]
  - Add ``flycheck-cppcheck-suppressions-file`` to pass a suppressions
    file to cppcheck. [GH-1329]
  - Add ``--force-exclusion`` flag to ``rubocop`` command. [GH-1348]
  - Flycheck now uses ESLint's JSON output instead of checkstyle XML. [GH-1350]
  - Add ``flychjeck-eslint-args`` to pass arguments to ``javascript-eslint``.
    [GH-1360]
  - Flycheck will now execute ``rubocop`` from the directory where a ``Gemfile``
    is located. If a ``Gemfile`` does not exist, the old behaviour of running
    the command from the directory where ``.rubocop.yml`` is found will be
    used. [GH-1368]
  - Add ``flycheck-sh-bash-args`` to pass arguments to ``sh-bash``. [GH-1439]
  - ``haskell-stack-ghc`` will not try to install GHC anymore. [GH-1443]
  - Add ``flycheck-ghdl-ieee-library`` to select which standard IEEE
    library to use for ghdl. [GH-1547]
  - The ``javascript-eslint`` checker now supports ``typescript-mode`` by
    default.
  - Add ``flycheck-erlang-rebar3-profile`` to select which profile to
    use when compiling erlang with rebar3. [GH-1560]
  - Add ``flycheck-relevant-error-other-file-show`` to avoid showing errors
    from other files. [GH-1579]
  - The ``nix-linter`` checker now has an error explainer. [GH-1586]
  - The Emacs Lisp checker can now run in buffers not backed by files. [GH-1695]

- **Breaking changes**

  - Remove the ``javascript-jscs`` checker. [GH-1024]
  - Remove the ``elixir-dogma`` checker. [GH-1450]
  - ``rust-cargo`` now requires Rust 1.17 or newer. [GH-1289]
  - ``rust`` now requires 1.18 or newer. [GH-1501]
  - Rename ``flycheck-cargo-rustc-args`` to ``flycheck-cargo-check-args``.
    [GH-1289]
  - ``rust-cargo`` does not use the variable ``flycheck-rust-args`` anymore.
    [GH-1289]
  - Improve detection of default directory for ``haskell-ghc`` to consider
    ``hpack`` project files. [GH-1435]
  - Replace ``go tool vet`` with ``go vet``. [GH-1548]
  - Remove the deprecated ``go-megacheck`` checker, which is replaced by
    ``go-staticcheck``. [GH-1583]

31 (Oct 07, 2017)
=================

- **Breaking changes**

  - ``rust-cargo`` now requires Rust 1.15 or newer [GH-1201]
  - Remove javascript-gjslint checker

- New syntax checkers:

  - Protobuf with ``protoc`` [GH-1125]
  - systemd-analyze with ``systemd-analyze`` [GH-1135]
  - Nix with ``nix-instantiate`` [GH-1164]
  - Dockerfile with ``hadolint`` [GH-1194]
  - AsciiDoc with ``asciidoctor`` [GH-1167]
  - CSS/SCSS/LESS with ``stylelint`` [GH-903]
  - Ruby with ``reek`` [GH-1244]
  - Go with ``megacheck`` [GH-1290]
  - LLVM IR with ``llc`` [GH-1302]
  - Text prose with ``proselint`` [GH-1304]

- New features:

  - Add ``flycheck-xml-xmlstarlet-xsd-path`` and ``flycheck-xml-xmllint-xsd-path`` to
    specify an XSD schema to validate XML documents against [GH-1272]
  - Add ``flycheck-tslint-args`` to pass additional arguments to tslint [GH-1186]
  - Add an error explainer to the ``rpm-rpmlint`` checker using
    ``rpmlint -I`` [GH-1235]
  - Add ``flycheck-emacs-lisp-check-declare`` to check function declaration in
    the ``emacs-lisp`` checker [GH-1286]
  - Add ``flycheck-shellcheck-follow-sources`` to check included files when
    using the ``sh-shellcheck`` checker [GH-1256]

- Improvements:

  - Use option ``flycheck-go-build-tags`` for ``go-test``,
    ``go-vet`` and ``go-errcheck`` as well.
  - Add a revert function to ``flycheck-verify-setup``, so hitting
    ``g`` reloads the buffer.
  - Make sure the erlang compiler is only run on compilable files.
  - ``flycheck-tslint`` does not crash any more on deprecation notices [GH-1174]
  - ``rust-cargo`` now checks integration tests, examples and benchmarks
    [GH-1206]
  - ``rust-cargo`` does not use ``flycheck-rust-library-path`` anymore, as
    dependencies are taken care of by Cargo [GH-1206]
  - ``c/c++-gcc`` checker now works from GCC 4.4 and up [GH-1226]

30 (Oct 12, 2016)
=================

- **Breaking changes**

  - Flycheck now requires flake8 3.0 or newer
  - Remove ``--config`` option in ``lua-luacheck`` in favour of ``luacheck``'s
    own ``.luacheckrc`` detection. Therefore ``flycheck-luacheckrc`` is
    no longer used [GH-1057]
  - ``:modes`` is now mandatory for syntax checker definitions [GH-1071]
  - Remove jade checker [GH-951] [GH-1084]
  - Remove ``javascript-eslintrc`` and instead rely on eslint's own configuration file
    search [GH-1085]
  - ``C-c ! e`` explains errors now [GH-1122]

- New syntax checkers:

  - Elixir with ``dogma`` [GH-969]
  - sass and scss with ``sass-lint`` [GH-1070]
  - Pug [GH-951] [GH-1084]

- New features:

  - Add ``flycheck-cargo-rustc-args`` to pass multiple arguments to cargo rustc
    subcommand [GH-1079]
  - Add ``:error-explainer`` to ``flycheck-define-checker`` and
    ``flycheck-explain-error-at-point`` to display explanations of errors
    [GH-1122]
  - Add an error explainer to the ``rust`` and ``rust-cargo`` checkers using
    ``rustc --explain`` [GH-1122]
  - Add ``:enabled`` property to ``flycheck-define-checker`` [GH-1089]

- Improvements:

  - Do not use ``javascript-eslint`` if eslint cannot find a valid configuration
    [GH-1085]
  - Automatically disable syntax checkers which are not installed instead of
    checking executable before each syntax check [GH-1116]
  - Add patterns for syntax errors to ``scheme-chicken`` [GH-1123]

29 (Aug 28, 2016)
=================

- **Breaking changes**

  - Change ``flycheck-eslint-rulesdir`` (string) to
    ``flycheck-eslint-rules-directories`` (list of strings) [GH-1016]
  - Require rust 1.7 or newer for ``rust`` and ``rust-cargo`` [GH-1036]

- New syntax checkers:

  - Slim with ``slim-lint`` [GH-1013]
  - CHICKEN Scheme with ``csc`` [GH-987]

- New features:

  - Add ``:working-directory`` option to ``flycheck-define-command-checker``
    [GH-973] [GH-1012]
  - ``flycheck-go-build-install-deps`` turns on dependency installation for ``go test``
    as well as ``go build`` [GH-1003]

- Improvements:

  - Add default directory for ``haskell-stack-ghc`` and ``haskell-ghc`` checkers
    [GH-1007]
  - ``rust`` and ``rust-cargo`` checkers now support the new error format of
    rust 1.12 [GH-1016]
  - ``flycheck-verify-checker`` and ``flycheck-verify-setup`` now include
    information about configuration files of syntax checkers [GH-1021] [GH-1038]

28 (Jun 05, 2016)
=================

- **Breaking changes**:

  - Rename ``luacheck`` to ``lua-luacheck`` to comply with our naming
    conventions
  - Remove ``flycheck-cppcheck-language-standard`` in favour of
    ``flycheck-cppcheck-standards`` which is a list of standards [GH-960]

- New features:

  - Add option to set binary name for ``rust-cargo`` [GH-958]
  - Add ``flycheck-cppcheck-standards`` to pass multiple code standards to
    cppcheck [GH-960]
  - Add ``flycheck-cppcheck-suppressions`` to suppress warnings for cppcheck
    [GH-960]

- Improvements:

  - Check Racket syntax in Geiser Mode [GH-979]

- Bug fixes

  - Do not signal errors when tslint reports no output [GH-981]
  - Do not generate invalid temporary filenames on Windows [GH-983]

27 (May 08, 2016)
=================

- **Breaking changes**

  - Require PHP Code Sniffer 2.6 or newer for ``php-phpcs`` [GH-921]

- New syntax checkers:

  - Go with ``go-unconvert`` [GH-905]
  - Markdown with ``mdl`` [GH-839] [GH-916]
  - TypeScript with ``tslint`` [GH-947] [GH-949]

- Improvements:

  - Pass checkdoc settings from Emacs to `emacs-lisp-checkdoc` [GH-741] [GH-937]

- Bug fixes:

  - Fix parsing of syntax errors in triple-quoted strings for
    ``python-pycompile`` [GH-948]
  - Correctly handle rules based on the current file name in ``php-phpcs``
    [GH-921]

26 (Apr 27, 2016)
=================

Flycheck now has a `Code of Conduct`_ which defines the acceptable behaviour and
the moderation guidelines for the Flycheck community. [GH-819]

Flycheck also provides a `Gitter channel`_ now for questions and discussions
about development. [GH-820]

The native Texinfo manual is again replaced with a Sphinx_ based documentation.
We hope that this change makes the manual easier to edit and to maintain and
more welcoming for new contributors.  The downside is that we can not longer
include a Info manual in Flycheck’s MELPA packages.

From this release onward Flycheck will use a single continuously increasing
version number.  Breaking changes may occur at any point.

.. _Code of Conduct: http://www.flycheck.org/en/latest/community/conduct.html
.. _Gitter channel: https://gitter.im/flycheck/flycheck
.. _Sphinx: http://sphinx-doc.org

- **Breaking changes**:

  - Remove ``flycheck-copy-messages-as-kill``, obsolete since Flycheck
    0.22
  - Remove ``flycheck-perlcritic-verbosity``, obsolete since Flycheck
    0.22
  - Replace ``flycheck-completion-system`` with
    ``flycheck-completing-read-function`` [GH-870]
  - JSON syntax checkers now require ``json-mode`` and do not check in
    Javascript Mode anymore
  - Prefer eslint over jshint for Javascript
  - Obsolete ``flycheck-info`` in favour of the new ``flycheck-manual`` command

- New syntax checkers:

  - Processing [GH-793] [GH-812]
  - Racket [GH-799] [GH-873]

- New features:

  - Add ``flycheck-puppet-lint-rc`` to customise the location of the
    puppetlint configuration file [GH-846]
  - Add ``flycheck-puppet-lint-disabled-checks`` to disable specific
    checks of puppetlint [GH-824]
  - New library ``flycheck-buttercup`` to support writing Buttercup_ specs for
    Flycheck
  - Add ``flycheck-perlcriticrc`` to set a configuration file for
    Perl::Critic [GH-851]
  - Add ``flycheck-jshint-extract-javascript`` to extract Javascript
    from HTML [GH-825]
  - Add ``flycheck-cppcheck-language-standard`` to set the language
    standard for cppcheck [GH-862]
  - Add ``flycheck-mode-line-prefix`` to customise the prefix of
    Flycheck’s mode line lighter [GH-879] [GH-880]
  - Add ``flycheck-go-vet-shadow`` to check for shadowed variables
    with ``go vet`` [GH-765] [GH-897]
  - Add ``flycheck-ghc-stack-use-nix`` to enable Nix support for Stack GHC
    [GH-913]

- Improvements:

  - Map error IDs from flake8-pep257 to Flycheck error levels
  - Explicitly display errors at point with ``C-c ! h`` [GH-834]
  - Merge message and checker columns in the error list to remove redundant
    ellipsis [GH-828]
  - Indicate disabled checkers in verification buffers [GH-749]
  - Do not enable Flycheck Mode in ``fundamental-mode`` buffers [GH-883]
  - Write ``go test`` output to a temporary files [GH-887]
  - Check whether ``lintr`` is actually installed [GH-911]

- Bug fixes:

  - Fix folding of C/C++ errors from included files [GH-783]
  - Fix verification of SCSS-Lint checkstyle reporter
  - Don’t fall back to ``rust`` if ``rust-cargo`` should be used [GH-817]
  - Don’t change current buffer when closing the error message buffer [GH-648]
  - Never display error message buffer in current window [GH-822]
  - Work around a caching issue in Rubocop [GH-844]
  - Fix checkdoc failure with some Emacs Lisp syntax [GH-833] [GH-845] [GH-898]
  - Correctly parse Haskell module name with exports right after the module name
    [GH-848]
  - Don’t hang when sending buffers to node.js processes on Windows
    [GH-794][GH-850]
  - Parse suggestions from ``hlint`` [GH-874]
  - Go errcheck handles multiple ``$GOPATH`` entries correctly now
    [GH-580][GH-906]
  - Properly handle Go build failing in a directory with multiple packages
    [GH-676] [GH-904]
  - Make cppcheck recognise C++ header files [GH-909]
  - Don’t run phpcs on empty buffers [GH-907]

.. _Buttercup: https://github.com/jorgenschaefer/emacs-buttercup
