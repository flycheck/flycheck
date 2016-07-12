29-cvs (in development)
=======================

- New features:

  - Add ``:working-directory`` option to ``flycheck-define-command-checker``
    [GH-973] [GH-1012]
  - ``flycheck-go-build-install-deps`` turns on dependency installation for ``go test``
    as well as ``go build`` [GH-1003]

- Improvements:

  - Add default directory for ``haskell-stack-ghc`` and ``haskell-ghc`` checkers
    [GH-1007]


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
