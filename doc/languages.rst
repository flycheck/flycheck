.. _flycheck-languages:

=====================
 Supported Languages
=====================

This document lists all programming and markup languages which Flycheck
supports.

.. note::

   Extensions may provide support for additional languages or add deeper
   integration with existing languages.

   Take a look at the :ref:`list of extensions <flycheck-extensions>` to see
   what the community can offer to you.

Each language has one or more syntax checkers whose names follow a convention of
:samp:`{language}-{tool}`.  All syntax checkers are listed in the order they
would be applied to a buffer, with all available options.  For more information
about a syntax checker open Emacs and use :command:`flycheck-describe-checker`
to view the docstring of the syntax checker.  Likewise, you may use
:command:`describe-variable` to read the complete docstring of any option.

.. supported-language:: Ada

   .. syntax-checker:: ada-gnat

      Check ADA syntax and types with `GNAT`_.

      .. _GNAT: https://www.adacore.com/community/

      .. defcustom:: flycheck-gnat-args

         A list of additional options.

      .. defcustom:: flycheck-gnat-include-path

         A list of include directories.  Relative paths are relative to the path
         of the buffer being checked.

      .. defcustom:: flycheck-gnat-language-standard

         The language standard to use as string.

      .. defcustom:: flycheck-gnat-warnings

         A list of additional warnings to enable.  Each item is the name of a
         warning category to enable.

.. supported-language:: AsciiDoc

   .. syntax-checker:: asciidoctor

      Check AsciiDoc with the default Asciidoctor_ backend.

      .. _Asciidoctor: http://asciidoctor.org

   .. syntax-checker:: asciidoc

      Check AsciiDoc_ with the standard AsciiDoc processor.

      .. _AsciiDoc: http://www.methods.co.nz/asciidoc

.. supported-language:: Awk

   .. syntax-checker:: awk-gawk

      Check Awk with gawk_.

      .. _gawk: https://www.gnu.org/software/gawk/manual

.. supported-language:: Bazel

   .. syntax-checker:: bazel-buildifier

      Check Bazel with buildifier_.

      .. _buildifier: https://github.com/bazelbuild/buildtools

.. supported-language:: C/C++
   :index_as: C
              C++

   Flycheck checks C and C++ with either `c/c++-clang` or `c/c++-gcc`, and then
   with `c/c++-cppcheck`.

   .. syntax-checker:: c/c++-clang
                       c/c++-gcc

      Check C/C++ for syntax and type errors with Clang_ or GCC_ respectively.

      .. note::

         `c/c++-gcc` requires GCC 4.4 or newer.

      .. _Clang: http://clang.llvm.org/
      .. _GCC: https://gcc.gnu.org/

      .. defcustom:: flycheck-clang-args
                     flycheck-gcc-args

         A list of additional arguments for `c/c++-clang` and `c/c++-gcc`
         respectively.

      .. defcustom:: flycheck-clang-blocks

         Whether to enable blocks in `c/c++-clang`.

      .. defcustom:: flycheck-clang-definitions
                     flycheck-gcc-definitions

         A list of additional preprocessor definitions for `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. defcustom:: flycheck-clang-include-path
                     flycheck-gcc-include-path

         A list of include directories for `c/c++-clang` and `c/c++-gcc`
         respectively, relative to the file being checked.

      .. defcustom:: flycheck-clang-includes
                     flycheck-gcc-includes

         A list of additional include files for `c/c++-clang` and `c/c++-gcc`
         respectively, relative to the file being checked.

      .. defcustom:: flycheck-clang-language-standard
                     flycheck-gcc-language-standard

         The language standard to use in `c/c++-clang` and `c/c++-gcc`
         respectively as string, via the ``-std`` option.

      .. defcustom:: flycheck-clang-ms-extensions

         Whether to enable Microsoft extensions to C/C++ in `c/c++-clang`.

      .. defcustom:: flycheck-clang-no-exceptions
                     flycheck-gcc-no-exceptions

         Whether to disable exceptions in `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. defcustom:: flycheck-clang-no-rtti
                     flycheck-gcc-no-rtti

         Whether to disable RTTI in `c/c++-clang` and `c/c++-gcc` respectively,
         via ``-fno-rtti``.

      .. defcustom:: flycheck-clang-standard-library

         The name of the standard library to use for `c/c++-clang`, as string.

      .. defcustom:: flycheck-gcc-openmp

         Whether to enable OpenMP in `c/c++-gcc`.

      .. defcustom:: flycheck-clang-pedantic
                     flycheck-gcc-pedantic

         Whether to warn about language extensions in `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. defcustom:: flycheck-clang-pedantic-errors
                     flycheck-gcc-pedantic-errors

         Whether to error on language extensions in `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. defcustom:: flycheck-clang-warnings
                     flycheck-gcc-warnings

         A list of additional warnings to enable in `c/c++-clang` and
         `c/c++-gcc` respectively.  Each item is the name of a warning or
         warning category for ``-W``.

   .. syntax-checker:: c/c++-cppcheck

      Check C/C++ for semantic and stylistic issues with cppcheck_.

      .. _cppcheck: http://cppcheck.sourceforge.net/

      .. defcustom:: flycheck-cppcheck-checks

         A list of enabled checks.  Each item is the name of a check for the
         ``--enable`` option.

      .. defcustom:: flycheck-cppcheck-inconclusive

         Whether to enable inconclusive checks.  These checks may yield more
         false positives than normal checks.

         .. note::

            This option requires cppcheck 1.54 or newer.

      .. defcustom:: flycheck-cppcheck-include-path

         A list of include directories.  Relative paths are relative to the file
         being checked.

      .. defcustom:: flycheck-cppcheck-standards

         The C, C++ and/or POSIX standards to use via one or more ``--std=``
         arguments.

      .. defcustom:: flycheck-cppcheck-suppressions

         The cppcheck suppressions list to use via one or more ``--suppress=``
         arguments.

      .. defcustom:: flycheck-cppcheck-suppressions-file

         The cppcheck suppressions file to use via the
         ``--suppressions-list=`` argument.

.. supported-language:: CFEngine

   .. syntax-checker:: cfengine

      Check syntax with `CFEngine <https://cfengine.com/>`_.

.. supported-language:: Chef

   .. syntax-checker:: chef-foodcritic

      Check style in Chef recipes with `foodcritic <http://www.foodcritic.io>`_.

      .. defcustom:: flycheck-foodcritic-tags

         A list of tags to select.

.. supported-language:: Coffeescript

   Flycheck checks Coffeescript syntax with `coffee` and then lints with
   `coffee-coffeelint`.

   .. syntax-checker:: coffee

      Check syntax with the `Coffeescript <https://coffeescript.org/>`_ compiler.

   .. syntax-checker:: coffee-coffeelint

      Lint with `Coffeelint <http://www.coffeelint.org/>`_.

      .. syntax-checker-config-file:: flycheck-coffeelintrc

.. supported-language:: Coq

   .. syntax-checker:: coq

      Check and proof with the standard `Coq <https://coq.inria.fr/>`_ compiler.

.. supported-language:: CSS

   .. syntax-checker:: css-csslint

      Check syntax and style with `CSSLint`_.

      .. _CSSLint: https://github.com/CSSLint/csslint

   .. syntax-checker:: css-stylelint

      Syntax-check and lint CSS with stylelint_.

      .. _stylelint: https://stylelint.io

      .. syntax-checker-config-file:: flycheck-stylelintrc

      .. defcustom:: flycheck-stylelint-quiet

         Whether to run stylelint in quiet mode via ``--quiet``.

.. supported-language:: CUDA C/C++
   :index_as: CUDA

   .. syntax-checker:: cuda-nvcc

      Checks syntax for CUDA C/C++ using the nvcc
      `nvcc <https://developer.nvidia.com/cuda-llvm-compiler>`_ compiler
      bundled in the NVIDIA Toolkit.

      CUDA C/C++ uses whichever system compiler you have configured, gcc/clang
      etc, but will sanitise error messages into a standardised format that
      can be picked up via flycheck. Corner cases may cause some odd behavior.

      .. defcustom:: flycheck-cuda-language-standard

         The C or C++ Language standard that you want the CUDA compiler to enforce.

      .. defcustom:: flycheck-cuda-includes

         A list of cuda includes.

      .. defcustom:: flycheck-cuda-include-path

         A list of include directories for nvcc.

      .. defcustom:: flycheck-cuda-definitions

         Additional preprocessor definitions for nvcc. Is passed unaltered to both
         GPU compiler and underlying C/C++ compiler.

.. supported-language:: CWL

   .. syntax-checker:: cwl

      Syntax check with (`Schema Salad <https://www.commonwl.org/v1.0/SchemaSalad.html>`_).

      .. defcustom:: flycheck-cwl-schema-path

         A path for the schema file for Common Workflow Language.

.. supported-language:: D

   .. syntax-checker:: d-dmd

      Check syntax and types with (`DMD <https://dlang.org/>`_).

      .. note::

         This syntax checker requires DMD 2.066 or newer.

      .. defcustom:: flycheck-dmd-include-path

         A list of include directories.

      .. defcustom:: flycheck-dmd-args

         A list of additional arguments.

   .. seealso::

      :flyc:`flycheck-d-unittest`
         Flycheck extension which provides a syntax checker to run D unittests
         on the fly and report the results with Flycheck.

.. supported-language:: Dockerfile

   .. syntax-checker:: dockerfile-hadolint

      Check syntax and code style with hadolint_

      .. _hadolint: https://github.com/hadolint/hadolint

.. supported-language:: Elixir

   .. syntax-checker:: elixir-credo

      Check code style with `credo <https://github.com/rrrene/credo>`_

      .. defcustom:: flycheck-elixir-credo-strict

         When non-nil, run credo in strict mode, via ``--strict``.

.. supported-language:: Emacs Lisp

   Flycheck checks Emacs Lisp with `emacs-lisp` and then with
   `emacs-lisp-checkdoc`.

   .. syntax-checker:: emacs-lisp

      Check syntax with the built-in byte compiler.

      .. defcustom:: flycheck-emacs-lisp-load-path

         The load path as list of strings.  Relative directories are expanded
         against the `default-directory` of the buffer being checked.

      .. defcustom:: flycheck-emacs-lisp-initialize-packages

         Whether to initialize Emacs' package manager with `package-initialize`
         before checking the buffer.  If set to :elisp:`auto` (the default),
         only initialize the package managers when checking files under
         `user-emacs-directory`.

      .. defcustom:: flycheck-emacs-lisp-package-user-dir

         The package directory as string.  Has no effect if
         `flycheck-emacs-lisp-initialize-packages` is nil.

      .. defcustom:: flycheck-emacs-lisp-check-declare

         If non-nil, also check `declare-function` forms using
         `check-declare-file`.

   .. syntax-checker:: emacs-lisp-checkdoc

      Check Emacs Lisp documentation conventions with ``checkdoc``.

   .. seealso::

      :infonode:`(elisp)Documentation Tips`
         Information about documentation conventions for Emacs Lisp.

      :gh:`purcell/flycheck-package`
         Flycheck extension which adds a syntax checker to check for violation
         of Emacs Lisp library headers and packaging conventions.

      :infonode:`(elisp)Library Headers`
         Information about library headers for Emacs Lisp files.

.. supported-language:: Ember Templates

   .. syntax-checker:: ember-template

      Check your Ember templates with
      `ember-template-lint <https://github.com/ember-template-lint/ember-template-lint>`_

      .. syntax-checker-config-file:: flycheck-ember-template-lintrc

.. supported-language:: Erlang

   Flycheck checks Erlang with `erlang-rebar3` in rebar projects and
   `erlang` otherwise.

   .. syntax-checker:: erlang

      Check Erlang with the standard `Erlang <http://www.erlang.org/>`_
      compiler.

      .. defcustom:: flycheck-erlang-include-path

         A list of include directories.

      .. defcustom:: flycheck-erlang-library-path

         A list of library directories.

   .. syntax-checker:: erlang-rebar3

      Check Erlang with the `rebar3 <https://www.rebar3.org/>`_ build tool.

      .. defcustom:: flycheck-erlang-rebar3-profile

         The profile to use when compiling, e.g. "default" or "test".
         The default value is nil which will use the test profile in test
         directories, the eqc profile in eqc directories and the default profile
         otherwise.

.. supported-language:: ERuby

   .. syntax-checker:: eruby-erubis

      Check ERuby with `erubis <http://www.kuwata-lab.com/erubis/>`_.

   .. syntax-checker:: eruby-ruumba

      Check syntax and lint with `Ruumba <https://github.com/ericqweinstein/ruumba>`_.

      .. note::

         This syntax checker requires Ruumba 0.1.7 or newer.

      .. defcustom:: flycheck-ruumba-lint-only

         Whether to suppress warnings about style issues, via the ``--lint``
         option.

      .. syntax-checker-config-file:: flycheck-ruumbarc

.. supported-language:: Fortran

   .. syntax-checker:: fortran-gfortran

      Check Fortran syntax and type with GFortran_.

      .. _GFortran: https://gcc.gnu.org/onlinedocs/gfortran/

      .. defcustom:: flycheck-gfortran-args

         A list of additional arguments.

      .. defcustom:: flycheck-gfortran-include-path

         A list of include directories.  Relative paths are relative to the file
         being checked.

      .. defcustom:: flycheck-gfortran-language-standard

         The language standard to use via the ``-std`` option.

      .. defcustom:: flycheck-gfortran-layout

         The source code layout to use.  Set to :elisp:`free` or :elisp:`fixed`
         for free or fixed layout respectively, or nil (the default) to let
         GFortran automatically determine the layout.

      .. defcustom:: flycheck-gfortran-warnings

         A list of warnings enabled via the ``-W`` option.

.. supported-language:: Go

   Flycheck checks Go with the following checkers:

   1. `go-gofmt`
   2. `go-golint`
   3. `go-vet`
   4. `go-build` or `go-test`
   5. `go-errcheck`
   6. `go-unconvert`
   7. `go-staticcheck`

   .. syntax-checker:: go-gofmt

      Check Go syntax with `gofmt <https://golang.org/cmd/gofmt/>`_.

   .. syntax-checker:: go-golint

      Check Go code style with `Golint <https://github.com/golang/lint>`_.

   .. syntax-checker:: go-vet

      Check Go for suspicious code with vet_.

      .. defcustom:: flycheck-go-vet-print-functions

         A list of print-like functions to check calls for format string problems.

      .. defcustom:: flycheck-go-build-tags

         A list of build tags.

      .. _vet: https://golang.org/cmd/vet/

   .. syntax-checker:: go-build

      Check syntax and type with the `Go compiler`_.

      .. note::

         This syntax checker requires Go 1.6 or newer.

      .. _Go compiler: https://golang.org/cmd/go

      .. defcustom:: flycheck-go-build-install-deps

         Whether to install dependencies while checking with `go-build` or
         `go-test`

      .. defcustom:: flycheck-go-build-tags
         :noindex:

         See `flycheck-go-build-tags`

   .. syntax-checker:: go-test

      Check syntax and types of Go tests with the `Go compiler`_.

      .. note::

         This syntax checker requires Go 1.6 or newer.

      .. defcustom:: flycheck-go-build-install-deps
         :noindex:

         See `flycheck-go-build-install-deps`.

      .. defcustom:: flycheck-go-build-tags
         :noindex:

         See `flycheck-go-build-tags`

   .. syntax-checker:: go-errcheck

      Check for unhandled error returns in Go with errcheck_.

      .. note::

         This syntax checker requires errcheck build from commit 8515d34 (Aug
         28th, 2015) or newer.

      .. _errcheck: https://github.com/kisielk/errcheck

      .. defcustom:: flycheck-go-build-tags
         :noindex:

         See `flycheck-go-build-tags`

   .. syntax-checker:: go-unconvert

      Check for unnecessary type conversions with unconvert_.

      .. _unconvert: https://github.com/mdempsky/unconvert

   .. syntax-checker:: go-staticcheck

      Perform static analysis and code linting with staticcheck_, the successor to megacheck.

      .. defcustom:: flycheck-go-version

         staticcheck_ explicitly supports the last two releases of Go, but
         supports targeting older versions. Go versions should be specified
         like, "1.6", or, "1.11.4".

      .. _staticcheck: https://staticcheck.io/

.. supported-language:: Groovy

   .. syntax-checker:: groovy

      Check syntax using the `Groovy <http://www.groovy-lang.org/>`_ compiler.

.. supported-language:: Haml

   .. syntax-checker:: haml

      Check syntax with the `Haml <http://haml.info/>`_ compiler.

.. supported-language:: Handlebars

   .. syntax-checker:: handlebars

      Check syntax with the `Handlebars <http://handlebarsjs.com/>`_ compiler.

.. supported-language:: Haskell

   Flycheck checks Haskell with `haskell-stack-ghc` (in Stack projects) or
   `haskell-ghc`, and then with `haskell-hlint`.

   .. seealso::

      :flyc:`flycheck-haskell`
         Flycheck extension to configure Flycheck's Haskell checkers from the
         metadata, with support for Cabal sandboxes.

      :flyc:`flycheck-hdevtools`
         Flycheck extension which adds an alternative syntax checker for GHC
         using `hdevtools <https://github.com/bitc/hdevtools/>`_.

   .. syntax-checker:: haskell-stack-ghc
                       haskell-ghc

      Check syntax and type GHC_.  In Stack_ projects invoke GHC through Stack
      to bring package dependencies from Stack in.

      .. _GHC: https://www.haskell.org/ghc/
      .. _Stack: https://github.com/commercialhaskell/stack

      .. defcustom:: flycheck-ghc-args

         A list of additional arguments.

      .. defcustom:: flycheck-ghc-no-user-package-database

         Whether to disable the user package database (only for `haskell-ghc`).

      .. defcustom:: flycheck-ghc-stack-use-nix

         Whether to enable Nix support for Stack (only for `haskell-stack-ghc`).

      .. defcustom:: flycheck-ghc-stack-project-file

         Allows to override the default ``stack.yaml`` file for Stack,
         via ``--stack-yaml`` (only for `haskell-stack-ghc`).

      .. defcustom:: flycheck-ghc-package-databases

         A list of additional package databases for GHC (only for
         `haskell-ghc`).  Each item points to a directory containing a package
         directory, via ``-package-db``.

      .. defcustom:: flycheck-ghc-search-path

         A list of module directories, via ``-i``.

      .. defcustom:: flycheck-ghc-language-extensions

         A list of language extensions, via ``-X``.

   .. syntax-checker:: haskell-hlint

      Lint with `hlint <https://github.com/ndmitchell/hlint>`_.

      .. defcustom:: flycheck-hlint-args

         A list of additional arguments.

      .. defcustom:: flycheck-hlint-language-extensions

         A list of language extensions to enable.

      .. defcustom:: flycheck-hlint-ignore-rules

         A list of rules to ignore.

      .. defcustom:: flycheck-hlint-hint-packages

         A list of additional hint packages to include.

      .. syntax-checker-config-file:: flycheck-hlintrc

.. supported-language:: HTML

   .. syntax-checker:: html-tidy

      Check HTML syntax and style with `Tidy HTML5`_.

      .. _Tidy HTML5: https://github.com/htacg/tidy-html5

      .. syntax-checker-config-file:: flycheck-tidyrc

.. supported-language:: Javascript

   Flycheck checks Javascript with one of `javascript-eslint` or
   `javascript-jshint`.

   Alternatively `javascript-standard` is used instead all of the former ones.

   .. syntax-checker:: javascript-eslint

      Check syntax and lint with `ESLint <https://eslint.org/>`_.

      .. note::

         Flycheck automatically :ref:`disables <flycheck-disable-checkers>`
         this syntax checker if eslint cannot find a valid configuration file
         for the current buffer.

      .. defcustom:: flycheck-eslint-args

         A list of additional arguments that are passed to eslint.

      .. defcustom:: flycheck-eslint-rules-directories

         A list of directories with custom rules.

   .. syntax-checker:: javascript-jshint

      Check syntax and lint with `JSHint <http://jshint.com/>`_.

      .. defcustom:: flycheck-jshint-extract-javascript

         Whether to extract Javascript from HTML before linting.

      .. syntax-checker-config-file:: flycheck-jshintrc

   .. syntax-checker:: javascript-standard

      Check syntax and code style with Standard_ or Semistandard_.

      .. _Standard: https://github.com/standard/standard
      .. _Semistandard: https://github.com/Flet/semistandard

.. supported-language:: JSON

   Flycheck checks JSON with `json-jsonlint`, `json-python-json`, or
   `json-jq`.

   .. syntax-checker:: json-jsonlint

      Check JSON with `jsonlint <https://github.com/zaach/jsonlint>`_.

   .. syntax-checker:: json-python-json

      Check JSON with Python's built-in :py:mod:`json` module.

   .. syntax-checker:: json-jq

      Check JSON with jq_.

      This checker accepts multiple consecutive JSON values in a single input, which is useful for jsonlines data.

      .. _jq: https://stedolan.github.io/jq/

.. supported-language:: Jsonnet

   .. syntax-checker:: jsonnet

      Checks `Jsonnet <https://jsonnet.org>`_ with `jsonnet`.

.. supported-language:: Less

   .. syntax-checker:: less

      Check syntax with the `Less <http://lesscss.org/>`_ compiler.

      .. note::

         This syntax checker requires lessc 1.4 or newer.

   .. syntax-checker:: less-stylelint

      Syntax-check and lint Less with stylelint_.

      .. _stylelint: https://stylelint.io

      .. syntax-checker-config-file:: flycheck-stylelintrc

      .. defcustom:: flycheck-stylelint-quiet

         Whether to run stylelint in quiet mode via ``--quiet``.

.. supported-language:: LLVM

   .. syntax-checker:: llvm-llc

      Check syntax with `llc <http://llvm.org/docs/CommandGuide/llc.html>`_.

.. supported-language:: Lua

   Flycheck checks Lua with `lua-luacheck`, falling back to `lua`.

   .. syntax-checker:: lua-luacheck

      Check syntax and lint with Luacheck_.

      .. syntax-checker-config-file:: flycheck-luacheckrc

      .. defcustom:: flycheck-luacheck-standards

         The luacheck standards to use via one or more ``--std`` arguments.

      .. _Luacheck: https://github.com/mpeterv/luacheck

   .. syntax-checker:: lua

      Check syntax with the `Lua compiler <http://www.lua.org/>`_.

.. supported-language:: Markdown

   .. syntax-checker:: markdown-markdownlint-cli

      Check Markdown with `markdownlint-cli
      <https://github.com/igorshubovych/markdownlint-cli>`_.

      .. syntax-checker-config-file:: flycheck-markdown-markdownlint-cli-config

   .. syntax-checker:: markdown-mdl

      Check Markdown with `markdownlint <https://github.com/markdownlint/markdownlint/>`_.

      .. defcustom:: flycheck-markdown-mdl-rules

         A list of enabled rules.

      .. defcustom:: flycheck-markdown-mdl-tags

         A list of enabled rule tags.

      .. syntax-checker-config-file:: flycheck-markdown-mdl-style

.. supported-language:: Nix

   .. syntax-checker:: nix

      Check Nix with nix-instantiate_.

      .. _nix-instantiate: https://nixos.org/nix/manual/#sec-nix-instantiate

   .. syntax-checker:: nix-linter

      Check Nix with nix-linter_.

      .. _nix-linter: https://github.com/Synthetica9/nix-linter

.. supported-language:: Opam

   .. syntax-checker:: opam

      Check Opam configuration files with `opam lint`_.

      .. _opam lint: https://opam.ocaml.org/doc/man/opam-lint.html

.. supported-language:: Perl

   Flycheck checks Perl with `perl` and `perl-perlcritic`.

   .. syntax-checker:: perl

      Check syntax with the `Perl <https://www.perl.org/>`_ interpreter.

      .. defcustom:: flycheck-perl-include-path

         A list of include directories, relative to the file being checked.

      .. defcustom:: flycheck-perl-module-list

         A list of module names to implicitly use.

   .. syntax-checker:: perl-perlcritic

      Lint and check style with `Perl::Critic`_.

      .. _Perl::Critic: https://metacpan.org/pod/Perl::Critic

      .. defcustom:: flycheck-perlcritic-severity

         The severity level as integer for the ``--severity``.

      .. defcustom:: flycheck-perlcritic-theme

         The theme expression, passed as the ``--theme`` to ``perlcritic``.

      .. syntax-checker-config-file:: flycheck-perlcriticrc

.. supported-language:: PHP

   Flycheck checks PHP with `php`, `php-phpmd` and `php-phpcs`.

   .. syntax-checker:: php

      Check syntax with `PHP CLI`_

      .. _PHP CLI: http://php.net/manual/en/features.commandline.php

   .. syntax-checker:: php-phpmd

      Lint with `PHP Mess Detector <https://phpmd.org/>`_.

      .. defcustom:: flycheck-phpmd-rulesets

         A list of rule sets.  Each item is either the name of a default rule
         set, or the path to a custom rule set file.

   .. syntax-checker:: php-phpcs

      Check style with `PHP Code Sniffer`_.

      .. note::

         This syntax checker requires PHP Code Sniffer 2.6 or newer.

      .. _PHP Code Sniffer: http://pear.php.net/package/PHP_CodeSniffer

      .. defcustom:: flycheck-phpcs-standard

         The coding standard, either as name of a built-in standard, or as path
         to a standard specification.

.. supported-language:: Processing

   .. syntax-checker:: processing

      Check syntax using the `Processing <https://processing.org/>`_ compiler.

.. supported-language:: Protobuf

   .. syntax-checker:: protobuf-protoc

      Check syntax using the protoc_ compiler.

      .. _protoc: https://developers.google.com/protocol-buffers/

      .. defcustom:: flycheck-protoc-import-path

         A list of directories to resolve import directives.  Relative paths are
         relative to the path of the buffer being checked.

   .. syntax-checker:: protobuf-prototool

      Lint with `prototool <https://github.com/uber/prototool>`_.

.. supported-language:: Pug

   .. syntax-checker:: pug

      Check syntax using the `Pug <https://www.pugjs.org>`_ compiler.

.. supported-language:: Puppet

   Flycheck checks Puppet with `puppet-parser` and lints with `puppet-lint`.

   .. syntax-checker:: puppet-parser

      Check syntax with the `Puppet <https://puppet.com/>`_ compiler.

   .. syntax-checker:: puppet-lint

      Link with `Puppet Lint <http://puppet-lint.com/>`_.

      .. defcustom:: flycheck-puppet-lint-disabled-checks

         A list of checks to disable.

      .. syntax-checker-config-file:: flycheck-puppet-lint-rc

.. supported-language:: Python

   Flycheck checks Python with `python-flake8` or `python-pylint`, and falls
   back to `python-pycompile` if neither of those is available.

   All Python checkers are invoked indirectly using ``python -c ...`` (rather
   than a direct call to ``flake8`` or ``pylint``) to make it easier to switch
   between Python 2 and 3.  For example, you can use ``(setq
   flycheck-python-pylint-executable "python3")`` to run ``pylint`` using Python
   3, or ``(defvaralias 'flycheck-python-flake8-executable
   'python-shell-interpreter)`` to run ``flake8`` through the executable pointed
   to by ``python-shell-interpreter``.

   .. note::

      If Flycheck complains about a missing Python checker, make sure that the
      checker is reachable from ``sys.path``, using e.g. ``python -m pylint``:
      often, the issue is that the checker is installed globally but not in the
      current virtualenv.  Alternatively, you can invoke the checker script
      directly, with ``(setq flycheck-python-pylint-executable "pylint")``.

   .. seealso::

      :gh:`flycheck-pyflakes <Wilfred/flycheck-pyflakes>`
         Flycheck extension which adds a syntax checker using `Pyflakes
         <https://github.com/PyCQA/pyflakes>`_.

      :gh:`msherry/flycheck-pycheckers`
         Flycheck extension which can use multiple checkers simultaneously --
         including pyflakes, pep8, flake8, pylint, and mypy 2/3.

   .. syntax-checker:: python-flake8

      Check syntax and lint with `flake8 <https://flake8.readthedocs.io/>`_.

      .. note::

         This syntax checker requires flake8 3.0 or newer.

      .. defcustom:: flycheck-flake8-error-level-alist

         An alist mapping Flake8 error IDs to Flycheck error levels.

      .. defcustom:: flycheck-flake8-maximum-complexity

         The maximum McCabe complexity allowed for methods.

      .. defcustom:: flycheck-flake8-maximum-line-length

         The maximum length of lines.

      .. syntax-checker-config-file:: flycheck-flake8rc

   .. syntax-checker:: python-pyright

      Type check python with  `pyright <https://github.com/microsoft/pyright>`_.

      .. note::

         This syntax checker requires pyright.

   .. syntax-checker:: python-mypy

      Type check python with  `mypy <http://www.mypy-lang.org/>`_.

      .. note::

         This syntax checker requires mypy 0.580 or newer.

      .. syntax-checker-config-file:: flycheck-python-mypy-config

      .. defcustom:: flycheck-python-mypy-cache-dir

         Directory used to write ``.mypy_cache`` directories.

         Set to ``null-device`` to disable writing cache directories
         entirely.

   .. syntax-checker:: python-pylint

      Check syntax and lint with `Pylint <https://pylint.org/>`_.

      .. note::

         This syntax checker requires Pylint 1.0 or newer.

      .. defcustom:: flycheck-pylint-use-symbolic-id

         Whether to report symbolic (e.g. ``no-name-in-module``) or numeric
         (e.g. ``E0611``) message identifiers.

      .. syntax-checker-config-file:: flycheck-pylintrc

   .. syntax-checker:: python-pycompile

      Check syntax with Python's byte compiler (see :py:mod:`py_compile`).

.. supported-language:: R

   .. syntax-checker:: r-lintr

      Check syntax and lint with `lintr <https://github.com/jimhester/lintr>`_.

      .. defcustom:: flycheck-lintr-caching

         Whether to enable caching in lintr.  On by default; it is not
         recommended to disable caching unless it causes actual problems.

      .. defcustom:: flycheck-lintr-linters

         Linters to use as a string with an R expression which selects the
         linters to use.

.. supported-language:: Racket

   .. syntax-checker:: racket

      Check syntax with `raco expand`_ from the ``compiler-lib`` package.

      .. note::

         This syntax checker needs the ``compiler-lib`` package.

      .. _raco expand: http://docs.racket-lang.org/raco/expand.html

.. supported-language:: RPM Spec

   .. syntax-checker:: rpm-rpmlint

      Lint with `rpmlint <https://sourceforge.net/projects/rpmlint/>`_.

.. supported-language:: reStructuredText

   Flycheck checks reStructuredText with `rst-sphinx` in Sphinx_ projects and
   with `rst` otherwise.

   .. _Sphinx: http://sphinx-doc.org/

   .. syntax-checker:: rst-sphinx

      Check documents with Sphinx_.

      .. note::

         This syntax checker requires Sphinx 1.2 or newer.

      .. defcustom:: flycheck-sphinx-warn-on-missing-references

         Whether to emit warnings for all missing references.

   .. syntax-checker:: rst

      Check documents with `docutils <http://docutils.sourceforge.net/>`_.

.. supported-language:: Ruby

   Flycheck checks Ruby with `ruby-rubocop`, `ruby-reek` and `ruby-rubylint`,
   falling back to `ruby` or `ruby-jruby` for basic syntax checking if those
   are not available.

   .. syntax-checker:: ruby-rubocop

      Check syntax and lint with `RuboCop <https://rubocop.org/>`_.

      .. note::

         This syntax checker requires Rubocop 0.34 or newer.

      .. defcustom:: flycheck-rubocop-lint-only

         Whether to suppress warnings about style issues, via the ``--lint``
         option.

      .. syntax-checker-config-file:: flycheck-rubocoprc

   .. syntax-checker:: ruby-standard

      Check syntax and lint with `Ruby Standard <https://github.com/testdouble/standard/>`_.

      .. note::

         This syntax checker and ruby-rubocop are mutually exclusive, since Standard employs an opinionated rubocop config.

      .. defcustom:: flycheck-rubocop-lint-only
         :noindex:

         See `flycheck-rubocop-lint-only`.

      .. syntax-checker-config-file:: flycheck-ruby-standardrc

   .. syntax-checker:: ruby-reek

      Check syntax and lint with reek_.

      .. _Reek: https://github.com/troessner/reek

      .. syntax-checker-config-file:: flycheck-reekrc

      .. note::

         ``flycheck-reekrc`` defaults to ``nil``, because Reek can find its own
         configuration.

   .. syntax-checker:: ruby-rubylint

      Check syntax and lint with ruby-lint_.

      .. note::

         This syntax checker requires ruby-lint 2.0.2 or newer.

      .. _ruby-lint: http://code.yorickpeterse.com/ruby-lint/latest/

      .. syntax-checker-config-file:: flycheck-rubylintrc

   .. syntax-checker:: ruby

      Check syntax with the `Ruby <https://www.ruby-lang.org/>`_ interpreter.

   .. syntax-checker:: ruby-jruby

      Check syntax with the `JRuby <http://jruby.org/>`_ interpreter.

.. supported-language:: Rust

   Flycheck checks Rust_ with `rust-cargo` in Cargo projects, or `rust`
   otherwise.  For Cargo projects, you can also use the clippy_ linter with
   `rust-clippy`.

   .. _Rust: https://www.rust-lang.org/
   .. _clippy: https://github.com/rust-lang-nursery/rust-clippy

   .. syntax-checker:: rust-cargo
                       rust
                       rust-clippy

      Check syntax and types with the Rust_ compiler.  In a Cargo_ project the
      compiler is invoked through ``cargo check`` to take Cargo dependencies
      into account.

      `rust-clippy` has no configurable options.

      .. note::

         `rust-cargo` requires Rust 1.17 or newer.
         `rust` requires Rust 1.18 or newer.
         `rust-clippy` requires the nightly version of Rust.

      .. _Cargo: http://doc.crates.io/index.html

      .. seealso::

         :flyc:`flycheck-rust`
            Flycheck extension to configure Rust syntax checkers according to
            the current Cargo_ project.

      .. defcustom:: flycheck-rust-args

         A list of additional arguments that are passed to rustc.  This option
         is ignored by `rust-cargo`.

      .. defcustom:: flycheck-cargo-check-args

         A list of additional arguments passed to the ``cargo check``
         subcommand.

      .. defcustom:: flycheck-rust-check-tests

         Whether to check test code in Rust.

      .. defcustom:: flycheck-rust-crate-root

         A path to the crate root for the current buffer, or nil if the current
         buffer is a crate by itself.

         `rust-cargo` ignores this option as the crate root is given by Cargo.

      .. defcustom:: flycheck-rust-crate-type

         For `rust-cargo`, the target type as a string, one of ``lib``, ``bin``,
         ``example``, ``test`` or ``bench``.  Can also be nil for projects with
         a single target.

         For `rust`, the type of the crate to check, as a string for the
         ``--crate-type`` option.

      .. defcustom:: flycheck-rust-binary-name

         The name of the binary to pass to ``cargo check --TARGET-TYPE``, as a
         string.

         For `rust-cargo`, always required unless `flycheck-rust-crate-type` is
         ``lib`` or nil, in which case it is ignored.

         Ignored by `rust`.

      .. defcustom:: flycheck-rust-features

         List of features to activate during build or check.

         The value of this variable is a list of strings denoting features
         that will be activated to build the target to check. Features will
         be passed to ``cargo check --features=FEATURES``.

         Empty by default.

         Ignored by `rust`.

      .. defcustom:: flycheck-rust-library-path

         A list of additional library directories. Relative paths are relative
         to the buffer being checked.

.. supported-language:: Sass/SCSS

   Flycheck checks SASS with `sass/scss-sass-lint`, falling back to `sass`, and
   SCSS with  `scss-lint` or `scss-stylelint` falling back to
   `sass/scss-sass-lint` first and then `scss` if neither is available.

   .. syntax-checker:: scss-lint

      Syntax-check and lint SCSS with SCSS-Lint_.

      .. note::

         This syntax checker requires SCSS-Lint 0.43.2 or newer.

      .. _SCSS-Lint: https://github.com/brigade/scss-lint

      .. syntax-checker-config-file:: flycheck-scss-lintrc

   .. syntax-checker:: sass/scss-sass-lint

      Syntax-check and lint Sass/SCSS with SASS-Lint_.

      .. _SASS-Lint: https://github.com/sasstools/sass-lint

      .. syntax-checker-config-file:: flycheck-sass-lintrc

   .. syntax-checker:: scss-stylelint

      Syntax-check and lint SCSS with stylelint_.

      .. _stylelint: https://stylelint.io

      .. syntax-checker-config-file:: flycheck-stylelintrc

      .. defcustom:: flycheck-stylelint-quiet

         Whether to run stylelint in quiet mode via ``--quiet``.

   .. syntax-checker:: sass
                       scss

      Check SASS and SCSS respectively with the `SCSS compiler
      <http://sass-lang.com/>`_.

      .. defcustom:: flycheck-sass-compass
                     flycheck-scss-compass

         Whether to enable the Compass CSS framework with ``--compass``.

.. supported-language:: Scala

   Flycheck checks Scala with `scala` and `scala-scalastyle`.

   .. syntax-checker:: scala

      Check syntax and types with the `Scala <https://www.scala-lang.org/>`_
      compiler.

      .. note::

         This syntax checker is fairly primitive.  For a better Scala experience
         we recommend Ensime_.

         .. _Ensime: http://ensime.github.io/

   .. syntax-checker:: scala-scalastyle

      Check style with `Scalastyle <http://www.scalastyle.org/>`_.

      .. syntax-checker-config-file:: flycheck-scalastylerc

      .. important::

         A configuration file is mandatory for this syntax checker.  If
         `flycheck-scalastylerc` is not set or the configuration file not found
         this syntax checker will not be applied.

.. supported-language:: Scheme

   Flycheck checks CHICKEN Scheme files with ``csc``.

   .. syntax-checker:: scheme-chicken

      Check syntax with ``csc``, the `CHICKEN Scheme <http://call-cc.org/>`_
      compiler.

      .. defcustom:: flycheck-scheme-chicken-args

     A list of additional options.

   .. important::

      `Geiser <http://www.nongnu.org/geiser/>`_ must be installed and active for
      this checker to work.

.. supported-language:: Shell scripting languages

   Flycheck checks various shell scripting languages:

   * Bash with `sh-bash` and `sh-shellcheck`
   * POSIX shell (i.e. :file:`/bin/sh`) with `sh-posix-dash` or `sh-posix-bash`
   * Zsh with `sh-zsh`

   .. syntax-checker:: sh-bash

      Check Bash_ syntax.

      .. _Bash: http://www.gnu.org/software/bash/

      .. defcustom:: flycheck-sh-bash-args

         A list of additional arguments that are passed to bash.

   .. syntax-checker:: sh-posix-dash

      Check POSIX shell syntax with Dash_.

      .. _Dash: http://gondor.apana.org.au/~herbert/dash/

   .. syntax-checker:: sh-posix-bash

      Check POSIX shell syntax with Bash_.

   .. syntax-checker:: sh-zsh

      Check `Zsh <http://www.zsh.org/>`_ syntax.

   .. syntax-checker:: sh-shellcheck

      Lint Bash and POSIX shell with ShellCheck_.

      .. _ShellCheck: https://github.com/koalaman/shellcheck/

      .. defcustom:: flycheck-shellcheck-excluded-warnings

         A list of excluded warnings.

      .. defcustom:: flycheck-shellcheck-follow-sources

         Allow shellcheck to read sourced files.

.. supported-language:: Slim

   .. syntax-checker:: slim

      Check Slim using the `Slim <http://slim-lang.com/>`_ compiler.

   .. syntax-checker:: slim-lint

      Check Slim best practices using the `slim-lint
      <https://github.com/sds/slim-lint>`_ linter.

.. supported-language:: SQL

   .. syntax-checker:: sql-sqlint

      Check SQL syntax with `Sqlint <https://github.com/purcell/sqlint>`_.

.. supported-language:: systemd Unit Configuration

   .. syntax-checker:: systemd-analyze

      Check systemd unit configuration file syntax with `systemd-analyze`_.

      .. _systemd-analyze: https://www.freedesktop.org/software/systemd/man/systemd-analyze.html

.. supported-language:: Tcl

   .. syntax-checker:: tcl-nagelfar

      Check Tcl syntax with `Nagelfar <http://nagelfar.sourceforge.net/>`_.

.. supported-language:: Terraform

   .. syntax-checker:: terraform

      Check Terraform syntax with `terraform fmt`_

      .. _terraform fmt: https://www.terraform.io/docs/commands/fmt.html

   .. syntax-checker:: terraform-tflint

      Check Terraform with `tflint <https://github.com/wata727/tflint>`_

      .. defcustom:: flycheck-tflint-variable-files

         A list of files to resolve terraform variables.  Relative paths are
         relative to the path of the buffer being checked.

.. supported-language:: Text

   .. syntax-checker:: proselint

      Check English prose with `Proselint <http://proselint.com/>`_.

   .. syntax-checker:: textlint

      Check prose with `textlint <https://textlint.github.io/>`_.

      .. syntax-checker-config-file:: flycheck-textlint-config

      .. defcustom:: flycheck-textlint-plugin-alist

         An alist mapping major modes to textlint plugins.

         Flycheck currently supports the following textlint plugins on NPM:

         * textlint-plugin-rst
         * textlint-plugin-html
         * textlint-plugin-latex
         * textlint-plugin-asciidoctor (as well as other AsciiDoc plugins)

      .. note::

         textlint plugins need to be installed separately.

.. supported-language:: TeX/LaTeX

   Flycheck checks TeX and LaTeX with either `tex-chktex` or `tex-lacheck`.

   .. syntax-checker:: tex-chktex

      Check style with `ChkTeX <http://www.nongnu.org/chktex/>`_.

      .. syntax-checker-config-file:: flycheck-chktexrc

   .. syntax-checker:: tex-lacheck

      Check style with `Lacheck <http://www.ctan.org/pkg/lacheck>`_.

.. supported-language:: Texinfo

   .. syntax-checker:: texinfo

      Check syntax with :program:`makeinfo` from Texinfo_.

      .. _Texinfo: http://www.gnu.org/software/texinfo/

.. supported-language:: TypeScript

   .. syntax-checker:: typescript-tslint

      Check syntax and style with `TSLint <https://github.com/palantir/tslint>`_.

      .. syntax-checker-config-file:: flycheck-typescript-tslint-config

      .. defcustom:: flycheck-typescript-tslint-rulesdir

         Additional rules directory, for user created rules.

      .. defcustom:: flycheck-tslint-args

         A list of additional arguments that are passed to tslint.

.. supported-language:: Verilog

   .. syntax-checker:: verilog-verilator

      Check syntax with `Verilator <https://www.veripool.org/wiki/verilator>`_.

      .. defcustom:: flycheck-verilator-include-path

         A list of include directories.  Relative paths are relative to the file
         being checked.

.. supported-language:: VHDL

   .. syntax-checker:: vhdl-ghdl

      Check syntax with `GHDL <https://github.com/ghdl/ghdl>`_.

      .. defcustom:: flycheck-ghdl-language-standard

         The language standard to use as string.

      .. defcustom:: flycheck-ghdl-workdir

         The directory to use for the file library.

      .. defcustom:: flycheck-ghdl-ieee-library

         The standard to use for the IEEE library.

.. supported-language:: XML

   Flycheck checks XML with `xml-xmlstarlet` or `xml-xmllint`.

   .. syntax-checker:: xml-xmlstarlet

      Check syntax with `XMLStarlet <http://xmlstar.sourceforge.net>`_.

      .. defcustom:: flycheck-xml-xmlstarlet-xsd-path
                     flycheck-xml-xmllint-xsd-path

         Location of XSD schema to validate against for `xml-xmlstarlet` and
         `xml-xmllint` respectively.

   .. syntax-checker:: xml-xmllint

      Check syntax with :program:`xmllint` from Libxml2_.

      .. _Libxml2: http://www.xmlsoft.org/

.. supported-language:: YAML

   Flycheck checks YAML with `yaml-jsyaml`, `yaml-ruby` or 'yaml-yamllint'.

   .. syntax-checker:: yaml-jsyaml

      Check syntax with `js-yaml <https://github.com/nodeca/js-yaml>`_.

   .. syntax-checker:: yaml-ruby

      Check syntax with Ruby's YAML parser.

   .. syntax-checker:: yaml-yamllint

      Check syntax with yamllint.

      .. syntax-checker-config-file:: flycheck-yamllintrc
