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

      .. _GNAT: http://libre.adacore.com/tools/gnat-gpl-edition

      .. option:: flycheck-gnat-args

         A list of additional options.

      .. option:: flycheck-gnat-include-path

         A list of include directories.  Relative paths are relative to the path
         of the buffer being checked.

      .. option:: flycheck-gnat-language-standard

         The language standard to use as string.

      .. option:: flycheck-gnat-warnings

         A list of additional warnings to enable.  Each item is the name of a
         warning category to enable.

.. supported-language:: AsciiDoc

   .. syntax-checker:: asciidoc

      Check AsciiDoc_ with the standard AsciiDoc processor.

      .. _AsciiDoc: http://www.methods.co.nz/asciidoc

.. supported-language:: C/C++
   :index_as: C
              C++

   Flycheck checks C and C++ with either `c/c++-clang` or `c/c++-gcc`, and then
   with `c/c++-cppcheck`.

   .. syntax-checker:: c/c++-clang
                       c/c++-gcc

      Check C/C++ for syntax and type errors with Clang_ or GCC_ respectively.

      .. _Clang: http://clang.llvm.org/
      .. _GCC: https://gcc.gnu.org/

      .. option:: flycheck-clang-args
                  flycheck-gcc-args

         A list of additional arguments for `c/c++-clang` and `c/c++-gcc`
         respectively.

      .. option:: flycheck-clang-blocks

         Whether to enable blocks in `c/c++-clang`.

      .. option:: flycheck-clang-definitions
                  flycheck-gcc-definitions

         A list of additional preprocessor definitions for `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. option:: flycheck-clang-include-path
                  flycheck-gcc-include-path

         A list of include directories for `c/c++-clang` and `c/c++-gcc`
         respectively, relative to the file being checked.

      .. option:: flycheck-clang-includes
                  flycheck-gcc-includes

         A list of additional include files for `c/c++-clang` and `c/c++-gcc`
         respectively, relative to the file being checked.

      .. option:: flycheck-clang-language-standard
                  flycheck-gcc-language-standard

         The language standard to use in `c/c++-clang` and `c/c++-gcc`
         respectively as string, via the ``-std`` option.

      .. option:: flycheck-clang-ms-extensions

         Whether to enable Microsoft extensions to C/C++ in `c/c++-clang`.

      .. option:: flycheck-clang-no-exceptions
                  flycheck-gcc-no-exceptions

         Whether to disable exceptions in `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. option:: flycheck-clang-no-rtti
                  flycheck-gcc-no-rtti

         Whether to disable RTTI in `c/c++-clang` and `c/c++-gcc` respectively,
         via ``-fno-rtti``.

      .. option:: flycheck-clang-standard-library

         The name of the standard library to use for `c/c++-clang`, as string.

      .. option:: flycheck-gcc-openmp

         Whether to enable OpenMP in `c/c++-gcc`.

      .. option:: flycheck-clang-pedantic
                  flycheck-gcc-pedantic

         Whether to warn about language extensions in `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. option:: flycheck-clang-pedantic-errors
                  flycheck-gcc-pedantic-errors

         Whether to error on language extensions in `c/c++-clang` and
         `c/c++-gcc` respectively.

      .. option:: flycheck-clang-warnings
                  flycheck-gcc-warnings

         A list of additional warnings to enable in `c/c++-clang` and
         `c/c++-gcc` respectively.  Each item is the name of a warning or
         warning category for ``-W``.

   .. syntax-checker:: c/c++-cppcheck

      Check C/C++ for semantic and stylistic issues with cppcheck_.

      .. _cppcheck: http://cppcheck.sourceforge.net/

      .. option:: flycheck-cppcheck-checks

         A list of enabled checks.  Each item is the name of a check for the
         ``--enable`` option.

      .. option:: flycheck-cppcheck-inconclusive

         Whether to enable inconclusive checks.  These checks may yield more
         false positives than normal checks.

      .. option:: flycheck-cppcheck-include-path

         A list of include directories.  Relative paths are relative to the file
         being checked.

      .. option:: flycheck-cppcheck-language-standard

         The C or C++ language standard to use via ``--std=``.

.. supported-language:: CFEngine

   .. syntax-checker:: cfengine

      Check syntax with `CFEngine <http://cfengine.com/>`_.

.. supported-language:: Chef

   .. syntax-checker:: chef-foodcritic

      Check style in Chef recipes with `foodcritic <http://www.foodcritic.io>`_.

      .. option:: flycheck-foodcritic-tags

         A list of tags to select.

.. supported-language:: Coffeescript

   Flycheck checks Coffeescript syntax with `coffee` and then lints with
   `coffee-coffeelint`.

   .. syntax-checker:: coffee

      Check syntax with the `Coffeescript <http://coffeescript.org/>`_ compiler.

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

.. supported-language:: D

   .. syntax-checker:: d-dmd

      Check syntax and types with (`DMD <http://dlang.org/>`_).

      .. option:: flycheck-dmd-include-path

         A list of include directories.

      .. option:: flycheck-dmd-args

         A list of additional arguments.

   .. seealso::

      :flyc:`flycheck-d-unittest`
         Flycheck extension which provides a syntax checker to run D unittests
         on the fly and report the results with Flycheck.

.. supported-language:: Emacs Lisp

   Flycheck checks Emacs Lisp with `emacs-lisp` and then with
   `emacs-lisp-checkdoc`.

   .. syntax-checker:: emacs-lisp

      Check syntax with the built-in byte compiler.

      .. option:: flycheck-emacs-lisp-load-path

         The load path as list of strings.  Relative directories are expanded
         against the `default-directory` of the buffer being checked.

      .. option:: flycheck-emacs-lisp-initialize-packages

         Whether to initialize Emacs' package manager with `package-initialize`
         before checking the buffer.  If set to :elisp:`auto` (the default),
         only initialize the package managers when checking files under
         `user-emacs-directory`.

      .. option:: flycheck-emacs-lisp-package-user-dir

         The package directory as string.  Has no effect if
         `flycheck-emacs-lisp-initialize-packages` is nil.

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

.. supported-language:: Erlang

   .. syntax-checker:: erlang

      Check Erlang with the standard `Erlang <http://www.erlang.org/>`_
      compiler.

      .. option:: flycheck-erlang-include-path

         A list of include directories.

      .. option:: flycheck-erlang-library-path

         A list of library directories.

.. supported-language:: ERuby

   .. syntax-checker:: eruby-erubis

      Check ERuby with `erubis <http://www.kuwata-lab.com/erubis/>`_.

.. supported-language:: Fortran

   .. syntax-checker:: fortran-gfortran

      Check Fortran syntax and type with GFortran_.

      .. _GFortran: https://gcc.gnu.org/onlinedocs/gfortran/

      .. option:: flycheck-gfortran-args

         A list of additional arguments.

      .. option:: flycheck-gfortran-include-path

         A list of include directories.  Relative paths are relative to the file
         being checked.

      .. option:: flycheck-gfortran-language-standard

         The language standard to use via the ``-std`` option.

      .. option:: flycheck-gfortran-layout

         The source code layout to use.  Set to :elisp:`free` or :elisp:`fixed`
         for free or fixed layout respectively, or nil (the default) to let
         GFortran automatically determine the layout.

      .. option:: flycheck-gfortran-warnings

         A list of warnings enabled via the ``-W`` option.

.. supported-language:: Go

   Flycheck checks Go with the following checkers:

   1. `go-gofmt`
   2. `go-golint`
   3. `go-vet`
   4. `go-build` or `go-test`
   5. `go-errcheck`

   .. syntax-checker:: go-gofmt

      Check Go syntax with `gofmt <https://golang.org/cmd/gofmt/>`_.

   .. syntax-checker:: go-golint

      Check Go code style with `Golint <https://github.com/golang/lint>`_.

   .. syntax-checker:: go-vet

      Check Go for suspicious code with vet_.

      .. option:: flycheck-go-vet-print-functions

         A list of print-like functions to check calls for format string problems.

      .. option:: flycheck-go-vet-shadow

         Whether to check for shadowed variables, in Go 1.6 or newer.

      .. _vet: https://golang.org/cmd/vet/

   .. syntax-checker:: go-build

      Check syntax and type with the `Go compiler`_.

      .. _Go compiler: https://golang.org/cmd/go

      .. option:: flycheck-go-build-install-deps

         Whether to install dependencies while checking.

      .. option:: flycheck-go-build-tags

         A list of build tags.

   .. syntax-checker:: go-test

      Check syntax and types of Go tests with the `Go compiler`_.

   .. syntax-checker:: go-errcheck

      Check for unhandled error returns in Go with errcheck_.

      .. _errcheck: https://github.com/kisielk/errcheck

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

      .. option:: flycheck-ghc-args

         A list of additional arguments.

      .. option:: flycheck-ghc-no-user-package-database

         Whether to disable the user package database (only for `haskell-ghc`).

      .. option:: flycheck-ghc-stack-use-nix

         Whether to enable Nix support for Stack (only for `haskell-stack-ghc`).

      .. option:: flycheck-ghc-package-databases

         A list of additional package databases for GHC (only for
         `haskell-ghc`).  Each item points to a directory containing a package
         directory, via ``-package-db``.

      .. option:: flycheck-ghc-search-path

         A list of module directories, via ``-i``.

      .. option:: flycheck-ghc-language-extensions

         A list of language extensions, via ``-X``.

   .. syntax-checker:: haskell-hlint

      Lint with `hlint <https://github.com/ndmitchell/hlint>`_.

      .. option:: flycheck-hlint-args

         A list of additional arguments.

      .. option:: flycheck-hlint-language-extensions

         A list of language extensions to enable.

      .. option:: flycheck-hlint-ignore-rules

         A list of rules to ignore.

      .. option:: flycheck-hlint-hint-packages

         A list of additional hint packages to include.

      .. syntax-checker-config-file:: flycheck-hlintrc

.. supported-language:: HTML

   .. syntax-checker:: html-tidy

      Check HTML syntax and style with `Tidy HTML5`_.

      .. _Tidy HTML5: https://github.com/htacg/tidy-html5

      .. syntax-checker-config-file:: flycheck-tidyrc

.. supported-language:: Jade

   .. syntax-checker:: jade

      Check syntax using the `Jade <http://jade-lang.com/>`_ compiler.

.. supported-language:: Javascript

   Flycheck checks Javascript with one of `javascript-eslint`,
   `javascript-jshint` or `javascript-gjslint`, and then with `javascript-jscs`.

   Alternatively `javascript-standard` is used instead all of the former ones.

   .. syntax-checker:: javascript-eslint

      Check syntax and lint with `ESLint <http://eslint.org/>`_.

      .. option:: flycheck-eslint-rulesdir

         A directory with custom rules.

      .. syntax-checker-config-file:: flycheck-eslintrc

   .. syntax-checker:: javascript-jshint

      Check syntax and lint with `JSHint <http://jshint.com/>`_.

      .. option:: flycheck-jshint-extract-javascript

         Whether to extract Javascript from HTML before linting.

      .. syntax-checker-config-file:: flycheck-jshintrc

   .. syntax-checker:: javascript-gjslint

      Lint with `Closure Linter`_.

      .. _Closure Linter: https://developers.google.com/closure/utilities

      .. syntax-checker-config-file:: flycheck-gjslintrc

   .. syntax-checker:: javascript-jscs

      Check code style with `JSCS <http://jscs.info/>`_.

      .. syntax-checker-config-file:: flycheck-jscsrc

   .. syntax-checker:: javascript-standard

      Check syntax and code style with Standard_ or Semistandard_.

      .. _Standard: https://github.com/feross/standard
      .. _Semistandard: https://github.com/Flet/semistandard

.. supported-language:: JSON

   Flycheck checks JSON with `json-jsonlint` or `json-python-json`.

   .. syntax-checker:: json-jsonlint

      Check JSON with `jsonlint <https://github.com/zaach/jsonlint>`_.

   .. syntax-checker:: json-python-json

      Check JSON with Python's built-in :py:mod:`json` module.

.. supported-language:: Less

   .. syntax-checker:: less

      Check syntax with the `Less <http://lesscss.org/>`_ compiler.

.. supported-language:: Lua

   Flycheck checks Lua with `luacheck`, falling back to `lua`.

   .. syntax-checker:: luacheck

      Check syntax and lint with Luacheck_.

      .. _Luacheck: https://github.com/mpeterv/luacheck

      .. syntax-checker-config-file:: flycheck-luacheckrc

   .. syntax-checker:: lua

      Check syntax with the `Lua compiler <http://www.lua.org/>`_.

.. supported-language:: Perl

   Flycheck checks Perl with `perl` and `perl-perlcritic`.

   .. syntax-checker:: perl

      Check syntax with the `Perl <https://www.perl.org/>`_ interpreter.

      .. option:: flycheck-perl-include-path

         A list of include directories, relative to the file being checked.

   .. syntax-checker:: perl-perlcritic

      Lint and check style with `Perl::Critic`_.

      .. _Perl::Critic: https://metacpan.org/pod/Perl::Critic

      .. option:: flycheck-perlcritic-severity

         The severity level as integer for the ``--severity``.

      .. syntax-checker-config-file:: flycheck-perlcriticrc

.. supported-language:: PHP

   Flycheck checks PHP with `php`, `php-phpmd` and `php-phpcs`.

   .. syntax-checker:: php

      Check syntax with `PHP CLI`_

      .. _PHP CLI: http://php.net/manual/en/features.commandline.php

   .. syntax-checker:: php-phpmd

      Lint with `PHP Mess Detector <https://phpmd.org/>`_.

      .. option:: flycheck-phpmd-rulesets

         A list of rule sets.  Each item is either the name of a default rule
         set, or the path to a custom rule set file.

   .. syntax-checker:: php-phpcs

      Check style with `PHP CodeSniffer`_.

      .. _PHP CodeSniffer: https://github.com/squizlabs/PHP_CodeSniffer

      .. option:: flycheck-phpcs-standard

         The coding standard, either as name of a built-in standard, or as path
         to a standard specification.

.. supported-language:: Processing

   .. syntax-checker:: processing

      Check syntax using the `Processing <https://processing.org/>`_ compiler.

.. supported-language:: Puppet

   Flycheck checks Puppet with `puppet-parser` and lints with `puppet-lint`.

   .. syntax-checker:: puppet-parser

      Check syntax with the `Puppet <https://puppetlabs.com/>`_ compiler.

   .. syntax-checker:: puppet-lint

      Link with `Puppet Lint <http://puppet-lint.com/>`_.

      .. option:: flycheck-puppet-lint-disabled-checks

         A list of checks to disable.

      .. syntax-checker-config-file:: flycheck-puppet-lint-rc

.. supported-language:: Python

   Flycheck checks Python with `python-flake8` or `python-pylint`, and falls
   back to `python-pycompile` if neither of those is available.

   .. seealso::

      :gh:`flycheck-pyflakes <Wilfred/flycheck-pyflakes>`
         Flycheck extension which adds a syntax checker using `Pyflakes
         <https://github.com/pyflakes/pyflakes>`_.

   .. syntax-checker:: python-flake8

      Check syntax and lint with `flake8 <https://flake8.readthedocs.org/>`_.

      .. option:: flycheck-flake8-error-level-alist

         An alist mapping Flake8 error IDs to Flycheck error levels.

      .. option:: flycheck-flake8-maximum-complexity

         The maximum McCabe complexity allowed for methods.

      .. option:: flycheck-flake8-maximum-line-length

         The maximum length of lines.

      .. syntax-checker-config-file:: flycheck-flake8rc

   .. syntax-checker:: python-pylint

      Check syntax and lint with `Pylint <https://pylint.org/>`_.

      .. option:: flycheck-pylint-use-symbolic-id

         Whether to report symbolic (e.g. ``no-name-in-module``) or numeric
         (e.g. ``E0611``) message identifiers.

      .. syntax-checker-config-file:: flycheck-pylintrc

   .. syntax-checker:: python-pycompile

      Check syntax with Python's byte compiler (see :py:mod:`py_compile`).

.. supported-language:: R

   .. syntax-checker:: r-lintr

      Check syntax and lint with `lintr <https://github.com/jimhester/lintr>`_.

      .. option:: flycheck-lintr-caching

         Whether to enable caching in lintr.  On by default; it is not
         recommended to disable caching unless it causes actual problems.

      .. option:: flycheck-lintr-linters

         Linters to use as a string with an R expression which selects the
         linters to use.

.. supported-language:: Racket

   .. syntax-checker:: racket

      Check syntax with `raco expand`_ from the ``compiler-lib`` package.

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

      .. option:: flycheck-sphinx-warn-on-missing-references

         Whether to emit warnings for all missing references.

   .. syntax-checker:: rst

      Check documents with `docutils <http://docutils.sourceforge.net/>`_.

.. supported-language:: Ruby

   Flycheck checks Ruby with `ruby-rubocop` and `ruby-rubylint`, falling back to
   `ruby` or `ruby-jruby` for basic syntax checking if those are not available.

   .. syntax-checker:: ruby-rubocop

      Check syntax and lint with `RuboCop <http://batsov.com/rubocop/>`_.

      .. option:: flycheck-rubocop-lint-only

         Whether to suppress warnings about style issues, via the ``--lint``
         option.

      .. syntax-checker-config-file:: flycheck-rubocoprc

   .. syntax-checker:: ruby-rubylint

      Check syntax and lint with ruby-lint_.

      .. _ruby-lint: http://code.yorickpeterse.com/ruby-lint/latest/

      .. syntax-checker-config-file:: flycheck-rubylintrc

   .. syntax-checker:: ruby

      Check syntax with the `Ruby <https://www.ruby-lang.org/>`_ interpreter.

   .. syntax-checker:: ruby-jruby

      Check syntax with the `JRuby <http://jruby.org/>`_ interpreter.

.. supported-language:: Rust

   Flycheck checks Rust_ with `rust-cargo` in Cargo projects, or `rust`
   otherwise.

   .. _Rust: https://www.rust-lang.org/

   .. syntax-checker:: rust-cargo
                       rust

      Check syntax and types with the Rust_ compiler.  In a Cargo_ project the
      compiler is invoked through ``cargo rustc`` to take Cargo dependencies
      into account.

      .. _Cargo: http://doc.crates.io/index.html

      .. seealso::

         :flyc:`flycheck-rust`
            Flycheck extension to configure Rust syntax checkers according to
            the current Cargo_ project.

      .. option:: flycheck-rust-args

         A list of additional arguments.

      .. option:: flycheck-rust-check-tests

         Whether to check test code in Rust.

      .. option:: flycheck-rust-crate-root

         A path to the crate root for the current buffer, or nil if the current
         buffer is a crate by itself.

         `rust-cargo` ignores this option as the crate root is given by Cargo.

      .. option:: flycheck-rust-crate-type

         The type of the crate to check, as string for the ``--crate-type``
         option.

      .. option:: flycheck-rust-library-path

         A list of additional library directories. Relative paths are relative
         to the buffer being checked.

.. supported-language:: Sass

   .. syntax-checker:: sass

      Check syntax with the `Sass <http://sass-lang.com/>`_ compiler.

      .. option:: flycheck-sass-compass

         Whether to enable the Compass CSS framework via ``--compass``.

.. supported-language:: Scala

   Flycheck checks Scala with `scala` and `scala-scalastyle`.

   .. syntax-checker:: scala

      Check syntax and types with the `Scala <http://www.scala-lang.org/>`_
      compiler.

      .. note::

         This syntax checker is fairly primitive.  For a better Scala experience
         we recommend Ensime_.

         .. _Ensime: http://ensime.github.io

   .. syntax-checker:: scala-scalastyle

      Check style with `Scalastyle <http://www.scalastyle.org/>`_.

      .. syntax-checker-config-file:: flycheck-scalastylerc

      .. important::

         A configuration file is mandatory for this syntax checker.  If
         `flycheck-scalastylerc` is not set or the configuration file not found
         this syntax checker will not be applied.

.. supported-language:: SCSS

   Flycheck checks SCSS with `scss-lint`, falling back to `scss`.

   .. syntax-checker:: scss-lint

      Check syntax and lint with SCSS-Lint_.

      .. _SCSS-Lint: https://github.com/brigade/scss-lint

      .. syntax-checker-config-file:: flycheck-scss-lintrc

   .. syntax-checker:: scss

      Check syntax with the `SCSS compiler <http://sass-lang.com/>`_.

      .. option:: flycheck-scss-compass

         Whether to enable the Compass CSS framework with ``--compass``.

.. supported-language:: Shell scripting languages

   Flycheck checks various shell scripting languages:

   * Bash with `sh-bash` and `sh-shellcheck`
   * POSIX shell (i.e. :file:`/bin/sh`) with `sh-posix-dash` or `sh-posix-bash`
   * Zsh with `sh-zsh`

   .. syntax-checker:: sh-bash

      Check Bash_ syntax.

      .. _Bash: http://www.gnu.org/software/bash/

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

      .. option:: flycheck-shellcheck-excluded-warnings

         A list of excluded warnings.

.. supported-language:: Slim

   .. syntax-checker:: slim

      Check Slim using the `Slim <http://slim-lang.com/>`_ compiler.

.. supported-language:: SQL

   .. syntax-checker:: sql-sqlint

      Check SQL syntax with `Sqlint <https://github.com/purcell/sqlint>`_.

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

.. supported-language:: Verilog

   .. syntax-checker:: verilog-verilator

      Check syntax with `Verilator <http://www.veripool.org/wiki/verilator>`_.

      .. option:: flycheck-verilator-include-path

         A list of include directories.  Relative paths are relative to the file
         being checked.

.. supported-language:: XML

   Flycheck checks XML with `xml-xmlstarlet` or `xml-xmllint`.

   .. syntax-checker:: xml-xmlstarlet

      Check syntax with `XMLStarlet <http://xmlstar.sourceforge.net>`_.

   .. syntax-checker:: xml-xmllint

      Check syntax with :program:`xmllint` from Libxml2_.

      .. _Libxml2: http://www.xmlsoft.org/

.. supported-language:: YAML

   Flycheck checks YAML with `yaml-jsyaml` or `yaml-ruby`.

   .. syntax-checker:: yaml-jsyaml

      Check syntax with `js-yaml <https://github.com/nodeca/js-yaml>`_.

   .. syntax-checker:: yaml-ruby

      Check syntax with Ruby's YAML parser.
