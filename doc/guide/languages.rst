.. _supported-languages:

=====================
 Supported languages
=====================

.. require:: flycheck

Flycheck supports the following |#flycheck-languages| languages with
|#flycheck-checkers| syntax checkers.

Each language has one or more syntax checkers, whose names follow a
:varcode:`{language}-{tool}` convention, where :var:`language` is the
programming language supported by this syntax checker, and :var:`tool` the
specific syntax checker tool.

There are some 3rd party extensions which add support for more languages:

- flycheck-ledger_ adds a syntax checker for the Ledger_ accounting tool.
- flycheck-mercury_ adds a syntax checker for the Mercury_ language.

.. _flycheck-ledger: https://github.com/purcell/flycheck-ledger
.. _Ledger: http://ledger-cli.org/
.. _flycheck-mercury: https://github.com/flycheck/flycheck-mercury
.. _Mercury: http://mercurylang.org/

.. contents:: Supported languages
   :local:

Ada
===

.. flyc-checker:: ada-gnat
   :auto:

   .. rubric:: Options

   .. option:: flycheck-gnat-include-path
      :auto:

   .. option:: flycheck-gnat-language-standard
      :auto:

   .. option:: flycheck-gnat-warnings
      :auto:

AsciiDoc
========

.. flyc-checker:: asciidoc
   :auto:

C/C++
=====

.. flyc-checker:: c/c++-clang
   :auto:

   .. rubric:: Options

   .. option:: flycheck-clang-blocks
      :auto:

   .. option:: flycheck-clang-definitions
      :auto:

   .. option:: flycheck-clang-include-path
      :auto:

   .. option:: flycheck-clang-includes
      :auto:

   .. option:: flycheck-clang-language-standard
      :auto:

   .. option:: flycheck-clang-ms-extensions
      :auto:

   .. option:: flycheck-clang-no-exceptions
      :auto:

   .. option:: flycheck-clang-no-rtti
      :auto:

   .. option:: flycheck-clang-standard-library
      :auto:

   .. option:: flycheck-clang-warnings
      :auto:

.. flyc-checker:: c/c++-gcc
   :auto:

   .. rubric:: Options

   .. option:: flycheck-gcc-definitions
      :auto:

   .. option:: flycheck-gcc-include-path
      :auto:

   .. option:: flycheck-gcc-includes
      :auto:

   .. option:: flycheck-gcc-language-standard
      :auto:

   .. option:: flycheck-gcc-no-exceptions
      :auto:

   .. option:: flycheck-gcc-no-rtti
      :auto:

   .. option:: flycheck-gcc-warnings
      :auto:

.. flyc-checker:: c/c++-cppcheck
   :auto:

   .. rubric:: Options

   .. option:: flycheck-cppcheck-checks
      :auto:

   .. option:: flycheck-cppcheck-inconclusive
      :auto:

.. seealso::

   The flycheck-google-cpplint_ extension adds a syntax checker using Google's
   C++ style checker.

.. _flycheck-google-cpplint: https://github.com/flycheck/flycheck-google-cpplint

CFEngine
========

.. flyc-checker:: cfengine
   :auto:

Chef
====

.. seealso:: Chef_

.. flyc-checker:: chef-foodcritic
   :auto:

.. _Chef: http://www.getchef.com/chef/

Coffeescript
============

.. flyc-checker:: coffee
   :auto:

.. flyc-checker:: coffee-coffeelint
   :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-coffeelintrc
      :auto:

Coq
===

.. flyc-checker:: coq
   :auto:

CSS
===

.. flyc-checker:: css-csslint
   :auto:

D
=

.. flyc-checker:: d-dmd
   :auto:

   .. rubric:: Options

   .. option:: flycheck-dmd-include-path
      :auto:

.. seealso::

   The flycheck-d-unittest_ extension adds a syntax checker that runs D unit
   tests on the fly.

.. _flycheck-d-unittest: https://github.com/flycheck/flycheck-d-unittest

Elixir
======

.. flyc-checker:: elixir
   :auto:

Emacs Lisp
==========

.. seealso:: `Emacs`_, `Emacs Lisp reference`_

.. flyc-checker:: emacs-lisp
   :auto:

   .. rubric:: Options

   .. option:: flycheck-emacs-lisp-initialize-packages
      :auto:

   .. option:: flycheck-emacs-lisp-load-path
      :auto:

   .. option:: flycheck-emacs-lisp-package-user-dir
      :auto:

   .. seealso::

      With the flycheck-cask_ extension this syntax checker automatically uses
      packages from Cask_ projects.

.. flyc-checker:: emacs-lisp-checkdoc
   :auto:

   .. seealso:: :infonode:`(elisp)Documentation Tips`

.. _Emacs: http://www.gnu.org/software/emacs/
.. _Emacs Lisp reference: http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
.. _flycheck-cask: https://github.com/flycheck/flycheck-cask
.. _Cask: https://github.com/cask/cask

Erlang
======

.. flyc-checker:: erlang
   :auto:

ERuby
=====

.. flyc-checker:: eruby-erubis
   :auto:

Fortran
=======

.. flyc-checker:: fortran-gfortran
   :auto:

   .. rubric:: Options

   .. option:: flycheck-gfortran-include-path
      :auto:

   .. option:: flycheck-gfortran-language-standard
      :auto:

   .. option:: flycheck-gfortran-layout
      :auto:

   .. option:: flycheck-gfortran-warnings
      :auto:

Go
==

.. flyc-checker:: go-gofmt
   :auto:

.. flyc-checker:: go-golint
   :auto:

.. flyc-checker:: go-vet
   :auto:

   .. rubric:: Options

   .. option:: flycheck-go-vet-print-functions
      :auto:

.. flyc-checker:: go-build
   :auto:
.. flyc-checker:: go-test
   :auto:
.. flyc-checker:: go-errcheck
   :auto:

Haml
====

.. flyc-checker:: haml
   :auto:

Handlebars
==========

.. flyc-checker:: handlebars
   :auto:

Haskell
=======

.. flyc-checker:: haskell-ghc
   :auto:

   .. rubric:: Options

   .. option:: flycheck-ghc-language-extensions
      :auto:

   .. option:: flycheck-ghc-no-user-package-database
      :auto:

   .. option:: flycheck-ghc-package-databases
      :auto:

   .. option:: flycheck-ghc-search-path
      :auto:

   .. seealso::

      The flycheck-haskell_ extension configures this syntax checker according
      to the corresponding Cabal project, and adds Cabal sandboxes.

.. flyc-checker:: haskell-hlint
   :auto:

.. seealso::

   The flycheck-hdevtools_ extension adds a Haskell syntax checker using
   hdevtools_.

.. _flycheck-haskell: https://github.com/flycheck/flycheck-haskell
.. _flycheck-hdevtools: https://github.com/flycheck/flycheck-hdevtools
.. _hdevtools: https://github.com/bitc/hdevtools/

HTML
====

.. flyc-checker:: html-tidy
   :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-tidyrc
      :auto:

Javascript
==========

.. flyc-checker:: javascript-jshint
   :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-jshintrc
      :auto:

.. flyc-checker:: javascript-eslint
   :auto:

   .. rubric:: Options

   .. option:: flycheck-eslint-rulesdir
      :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-eslintrc
      :auto:

.. flyc-checker:: javascript-gjslint
   :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-gjslintrc
      :auto:

JSON
====

.. flyc-checker:: json-jsonlint
   :auto:

LESS
====

.. flyc-checker:: less
   :auto:

Lua
===

.. flyc-checker:: lua
   :auto:

Make
====

.. flyc-checker:: make
   :auto:

   .. seealso:: `GNU Make`_, `NetBSD Make`_, `FreeBSD Make`_, `OpenBSD Make`_

.. _GNU Make: http://www.gnu.org/software/make/
.. _NetBSD Make: http://netbsd.gw.com/cgi-bin/man-cgi?make++NetBSD-current
.. _FreeBSD Make: http://www.freebsd.org/cgi/man.cgi?query=make&sektion=1
.. _OpenBSD Make: http://www.openbsd.org/cgi-bin/man.cgi?query=make

Perl
====

.. flyc-checker:: perl
   :auto:

.. flyc-checker:: perl-perlcritic
   :auto:

   .. rubric:: Options

   .. option:: flycheck-perlcritic-verbosity
      :auto:

PHP
===

.. flyc-checker:: php
   :auto:

.. flyc-checker:: php-phpmd
   :auto:

   .. rubric:: Options

   .. option:: flycheck-phpmd-rulesets
      :auto:

.. flyc-checker:: php-phpcs
   :auto:

   .. rubric:: Options

   .. option:: flycheck-phpcs-standard
      :auto:

Puppet
======

.. flyc-checker:: puppet-parser
   :auto:

.. flyc-checker:: puppet-lint
   :auto:

Python
======

.. flyc-checker:: python-flake8
   :auto:

   .. rubric:: Options

   .. option:: flycheck-flake8-maximum-complexity
      :auto:

   .. option:: flycheck-flake8-maximum-line-length
      :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-flake8rc
      :auto:

.. flyc-checker:: python-pylint
   :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-pylintrc
      :auto:

.. seealso::

   flycheck-pyflakes_ adds a Python syntax checker using Pyflakes.

.. _flycheck-pyflakes: https://github.com/Wilfred/flycheck-pyflakes

Racket
======

.. flyc-checker:: racket
   :auto:

RPM SPEC
========

.. flyc-checker:: rpm-rpmlint
   :auto:

ReStructuredText
================

.. flyc-checker:: rst
   :auto:

.. flyc-checker:: rst-sphinx
   :auto:

   .. rubric:: Options

   .. option:: flycheck-sphinx-warn-on-missing-references
      :auto:

Ruby
====

.. flyc-checker:: ruby-rubocop
   :auto:

   .. rubric:: Options

   .. option:: flycheck-rubocop-lint-only
      :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-rubocoprc
      :auto:

.. flyc-checker:: ruby-rubylint
   :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-rubylintrc
      :auto:

      .. warning::

         You need ruby-lint version 2.0.2 or newer to use this variable.

.. flyc-checker:: ruby
   :auto:

.. flyc-checker:: ruby-jruby
   :auto:

Rust
====

.. flyc-checker:: rust
   :auto:

   .. rubric:: Options

   .. option:: flycheck-rust-check-tests
      :auto:

   .. option:: flycheck-rust-crate-root
      :auto:

   .. option:: flycheck-rust-crate-type
      :auto:

   .. option:: flycheck-rust-library-path
      :auto:

   .. seealso::

      The flycheck-rust_ extension customizes this syntax checker according to
      the Cargo project layout and settings.

.. _flycheck-rust: https://github.com/flycheck/flycheck-rust

Sass
====

.. flyc-checker:: sass
   :auto:

   .. rubric:: Options

   .. option:: flycheck-sass-compass
      :auto:

Scala
=====

.. flyc-checker:: scala
   :auto:

.. flyc-checker:: scala-scalastyle
   :auto:

   .. rubric:: Options

   .. option:: flycheck-scalastyle-jar
      :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-scalastylerc
      :auto:

Scss
====

.. flyc-checker:: scss
   :auto:

   .. rubric:: Options

   .. option:: flycheck-scss-compass
      :auto:

Shell script languages
======================

.. flyc-checker:: sh-bash
   :auto:

.. flyc-checker:: sh-posix-dash
   :auto:

   .. seealso:: `POSIX Shell Command Language`_

.. flyc-checker:: sh-posix-bash
   :auto:

   .. seealso::  `GNU Bash POSIX Mode`_, `POSIX Shell Command Language`_

.. flyc-checker:: sh-zsh
   :auto:

.. flyc-checker:: sh-shellcheck
   :auto:

.. _POSIX Shell Command Language: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
.. _GNU Bash POSIX Mode: http://www.gnu.org/software/bash/manual/html_node/Bash-POSIX-Mode.html#Bash-POSIX-Mode

Slim
====

.. flyc-checker:: slim
   :auto:

TeX/LaTeX
=========

.. seealso:: LaTeX_, `TeX Live`_

.. flyc-checker:: tex-chktex
   :auto:

   .. rubric:: Configuration file

   .. option:: flycheck-chktexrc
      :auto:

.. flyc-checker:: tex-lacheck
   :auto:

.. _LaTeX: http://www.latex-project.org/
.. _TeX Live: http://www.tug.org/texlive/

Texinfo
=======

.. flyc-checker:: texinfo
   :auto:

Verilog
=======

.. flyc-checker:: verilog-verilator
   :auto:

XML
===

.. flyc-checker:: xml-xmlstarlet
   :auto:

.. flyc-checker:: xml-xmllint
   :auto:

YAML
====

.. flyc-checker:: yaml-jsyaml
   :auto:

.. flyc-checker:: yaml-ruby
   :auto:
