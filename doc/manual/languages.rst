=====================
 Supported languages
=====================

.. default-domain:: flyc

Flycheck supports the following |#flycheck-languages| languages with
|#flycheck-checkers| syntax checkers.

Each language has one or more syntax checkers, whose names follow a
:samp:`{language}-{tool}` convention, where :samp:`{language}` is the
programming language supported by this syntax checker, and :samp:`{tool}` the
specific syntax checker tool.

This document provides just a brief overview, use
:el:command:`flycheck-describe-checker` to see comprehensive documenation about
a syntax checker.

.. contents:: Supported languages
   :local:

AsciiDoc
========

.. seealso::  http://www.methods.co.nz/asciidoc/

.. syntax-checker:: asciidoc

C/C++
=====

.. syntax-checker:: c/c++-clang

   .. seealso:: http://clang.llvm.org/

.. syntax-checker:: c/c++-cppcheck

   .. seealso:: http://cppcheck.sourceforge.net/

CFEngine
========

.. seealso:: http://cfengine.com/

.. syntax-checker:: cfengine

Chef
====

.. seealso:: http://www.getchef.com/chef/

.. syntax-checker:: chef-foodcritic

   .. seealso:: http://acrmp.github.io/foodcritic/

Coffeescript
============

.. seealso:: http://coffeescript.org/

.. syntax-checker:: coffee

.. syntax-checker:: coffee-coffeelint

   .. seealso:: http://www.coffeelint.org/

CSS
===

.. syntax-checker:: css-csslint

   .. seealso:: https://github.com/stubbornella/csslint

D
=

.. seealso:: http://dlang.org/

.. syntax-checker:: d-dmd

Elixir
======

.. seealso:: http://elixir-lang.org

.. syntax-checker:: elixir

Emacs Lisp
==========

.. seealso::

   Emacs
      http://www.gnu.org/software/emacs/

   Emacs Lisp manual
      http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html

.. syntax-checker:: emacs-lisp

.. syntax-checker:: emacs-lisp-checkdoc

   .. seealso::

      Tips for Documentation Strings
         http://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html#Documentation-Tips

Erlang
======

.. seealso:: http://www.erlang.org/

.. syntax-checker:: erlang

ERuby
=====

.. syntax-checker:: eruby-erubis

   .. seealso::

      Erubis
         http://www.kuwata-lab.com/erubis/

Go
==

.. seealso:: http://golang.org/

.. syntax-checker:: go-gofmt
.. syntax-checker:: go-golint

   .. seealso:: https://github.com/golang/lint

.. syntax-checker:: go-vet

   .. seealso::

      `go`
         http://golang.org/cmd/go

      `go tool vet`
         http://godoc.org/code.google.com/p/go.tools/cmd/vet

.. syntax-checker:: go-build
.. syntax-checker:: go-test

Haml
====

.. seealso:: http://haml.info/

.. syntax-checker:: haml

Handlebars
==========

.. seealso:: http://handlebarsjs.com/

.. syntax-checker:: handlebars

Haskell
=======

.. seealso:: http://www.haskell.org/

.. syntax-checker:: haskell-ghc

   .. seealso:: http://www.haskell.org/ghc/

.. syntax-checker:: haskell-hlint

   .. seealso:: https://github.com/ndmitchell/hlint

HTML
====

.. syntax-checker:: html-tidy

   .. seealso::

      HTML Tidy
         http://tidy.sourceforge.net/

      HTML Tidy for HTML 5
         http://w3c.github.io/tidy-html5/

Javascript
==========

.. syntax-checker:: javascript-jshint

   .. seealso:: http://www.jshint.com/

.. syntax-checker:: javascript-eslint

   .. seealso:: https://github.com/eslint/eslint

.. syntax-checker:: javascript-gjslint

   .. seealso::

      Closure Linter
         https://developers.google.com/closure/utilities/

JSON
====

.. syntax-checker:: json-jsonlint

   .. seealso:: https://github.com/zaach/jsonlint

LESS
====

.. syntax-checker:: less

   .. seealso:: http://www.lesscss.org/

Lua
===

.. syntax-checker:: lua

   .. seealso:: http://www.lua.org/

Make
====

.. seealso::

   IEEE Std 1003.1, 2013 Edition, Make
      http://pubs.opengroup.org/onlinepubs/9699919799/utilities/make.html

.. syntax-checker:: make

   .. seealso::

      GNU Make
         http://www.gnu.org/software/make/

      NetBSD Make
         http://netbsd.gw.com/cgi-bin/man-cgi?make++NetBSD-current

      FreeBSD Make
         http://www.freebsd.org/cgi/man.cgi?query=make&sektion=1

      OpenBSD Make
         http://www.openbsd.org/cgi-bin/man.cgi?query=make

Perl
====

.. seealso:: http://www.perl.org

.. syntax-checker:: perl

.. syntax-checker:: perl-perlcritic

   .. seealso:: http://search.cpan.org/dist/Perl-Critic/

PHP
===

.. syntax-checker:: php

   .. seealso::

      PHP Command Line
         http://php.net/manual/en/features.commandline.php

.. syntax-checker:: php-phpmd

   .. seealso::

      PHP Mess Detector
         http://phpmd.org/

.. syntax-checker:: php-phpcs

   .. seealso::

      PHP Code Sniffer
         http://pear.php.net/package/PHP_CodeSniffer/

Puppet
======

.. seealso:: https://puppetlabs.com/

.. syntax-checker:: puppet-parser

.. syntax-checker:: puppet-lint

   .. seealso:: http://www.puppet-lint.com/

Python
======

.. syntax-checker:: python-flake8

   .. seealso:: http://flake8.readthedocs.org/

.. syntax-checker:: python-pylint

   .. seealso:: http://www.pylint.org/

Racket
======

.. seealso:: http://racket-lang.org/

.. syntax-checker:: racket

ReStructuredText
================

.. seealso::

   ReStructuredText
      http://docutils.sourceforge.net/rst.html

   Docutils
      http://docutils.sourceforge.net/

.. syntax-checker:: rst

.. syntax-checker:: rst-sphinx

   .. seealso:: http://sphinx-doc.org

Ruby
====

.. seealso:: https://www.ruby-lang.org/

.. syntax-checker:: ruby-rubocop

   .. seealso:: https://github.com/bbatsov/rubocop

.. syntax-checker:: ruby-rubylint

   .. seealso:: https://github.com/YorickPeterse/ruby-lint

.. syntax-checker:: ruby

.. syntax-checker:: ruby-jruby

   .. seealso:: http://jruby.org/

Rust
====

.. seealso:: http://www.rust-lang.org/

.. syntax-checker:: rust

Sass
====

.. seealso:: http://sass-lang.com/

.. syntax-checker:: sass

Scala
=====

.. seealso:: http://www.scala-lang.org/

.. syntax-checker:: scala

Scss
====

.. syntax-checker:: scss

   .. seealso:: http://sass-lang.com/

Shell script languages
======================

.. syntax-checker:: sh-bash

   .. seealso:: http://www.gnu.org/software/bash/

.. syntax-checker:: sh-posix-dash

   .. seealso::

      DASH
         http://gondor.apana.org.au/~herbert/dash/

      POSIX Shell Command Language
         http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html

.. syntax-checker:: sh-posix-bash

   .. seealso::

      Bash POSIX Mode
         http://www.gnu.org/software/bash/manual/html_node/Bash-POSIX-Mode.html#Bash-POSIX-Mode

      POSIX Shell Command Language
         http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html

.. syntax-checker:: sh-zsh

   .. seealso:: http://www.zsh.org/

.. syntax-checker:: sh-shellcheck

   .. seealso:: https://github.com/koalaman/shellcheck/

Slim
====

.. seealso:: http://slim-lang.com/

.. syntax-checker:: slim

TeX/LaTeX
=========

.. seealso::

   LaTeX
      http://www.latex-project.org/

   TeX Live
      http://www.tug.org/texlive/

.. syntax-checker:: tex-chktex

   .. seealso:: http://www.nongnu.org/chktex/

.. syntax-checker:: tex-lacheck

   .. seealso:: http://www.ctan.org/pkg/lacheck

Texinfo
=======

.. syntax-checker:: texinfo

   .. seealso:: http://www.gnu.org/software/texinfo/

Verilog
=======

.. syntax-checker:: verilog-verilator

   .. seealso:: http://www.veripool.org/wiki/verilator

XML
===

.. syntax-checker:: xml-xmlstarlet

   .. seealso:: http://xmlstar.sourceforge.net/

.. syntax-checker:: xml-xmllint

   .. seealso::

      Libxml2
         http://www.xmlsoft.org/

YAML
====

.. syntax-checker:: yaml-jsyaml

   .. seealso:: https://github.com/nodeca/js-yaml

.. syntax-checker:: yaml-ruby

   .. seealso:: https://www.ruby-lang.org
