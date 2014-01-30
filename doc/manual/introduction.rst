==============
 Introduction
==============

Flycheck (aka “Flymake done right”) is a modern on-the-fly syntax checking
extension for GNU Emacs 24 with

- ready-to-use syntax checkers for a bunch of languages,
- easy customization,
- a comprehensive manual,
- a dead simple, declarative API to create new syntax checkers,
- major-mode based selection of syntax checkers,
- multiple syntax checkers per buffer,
- optional error list popup,
- a “doesn't get in your way” guarantee,
- and a clean, concise and understandable implementation with decent test
  coverage.

Features
========

- Automatic on-the-fly syntax checking while editing
- Fully automatic selection of the syntax checker
- Optional manual selection of a syntax checker with `flycheck-select-checker`
  at :kbd:`C-c ! s`
- Built-in syntax checkers for:

  - AsciiDoc (using `asciidoc`)
  - C/C++ (using `clang` and `cppcheck`)
  - CFEngine (using `cf-promises`)
  - Chef cookbooks (using `foodcritic`))
  - CoffeeScript (using `coffee` and `coffeelint`)
  - CSS (using `csslint`))
  - D (using `dmd`)
  - Elixir (using `elixirc`)
  - Emacs Lisp (using the byte compiler and CheckDoc)
  - Erlang (using `erlc`)
  - ERuby (using `erubis`)
  - Go (using `gofmt`, `go build` and `go test`)
  - Haml (using `haml`)
  - Handlebars (using `handlebars`)
  - Haskell (using `ghc` and `hlint`)
  - HTML (using `tidy`)
  - Javascript (using `jshint`, `eslint` and `gjslint`)
  - JSON (using `jsonlint`)
  - LESS (using `lessc`)
  - Lua (using `luac`)
  - Perl (using `perl`)
  - PHP (using `php`, `phpmd` and `phpcs`)
  - Puppet (using `puppet parser` and `puppet-lint`)
  - Python (using `flake8` or `pylint`)
  - Racket
  - ReStructuredText (using `rst2pseudoxml.py` from Docutils)
  - Ruby (using `rubocop`, `ruby` , `jruby` or `ruby-lint`)
  - Rust (using `rustc`)
  - Sass (using `sass`)
  - Scala (using `scalac`)
  - SCSS (using `scss`)
  - Shell scripts (using `bash`, `dash`, or `zsh` depending on the type of shell
    script)
  - Slim (using `slimrb`)
  - TeX/LaTeX (using `chktex` or `lacheck`)
  - Texinfo (using `makeinfo`)
  - Verilog (using `verilator`)
  - XML (using `xmlstarlet` or `xmllint`)
  - YAML (using `js-yaml` or `ruby`)

- Nice error indication and highlighting
- Easy customization
- Syntax checker configuration with project-specific configuration files and
  options
- Error navigation with `next-error` and `previous-error`
- Error list with `flycheck-list-errors` at :kbd:`C-c ! l`
- Easy declarative API to define new syntax checkers
- Error parsers for structured markup formats (e.g. Checkdoc XML)

.. _3rd-party-extensions:

3rd party extensions
====================

The following extensions provide additional cool features for Flycheck:

- flycheck-cask_ makes Flycheck use Cask packages in Cask_ projects.
- flycheck-color-mode-line_ colors the mode line according to the Flycheck
  status.
- flycheck-d-unittest_ adds a Flycheck checker to run unit tests for D programs
  on the fly.
- flycheck-hdevtools_ adds a Flycheck syntax checker for Haskell based on
  hdevtools_.
- flycheck-haskell_ improves Haskell support in Flycheck, by configuring
  Flycheck according to the current Cabal project, and using Cabal sandbox
  packages.
- flycheck-mercury_ adds a Flycheck syntax checker for the `Mercury Language`_.

.. _flycheck-cask: https://github.com/flycheck/flycheck-cask
.. _Cask: https://github.com/cask/cask
.. _flycheck-color-mode-line: https://github.com/flycheck/flycheck-color-mode-line
.. _flycheck-d-unittest: https://github.com/flycheck/flycheck-d-unittest
.. _flycheck-hdevtools: https://github.com/flycheck/flycheck-hdevtools
.. _hdevtools: https://github.com/bitc/hdevtools/
.. _flycheck-haskell: https://github.com/flycheck/flycheck-haskell
.. _flycheck-mercury: https://github.com/flycheck/flycheck-mercury
.. _Mercury language: http://mercurylang.org/

.. _installation:

Installation
============

Install the ELPA package from MELPA_ or Marmalade_ using :kbd:`M-x
package-install RET flycheck`.  The former is the *recommended* repository.

Neither of these repositories is included in GNU Emacs by default.  You need to
enable these repositories explicitly.  For instance, to add the MELPA
repository, add the following code to :file:`init.el`:

.. code-block:: cl

   (require 'package)
   (add-to-list 'package-archives
                '("melpa" . "http://melpa.milkbox.net/packages/") t)
   (package-initialize)

If you use Cask_, add the following to your :file:`Cask` file:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

Flycheck supports Emacs 24.  It is tested with Emacs 24.3, and Emacs snapshot
builds.  It should work with GNU Emacs 24.1 and 24.2 as well, but it is not
tested against these versions.  If it does not work with these versions, please
report an issue.

Flycheck does not support Windows, but tries to be compatible with it.  You may
try to use Flycheck on Windows.  It should mostly work, but expect problems and
issues.  Pull requests which improve Windows compatibility are welcome.

Flycheck also does not support GNU Emacs 23 and other flavors of Emacs
(e.g. XEmacs, Aquamacs, etc.).  Don't try, it will *not* work.

Most checkers depend on external tools to perform the actual syntax checking.
Use :command:`flycheck-describe-checker` to get help about a syntax checker and
its dependencies.

.. _MELPA: http://melpa.milkbox.net
.. _Marmalade: http://marmalade-repo.org/
