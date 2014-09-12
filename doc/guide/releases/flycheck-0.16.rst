=================
  Flycheck 0.16
=================

:date: Jan 12, 2014
:version: 0.16

Time for a new release of Flycheck, the modern syntax-checking extension for
Emacs:

.. only:: not format_texinfo

   .. figure:: /images/flycheck-0_16.png
      :align: center

      Flycheck 0.16 with `Solarized Light`_ and `Source Code Pro`_

In two months since the last release, Flycheck got a bunch of new syntax
checkers, a brand-new error list, and the ability to override the executables of
syntax checkers.

Let's go through the list of important changes.  For a detailed list, please
read the :ref:`changelog`.

.. _Source Code Pro: https://github.com/adobe/source-code-pro
.. _Solarized Light: https://github.com/bbatsov/solarized-emacs

.. contents:: Table of Contents
   :local:

Breaking changes
================

- The Hdevtools syntax checker was removed from Flycheck into a separate package
  `flycheck-hdevtools`_ due to various issues ([GH-275]).
- Support for coffeelint 0.x is dropped.

.. _flycheck-hdevtools: https://github.com/flycheck/flycheck-hdevtools

Syntax checkers
===============

New languages and checkers
--------------------------

Flycheck supports *eight* new languages: AsciiDoc_, Cfengine_, Chef_ recipes,
ERuby_, Handlebars_, Racket_, Texinfo_, and Verilog_.  Additionally, there are a
new syntax checker for Javascript (eslint_), Ruby (ruby-lint_), and YAML
(js-yaml_).

.. _AsciiDoc: http://asciidoc.org/
.. _Cfengine: http://cfengine.com/
.. _Chef: http://www.getchef.com/
.. _ERuby: http://www.kuwata-lab.com/erubis/
.. _Handlebars: http://handlebarsjs.com/
.. _Racket: http://racket-lang.org/
.. _Texinfo: https://www.gnu.org/software/texinfo
.. _Verilog: https://en.wikipedia.org/wiki/Verilog
.. _eslint: https://github.com/eslint/eslint
.. _ruby-lint: https://github.com/YorickPeterse/ruby-lint
.. _js-yaml: https://github.com/visionmedia/js-yaml

Better Haskell support
----------------------

Despite the aforementioned removal of Hdevtools, Haskell support made a leap
forward.  The GHC syntax checker resolves local imports properly now, and has
new options to change the search path and the package databases.

The brand-new flycheck-haskell_ extension makes use of these variables to
configure the syntax checker properly in Cabal projects.  The extensions adds
all source directories of a Cabal project to the GHC search path, and enables
the package database of the project's Cabal sandbox.

.. _flycheck-haskell: https://github.com/flycheck/flycheck-haskell

Miscellaneous new options
-------------------------

- The SASS and SCSS syntax checkers support the Compass framework now, via
  :option:`flycheck-sass-compass` and :option:`flycheck-scss-compass`
  respectively.
- Clang can enable Microsoft C/C++ extensions now, via
  :option:`flycheck-clang-ms-extensions`.
- Rubocop can inhibit all style hints via the new
  :option:`flycheck-rubocop-lint-only`.

New features
============

Syntax checker executables
--------------------------

You can now override the executables of syntax checkers.  See
:ref:`syntax-checker-executables` for details.

Disable syntax checkers easily
------------------------------

Flycheck as a new customization options :option:`flycheck-disabled-checkers`, to
easily disable syntax checkers.

Previously, you needed to remove syntax checkers from
:option:`flycheck-checkers` to disable them, either via the Customization
interface, or by custom Emacs Lisp:

.. code-block:: cl

   (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))

With the new variable, there is no need for this convoluted code anymore.
Instead, just set the variable:

.. code-block:: cl

   (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))

Even better, you can easily use this variable in file or directory variables.
For instance, you can use :kbd:`M-x add-file-local-variable RET
flycheck-disabled-checkers RET (emacs-lisp-checkdoc)` in your :file:`init.el` to
disable Checkdoc warnings while editing your :file:`init.el`.

Improved error list
===================

This release continues the improvements to the error list started in the last
release.  The error list is now based on Tabulated List Mode (see [GH-230]), to
address a number of issues in the old Compile Mode-based error list
(e.g. misleading commands and menu items such as “Recompile”).

The new error list, which you can see in the screenshot above, fixes these
issues, and has an improved visual appearance.  The columns are aligned now, and
the superfluous file name is omitted.

Get it
======

See :ref:`installation`.
