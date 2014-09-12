===============
 Flycheck 0.15
===============

:date: Nov 15, 2013
:version: 0.15

I have the honour and pleasure to announce a new release of Flycheck, the
modern syntax-checking extension for Emacs.

.. only:: not format_texinfo

   .. figure:: /images/flycheck-0_15.png
      :align: center

      Flycheck 0.15 with Zenburn_ and `Source Code Pro`_

It is three months since the last release, so this release brings quite a lot of
changes.  For a complete list of all changes, please read the complete list of
:ref:`changelog`.  This article will just cover the most important changes.

.. _Zenburn: https://github.com/bbatsov/zenburn-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro

.. contents:: Table of Contents
   :local:

Breaking changes
================

This release introduces three breaking changes.  Two of these are related to the
`new-error-list`_:

.. warning::

   `flycheck-display-errors-in-list` is gone.  If you had previously set
   :option:`flycheck-display-errors-function` to this function, **remove** this
   setting from your configuration.

Besides, `flycheck-list-errors` does not take a prefix argument anymore.  It
cannot list errors at point any longer.

The third breaking change is the removal of `flycheck-declare-checker`, which
was obsolete already since the last release.  Unless you have custom syntax
checkers not yet ported to :macro:`flycheck-define-checker`, this won't affect
you in any way.  I am not aware of any 3rd party extension which still uses
`flycheck-declare-checker`.

New syntax checkers
===================

Two new languages made it into this release:  YAML (using the YAML parser from
the Ruby standard library) and Slim_.

Additionally there are new syntax checkers for Javascript and PHP.  Flycheck can
check Javascript with Google's `Closure Linter`_, in place of Jshint.  For PHP,
Flycheck now uses the `PHP Mess Detector`_ to check for semantic errors, in
*addition* to a syntax checks with PHP CLI and a style checks with PHP
CodeSniffer.

.. _Slim: http://slim-lang.com
.. _Closure Linter: https://code.google.com/p/closure-linter/
.. _PHP Mess Detector: http://phpmd.org/

.. _new-error-list:

New error list
==============

The error list at :kbd:`C-c ! l` has been redesigned.

It is no longer a static list filled once by :kbd:`C-c ! l`.  Instead, it
automatically updates after each syntax check, and follows the current window,
i.e. if you switch to another window, the error list is updated to show the
errors of the corresponding buffer.

Furthermore, the error list highlights the errors at point and at the current
line.  If you move the point to an error location, the error list automatically
scrolls to the corresponding error, and highlights it with the new
:face:`flycheck-error-list-highlight-at-point` face.  Additionally, it
highlights all other errors at the current line with the new
:face:`flycheck-error-list-highlight` face.

C/C++ support
=============

The :flyc-checker:`c/c++-clang` syntax checker for C and C++ got a bunch
of new options:

- Set additional preprocessor definitions for syntax checking with
  :option:`flycheck-clang-definitions`, corresponding to the `-D` option for
  `clang`.
- Include additional headers or files during syntax checking with
  :option:`flycheck-clang-includes`, corresponding to the `-include` option for
  `clang`.
- Choose the language standard, e.g. C++98 or C++11, with
  :option:`flycheck-clang-language-standard`, corresponding to the `-std` option
  for `clang`.
- Disable RTTI during syntax checking with :option:`flycheck-clang-no-rtti`,
  corresponding to the `-fno-rtti` option for `clang`.  In current Clang
  versions, however, this does not cause errors or warnings when using RTTI.
- Choose the standard library for syntax checking with
  :option:`flycheck-clang-standard-library`, corresponding to the `-stdlib`
  option for `clang`.  Currently, Clang supports `libstdc++` for the good old
  GNU standard library, and `libc++` for the modern Libc++ from the LLVM project.

Besides, the Clang syntax checker was changed to correctly handle local include
files, e.g. `#include "foo.h"`.

New `info` level messages
=========================

In addition to the `warning` and `error` levels, Flycheck now has a new `info`
level, which is intended for informational messages which provide additional
information about a specific location in the source code.  With this new level,
the semantics of error levels is as follows:

`error`
  Definite errors which must be fixed for the source code to work correctly
`warning`
  Potential errors and issues, which can be ignored, but still deserve to be
  fixed
`info`
  Additional information about a specific source code location, which does not
  indicate an error or issue, but is still worth noting

Some syntax checkers were changed to use this new level for messages, which do
not really fit into the `warning` level:

- :flyc-checker:`c/c++-clang` for `note:` messages
- :flyc-checker:`python-flake8` for PEP8 naming issues emitted by the
  `pep8-naming` plugin
- :flyc-checker:`python-pylint` for convention level messages, e.g. naming
  issues, etc.

The new level is already supported by the popular Solarized_ and Zenburn_
themes.

.. _Solarized: https://github.com/bbatsov/solarized-emacs
.. _Zenburn: https://github.com/bbatsov/zenburn-emacs

Custom error levels
===================

The new `info` level is backed by a generic mechanism to define new error
levels, using the new function :function:`flycheck-define-error-level`.  For
instance, the `warning` level is now defined as follows:

.. code-block:: cl

   (flycheck-define-error-level 'warning
     :overlay-category 'flycheck-warning-overlay
     :fringe-bitmap 'question-mark
     :fringe-face 'flycheck-fringe-warning)

A error level consists of a category for overlays, which defines the appearance
and priority of overlays for this level, and bitmap and face for fringe
indicators.

After defining an error level, you can use it the error patterns of a syntax
checker as usual.

Other improvements
==================

Beside these important changes, there are also a number of smaller improvements:

- Flycheck does not longer check encrypted files for obvious reasons.
- The :flyc-checker:`emacs-lisp-checkdoc` syntax checker does not check
  `.dir-locals.el` anymore.
- :flyc-checker:`python-pylint` now parses error columns from the output of
  `pylint`.
- Spurious “flawed definition” warnings in :flyc-checker:`lua`,
  :flyc-checker:`rst` and :flyc-checker:`go-build` were fixed.
- :flyc-checker:`c/c++-cppcheck` output now parses correctly when using the pure
  Emacs Lisp XML parser in `xml.el`.

  .. note::

     Nonetheless, you are advised to use Emacs with `libxml` support.  Most
     Linux distributions ship Emacs packages with `libxml` support, but if you
     are building your own, or use a source-based distribution such as Gentoo,
     take care to enable `libxml` for Emacs.

Get it
======

See :ref:`installation`.
