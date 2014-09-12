===============
 Flycheck 0.20
===============

:date: Aug 12, 2014
:version: 0.20

Flycheck 0.20 is out, with new syntax checkers, many new options, improvements
and bug fixes, and some important breaking changes.

This announcement covers the most important changes.  For details, please refer
to the :ref:`changelog`.

.. contents:: Table of Contents
   :local:

Breaking changes
================

.. warning::

   The internals of syntax checker definitions changed in a
   backwards-incompatible way.  Due to the way macros work in Emacs Lisp **all
   packages depending on Flycheck** must be recompiled after upgrading to
   Flycheck 0.20!

Furthermore, the special meaning of a trailing equals sign in `(config …)` and
`(config-file …)` is removed.  Syntax checker definitions now need to explicitly
specify a function to concatenate the option and its arguments.  `(option
"--foo=" foo)` now becomes `(option "--foo=" foo concat)`.

As a consequence of this change, the position of the `filter` in `(option …)`
changed.  The `filter` is now in the **forth** element.  Hence, `(option "--foo"
foo flycheck-option-int)` now becomes `(option "--foo" foo nil
flycheck-option-int)`.

Language support
================

Flycheck now supports Ada and Fortran with :flyc-checker:`ada-gnat` and
:flyc-checker:`fortran-gfortran` respectively.

C/C++
-----

C/C++ can now be checked with GCC (:flyc-checker:`c/c++-gcc`) as an alternative
to Clang.  GCC supports the same options as Clang, with the exception of
:option:`flycheck-clang-blocks`.

The Clang syntax checker now supports the Clang block syntax with the new option
:option:`flycheck-clang-blocks`, and allows to disable exceptions with
:option:`flycheck-clang-no-exceptions`.

Furthermore, temporary files for the Clang syntax checker are now created in the
system temporary directory.

Rust
----

Support for Rust creates is greatly improved.  You can now specify the type of
crate to check with `flycheck-rust-crate-type`, and the root file of the crate
with `flycheck-rust-crate-root`.  The former avoids redundant warnings due to
unused code, and the latter helps to resolve relative module references.

Flycheck also parses info messages from Rust now.

Scala
-----

Flycheck now supports Scalastyle_ with :flyc-checker:`scala-scalastyle`.

.. _scalastyle: http://www.scalastyle.org/

Sass/SCSS
---------

:flyc-checker:`sass` and :flyc-checker:`scss` now use a temporary directory for
their cache and do not clutter the source directory anymore.

Javascript
----------

:option:`flycheck-eslintrc` is now `nil` by default.  ESLint_ will now find its
configuration file by itself, which enables configuration cascading.  See
`Configuring ESLint`_ for more information.

.. _ESLint: http://eslint.org/
.. _Configuring ESLint: http://eslint.org/docs/configuring/

Ruby
----

:flyc-checker:`ruby-rubylint` now supports configuration files with
:option:`flycheck-rubylintrc`.  This requires ruby-lint 2.0.2 or newer.

Error list improvements
=======================

The error list (:command:`flycheck-list-errors`) got a bunch of improvements:

- Errors can now be sorted by error level and by error location, either by
  clicking on the headers of `line` and `level` respectively, or by pressing
  :kbd:`S` when the point is on the text of the corresponding column.
- Pressing :kbd:`RET` on any error in the error list now jumps immediately to
  the error location.
- :kbd:`n` and :kbd:`p` now move by errors instead by lines, even with
  multi-line error messages, and display the error location in another window
  while navigating.

User interface improvements
===========================

The mode line indicator of Flycheck is now customizable with
:option:`flycheck-mode-line`, which is a mode line template (see
:infonode:`(elisp)Mode Line Format`).  The mode line of the error list buffer is
customizable as well, with :option:`flycheck-error-list-mode-line`.

The Flycheck menu at :menuselection:`Tools -> Syntax Checking` was improved.  It
now provides an item to toggle Flycheck Mode in the current buffer.  Items that
are not available currently are now disabled.  The menu is also shown on the
mode line indicator now.

:command:`flycheck-compile` now prompts for a syntax checker to use, defaulting
to the last used for the current buffer.

When Flycheck asks for a syntax checker in the minibuffer, it now presents a
reasonable default.

Miscellaneous new features
==========================

The new hook :hook:`flycheck-status-changed-functions` lets extensions and
customization react on any status change in Flycheck.

Error levels defined with `flycheck-define-error-level` can now have a numeric
severity used for sorting, with `:severity`.

Miscellaneous fixes
===================

Flycheck now properly unloads with `unload-feature`.

The :flyc-checker:`emacs-lisp` checker does not longer choke when
`package-initialize` signals an error.

Get it
======

Follow the :doc:`installation instructions <../installation>`.  If you already
have Flycheck installed, just update it from :kbd:`M-x list-packages`.
