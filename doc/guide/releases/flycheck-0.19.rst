===============
 Flycheck 0.19
===============

:date: Jun 12, 2014
:version: 0.19

We are pleased to announce the release of Flycheck 0.19.

.. contents:: Table of Contents
   :local:

Breaking changes
================

:flyc-checker:`ruby-rubylint` requires Ruby Lint 2.0 or newer, due to some
breaking changes in the command line interface of Ruby Lint.

Languages
=========

C/C++
-----

The new :option:`flycheck-cppcheck-inconclusive` enables
:flyc-checker:`c/c++-cppcheck` tests that might yield false positives.

When reporting errors in included files, :flyc-checker:`c/c++-clang` now
includes the error messages in the faulty include file in the error message on
the affected ``#include`` statement.

Emacs Lisp
----------

Flycheck now removes redundant whitespace in the error messages reported by the
:flyc-checker:`emacs-lisp` syntax checker.

Go
--

Go code is now checked for unhandled error return values with the new
:flyc-checker:`go-errcheck` syntax checker.

Flycheck does not longer signal errors in Go Mode, when the ``go`` executable is
not available.

Haskell
-------

Flycheck now dedents the error messages reported by :flyc-checker:`haskell-ghc`,
and properly checks Haskell files without extensions.

Rust
----

:flyc-checker:`rust` has a new :option:`flycheck-rust-check-tests` option, to
disable syntax checking of test code.

New features
============

Syntax checkers may now use the `:error-filter` argument to
:macro:`flycheck-define-checker` to apply a function to all errors after
parsing.  The function is given the list of parsed errors, and shall return the
list of errors that Flycheck is to use as the result of the syntax checker.  It
may modify the list and all error objects therein in-place.

The new option :option:`flycheck-keymap-prefix` lets you change the prefix key
used by Flycheck.  The default is still :kbd:`C-c !`.

.. warning::

   The Flycheck manual assumes that you are using the default prefix key.  You
   are encouraged to leave the prefix key unchanged, and instead add your own
   custom bindings to the Flycheck key map:

   .. code-block:: cl

      (eval-after-load 'flycheck
        '(define-key flycheck-mode-map (kbd "<f8>") #'flycheck-list-errors))

Likewise, you now can configure the prefix of in-place temporary files created
by Flycheck with :option:`flycheck-temp-prefix`.

.. warning::

   Changing the prefix may break syntax checkers.  In doubt, stick to the
   default prefix.

Bug fixes
=========

Flycheck now correctly kills running process and removes the temporary files
when stopping a syntax check or killing the buffer.

Also, Flycheck does not long warn if a syntax checker only reports errors for
other files.

Misc changes
============

Flycheck now has an official logo, and its own mail address.  You can see both
on the `Github profile`_ of the Flycheck project.

.. _Github profile: https://github.com/flycheck/

Get it
======

See :ref:`installation`.
