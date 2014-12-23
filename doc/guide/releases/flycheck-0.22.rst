===============
 Flycheck 0.22
===============

:date: Dec 23, 2014
:version: 0.22

Flycheck 0.22 comes with some major new features, most notably generic syntax
checkers and many improvements and bug fixes.  Unfortunately, some of these
changes break backwards compatibility.

This announcement covers the most important changes.  For details, please refer
to the :ref:`changelog`.

Breaking changes
================

.. warning::

   The addition of generic syntax checkers lead to some breaking changes to the
   internals of syntax checker definitions.  Due to the way macros work in Emacs
   Lisp **all packages depending on Flycheck** must be recompiled after
   upgrading to Flycheck 0.22!

   We hope that this will be the last breaking change of this kind, since the
   definition of syntax checkers is now wrapped into functions, and the
   :macro:`flycheck-define-checker` macro just provides some syntactic sugar for
   definitions.

Error parsers must explicitly set :cl-slot:`flycheck-error buffer` and
:cl-slot:`flycheck-error checker` of new :cl-struct:`flycheck-error` objects
now.  This is another consequence of generic syntax checkers.

`flycheck-perlcritic-verbosity` and `flycheck-copy-messages-as-kill` are
obsolete now, in favour of :option:`flycheck-perlcritic-severity` and
:command:`flycheck-copy-errors-as-kill` respectively.
`flycheck-google-messages` has been removed completely.

:command:`flycheck-select-checker` cannot select disabled syntax checkers
anymore.

Syntax checker options and configuration file variables are not buffer-local
anymore, with some exceptions.  Consequently you can now use `setq` to set their
global value, and you **must** use `setq-local` to set them locally.  Variables
set via file or directory variables are not affected by this change.

Setup
=====

The new :command:`flycheck-verify-setup` command provides information about all
applicable syntax checkers, and lists potential problems.  It is intended to
help you uncover issues in your Flycheck setup.

Language support
================

Ada
---

The new option :option:`flycheck-gnat-args` allows to pass arbitrary arguments
to :flyc-checker:`ada-gnat`.

C/C++
-----

Flycheck now correctly computes the error level from errors in included files.
Messages of errors in included files are now discarded to reduce the verbosity
of error reporting.

The new options :option:`flycheck-clang-args` and :option:`flycheck-gcc-args`
allow to pass arbitrary arguments to :flyc-checker:`c/c++-clang` and
:flyc-checker:`c/c++-gcc` respectively.

Emacs Lisp
----------

The new `inherit` value for :option:`flycheck-emacs-lisp-load-path` automates
`load-path` inheritance for the :flyc-checker:`emacs-lisp` syntax checker.  If
you have the following in your init file

.. code-block:: cl

   (setq-default flycheck-emacs-lisp-load-path load-path)

please replace it with:

.. code-block:: cl

   (setq flycheck-emacs-lisp-load-path 'inherit)

As a consequence of this change you can now enable `load-path` inheritance via
customize.

Haskell
-------

The :flyc-checker:`haskell-ghc` syntax checker supports literate Haskell now.

The new option :option:`flycheck-ghc-args` allows to pass arbitrary arguments to
:flyc-checker:`haskell-ghc`.

Fortran
-------

The new option :option:`flycheck-gfortran-args` allows to pass arbitrary
arguments to :flyc-checker:`fortran-gfortran`.

Python
------

The new syntax checker :flyc-checker:`python-pycompile` uses the built-in byte
compiler of Python to check a buffer for syntax errors.  It is provided as a
last resort for basic syntax checking of Python code, if neither pylint nor
flake8 are installed.

The error levels of :flyc-checker:`python-flake8` can now be customized via
:option:`flycheck-flake8-error-level-alist`.  The default value handles the
standard plugins of flake8, and the 3rd party pep8-naming plugin.  You can add
custom patterns to this option to add support for other flake8 plugins.

:flyc-checker:`python-pylint` now has correct error columns.

Rust
----

The :flyc-checker:`rust` parses help messages to `info`-level errors now.

TeX/LaTeX
---------

:flyc-checker:`tex-chktex` now has correct error columns as well.

Syntax checking improvements
============================

Flycheck now parses errors faster, because it does not longer access the file
system to check the file of each parsed errors, and it protects against
excessive error reporting by automatically disabling syntax checkers that try to
report too many errors.  The threshold is customizable via the new
:option:`flycheck-checker-error-threshold` option.

Error list improvements
=======================

The error list now shows errors IDs for syntax checkers that emit these.

Typing :kbd:`g` in the error list will re-check the source buffer, and refresh
the error list afterwards.

Generic syntax checkers
=======================

Flycheck now supports “generic” syntax checkers, which call arbitrary
synchronous or asynchronous Emacs Lisp functions to check a buffer.  The new
function :function:`flycheck-define-generic-checker` defines a new generic
syntax checker which calls a given function to conduct a syntax check.

Syntax checkers for external commands are now called “command syntax checkers”,
and implemented as a special kind of generic syntax checkers.
:macro:`flycheck-define-checker` still defines a command syntax checker.  The
new function :function:`flycheck-define-command-checker` is a non-macro variant
of :macro:`flycheck-define-generic-checker`.

Error IDs
=========

Syntax checkers can now add “IDs” to errors, which shall uniquely identify a
particular kind of error.  These IDs are used by Flycheck and Flycheck
extensions to identify errors, particularly in future extensions such as error
fixers.

The new `id` form for error patterns allows to parse IDs.  Error parsers and
generic syntax checkers can set the :cl-slot:`flycheck-error id` slot to add
IDs.  The :function:`flycheck-parse-checkstyle` parser automatically adds IDs
from the `source` attribute of errors.

The new error filters :function:`flycheck-dequalify-error-ids` and
:function:`flycheck-remove-error-ids` manipulate IDs of parsed errors.  The
former removes qualifications from error IDs, turning `Foo.Bar.E1` into `E1`.
The latter completely removes error IDs from parsed errors, for use in cases
where the syntax checker only outputs bogus IDs.

All built-in syntax checkers were updated to parse IDs, if possible.

The error list now shows these IDs, and :command:`flycheck-copy-errors-as-kill`
can put them into the kill ring.

Unit test library for Flycheck
==============================

The new library `flycheck-ert` provides assertions and utilities to write unit
tests for Flycheck syntax checkers.  The library is part of all Flycheck
packages.

3rd party extensions may use this library to write test cases for their syntax
checkers.  :ref:`flycheck-ert` documents the interface of this library.
