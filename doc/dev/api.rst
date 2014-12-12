.. _flycheck-api:

==============
 Flycheck API
==============

.. require:: flycheck

This chapter defines and documents the public API of Flycheck.  You can use this
API to define your own syntax checkers, or extend existing syntax checkers, or
by extending Flycheck in entirely new ways, e.g. to add some sophisticated error
analysis.

You will also find this document helpful if you want to develop Flycheck itself.

.. contents:: Contents
   :local:

.. _api-syntax-checks:

Syntax checks
=============

A syntax check performs the following steps:

1. Run hooks in :hook:`flycheck-before-syntax-check-hook`
2. Clear error information from previous syntax checks.
3. Select a :term:`suitable syntax checker`.
4. Copy the contents of the buffer to be checked to a temporary file.
5. Run the syntax checker.
6. Parse the output of the tool, and report all errors and warnings, via
   :hook:`flycheck-process-error-functions`
7. If the buffer can be checked with another syntax checker, continue from step
   4, but with the next syntax checker.  This is called :term:`chaining` of
   syntax checkers.
8. Run hooks in :hook:`flycheck-after-syntax-check-hook`.

.. hook:: flycheck-after-syntax-check-hook
   :auto:

.. hook:: flycheck-before-syntax-check-hook
   :auto:

.. hook:: flycheck-syntax-check-failed-hook
   :auto:

.. _api-generic-syntax-checkers:

Generic syntax checkers
=======================

Generic syntax checkers are the most basic syntax checkers in Flycheck.  These
syntax checkers use an Emacs Lisp function to check the current buffer.

.. function:: flycheck-define-generic-checker
   :auto:

   .. seealso:: :ref:`api-status-callback-protocol`

To make new syntax checkers available for automatic selection you need to
register them by adding them to :option:`flycheck-checkers`.

.. function:: flycheck-registered-checker-p
   :auto:

You can extend generic syntax checkers to add new modes or chain further
syntax checkers:

.. function:: flycheck-add-mode
   :auto:

.. function:: flycheck-add-next-checker
   :auto:

.. _api-status-callback-protocol:

Status callback protocol
------------------------

The ``callback`` argument to the `:start` function of a syntax checker defined
with :function:`flycheck-define-generic-checker` is a function taking two
arguments:

.. function:: status-callback status &optional data

   `status` is one of the following symbols, denoting the status being reported:

   `errored` (*finishing*)
      The syntax checker has errored.  `data` is an optional error message as
      string.

   `interrupted` (*finishing*)
      The syntax checker was interrupted.  `data` is ignored in this case.

   `finished` (*finishing*)
      The syntax checker has finished to check the buffer and reported some
      errors.  DATA is the list of :cl-struct:`flycheck-error` objects reported
      by the syntax check.

   `suspicious`
      The syntax checker encountered some suspicious state (like a potential
      fault in the syntax checker definition), which the user needs to be
      informed about.

   A *finishing* `status` symbol finishes the current syntax check, and allows
   Flycheck to conduct further syntax checks.  A syntax checker **must** call
   the callback at least once with a *finishing* `status` symbol.

   .. warning::

      Failure to call the callback will cause Flycheck to get stuck at the
      current syntax check.

Predicates for syntax checkers
------------------------------

Flycheck provides some ready-to-use predicates for generic checkers:

.. function:: flycheck-buffer-saved-p
   :auto:

Error filters
-------------

Additionally, Flycheck has some built-in error filters for generic checkers:

.. function:: flycheck-sanitize-errors
   :auto:

.. function:: flycheck-increment-error-columns
   :auto:

.. function:: flycheck-collapse-error-message-whitespace
   :auto:

.. function:: flycheck-dedent-error-messages
   :auto:

.. function:: flycheck-fold-include-errors
   :auto:

.. function:: flycheck-dequalify-error-ids
   :auto:

.. function:: flycheck-remove-error-ids
   :auto:

.. _api-command-syntax-checkers:

Command syntax checkers
=======================

Command syntax checkers are a specialised variant of generic syntax checkers
which use an external command to check the contents of the current buffer.  To
define a new command syntax checker, use
:function:`flycheck-define-command-checker`:

.. function:: flycheck-define-command-checker
   :auto:

Each command checker uses an accompanying variable to override the executable,
which you can define with :macro:`flycheck-def-executable-var`:

.. macro:: flycheck-def-executable-var
   :auto:

Since command syntax checkers are by far the most common kind of syntax
checkers, Flycheck provides a convenience macro :macro:`flycheck-define-checker`
which wraps up :function:`flycheck-define-command-checker` and
:macro:`flycheck-def-executable-var` into a single macro, and reduces syntactic
clutter by allowing unquoted arguments:

.. macro:: flycheck-define-checker
   :auto:

Command arguments
-----------------

The arguments passed to the external command of a command syntax checker are
subject to substitution with :function:`flycheck-substitute-argument`, which
replaces special symbols and forms with strings for consumption by the external
command:

.. function:: flycheck-substitute-argument
   :auto:

Options for command syntax checkers
-----------------------------------

Command syntax checkers can define options, whose values are substituted into
the external command.

.. macro:: flycheck-def-option-var
   :auto:

.. macro:: flycheck-def-args-var
   :auto:

Flycheck provides some built-in filters for such options:

.. function:: flycheck-option-int
   :auto:

.. function:: flycheck-option-comma-separated-list
   :auto:

.. _api-configuration-files:

Configuration files for command syntax checkers
-----------------------------------------------

Additionally, command syntax checkers can pass configuration files to external
commands.

.. macro:: flycheck-def-config-file-var
   :auto:

Flycheck tries to find an appropriate file based on the value of configuration
file variables, and substitutes the path to that file into the external command.

.. function:: flycheck-locate-config-file
   :auto:

Error parsing with regular expressions
--------------------------------------

Normally, command syntax checkers use regular expressions to extract errors from
the output.  For simplicity and readability, Flycheck uses RX expressions
instead of standard regular expressions, and provides some custom RX forms for
frequent patterns, implemented by :function:`flycheck-rx-to-string`:

.. function:: flycheck-rx-to-string
   :auto:

Internally, error parsing with regular expressions is implemented with a special
error parser:

.. function:: flycheck-parse-with-patterns
   :auto:

.. _api-error-parsers:

Error parsers
-------------

Alternatively, command syntax checkers can use custom functions to parse errors
from the command output.  Flycheck provides some built-in error parsers for
standard output formats:

.. function:: flycheck-parse-checkstyle
   :auto:

You can also write your own error parsers.  An error parser is a function with
the following signature:

.. function:: flycheck-error-parser output checker buffer

   `output` is the output of the command as string.  `checker` is the syntax
   checker from which the output comes, and `buffer` is the buffer that was
   checked.

The following functions can aid you in writing custom parsers:

.. function:: flycheck-parse-xml-string
   :auto:

.. _api-errors:

Errors
======

The list of errors in a buffer is stored in the local variable
:variable:`flycheck-current-errors`:

.. variable:: flycheck-current-errors
   :auto:

Flycheck errors are represented by the CL structure :cl-struct:`flycheck-error`.
See :infonode:`(cl)Structures` for more information about CL structures.

.. cl-struct:: flycheck-error

   A Flycheck error with the following slots.  Each of these slots may be `nil`.

   .. cl-slot:: buffer

      The buffer object referring to the buffer this error belongs to.

   .. cl-slot:: checker

      The syntax checker that reported this error.

   .. cl-slot:: filename

      A string containing the filename the error refers to.

   .. cl-slot:: line

      An integer providing the line the error refers to.

   .. cl-slot:: column

      An *optional* integer providing the column the error refers to.

      If this attribute is `nil`, Flycheck will assume that the error refers to
      the whole line.

      .. warning::

         For compatibility with external programs and **unlike** Emacs itself
         (e.g. in Compile Mode), Flycheck uses 1-based columns, not 0-based: The
         first character on a line is column 1.

         This is the format used by most external programs, but occasionally a
         program tries to proactively adapt to Emacs' convention, and outputs
         0-based columns.  In this case, you need to adapt the column numbers
         for Flycheck, via :function:`flycheck-increment-error-columns` as
         `:error-filter`.

   .. cl-slot:: message

      The human-readable error message as string.

   .. cl-slot:: level

      The error level of the message, as symbol denoting an error level defined
      with :function:`flycheck-define-error-level`.

   .. cl-slot:: id

      An *optional* unique identifier for this kind of error.

      This field should identify the kind of an error, not the individual error
      itself.

   There are two constructors to create new :cl-struct:`flycheck-error` objects:

   .. function:: flycheck-error-new-at line column &optional level message &key \
                    checker id filename buffer

      Create a new Flycheck error at the given :var:`line` and :var:`column`.

      :var:`line` and :var:`column` refer to the :cl-slot:`line` and
      :cl-slot:`column` of the new error.  The optional :var:`level` and
      :var:`message` arguments fill the :cl-slot:`level` and cl-slot:`message`
      slots respectively.

      :var:`checker`, :var:`id`, :var:`filename` and :var:`buffer` are keyword
      arguments, for :cl-slot:`checker`, :cl-slot:`id`, :cl-slot:`filename` and
      :cl-slot:`buffer` respectively.  :var:`buffer` defaults to the current
      buffer and :var:`filename` to the file name of the current buffer.  The
      other keyword arguments default to `nil`.

      .. warning::

         Due to a limitation of Common Lisp functions in Emacs Lisp, you must
         specify **all** optional arguments, that is, **both** :var:`level`
         **and** :var:`message`, to pass any keyword arguments.

   .. function:: flycheck-error-new &rest attributes

      Create a new :cl-struct:`flycheck-error` with the given :var:`attributes`.

      :var:`attributes` is a property list, where each property specifies the
      value for the corresponding slot of :cl-struct:`flycheck-error`, for
      instance:

      .. code-block:: cl

         (flycheck-error-new :line 10 :column 5 :message "Foo" :level 'warning)

   The following functions and macros work on errors:

   .. macro:: flycheck-error-with-buffer
      :auto:

   .. function:: flycheck-error-line-region
      :auto:

   .. function:: flycheck-error-column-region
      :auto:

   .. function:: flycheck-error-thing-region
      :auto:

   .. function:: flycheck-error-pos
      :auto:

   .. function:: flycheck-error-format
      :auto:

   .. function:: flycheck-error-<
      :auto:

   .. function:: flycheck-error-level-<
      :auto:

Error processing
----------------

.. hook:: flycheck-process-error-functions

.. function:: flycheck-add-overlay

Error analysis
--------------

Flycheck provides some functions for rudimentary error analysis:

.. function:: flycheck-count-errors
   :auto:

.. function:: flycheck-has-errors-p
   :auto:

.. function:: flycheck-has-max-errors-p
   :auto

Error levels
------------

Flycheck provides three built-in error levels:

`error`
   Severe errors which cannot be ignored
`warning`
   Potential errors which can be ignored
`info`
   Informational annotations

You can define new error levels with :function:`flycheck-define-error-level`:

.. function:: flycheck-define-error-level
   :auto:

.. function:: flycheck-error-level-p
   :auto:

.. _api-flycheck-buffer-status:

Flycheck buffer status
======================

.. hook:: flycheck-status-changed-functions
   :auto:

.. function:: flycheck-report-status
   :auto:

.. variable:: flycheck-last-status-change
   :auto:

.. function:: flycheck-mode-line-status-text
   :auto:

.. _api-utilities:

Utilities
=========

.. function:: flycheck-string-list-p
   :auto:

.. function:: flycheck-symbol-list-p
   :auto:
