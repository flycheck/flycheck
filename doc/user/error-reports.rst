==========================
 Error reports in buffers
==========================

This document explains how Flycheck shows results of syntax checks in the
current buffer.

When a syntax check in the current buffer has finished Flycheck reports the
results of the check in the current buffer in two ways:

* Highlight errors, warnings, etc. directly in the buffer according to
  `flycheck-highlighting-mode`.
* Indicate errors, warnings, etc. in the fringe according to
  `flycheck-indication-mode`.

Error levels
============

All errors that syntax checkers report have a *level* which tells you the
severity of the error.  Flycheck has three built-in levels:

``error``
   Severe errors like syntax or type errors.

``warning``
   Potential but not fatal mistakes which you should likely fix nonetheless.

``info``
   Purely informational messages which inform about notable things in the
   current buffer, or provide additional help to fix errors or warnings.

Each error level has a distinct highlighting and colour which helps you to
identify the severity of each error right in the buffer.

Error highlights
================

Flycheck highlights errors directly in the buffer according to
`flycheck-highlighting-mode`:

.. defcustom:: flycheck-highlighting-mode

   How Flycheck highlights errors and warnings in the buffer:

   ``nil``
      Do not highlight anything at all.

   ``lines``
      Highlight the whole line and discard any information about the column.

   ``columns``
      Highlight the column of the error if any, otherwise like ``lines``.

   ``symbols``
      Highlight the entire symbol around the error column if any, otherwise like
      ``columns``.  This is this default.

   ``sexps``
      Highlight the entire expression around the error column if any, otherwise
      like ``columns``.

   .. warning::

      In some major modes ``sexps`` is *very* slow, because discovering
      expression boundaries efficiently is hard.

      The built-in ``python-mode`` is known to suffer from this issue.

      Be careful when enabling this mode.

The highlights use the following faces depending on the error level:

.. defface:: flycheck-error
             flycheck-warning
             flycheck-info

   The highlighting face for ``error``, ``warning`` and ``info`` levels
   respectively.

Fringe icons
============

In GUI frames Flycheck also adds icons to the fringe—the left or right border of
an Emacs window—to help you identify erroneous lines quickly:

.. defcustom:: flycheck-indication-mode

   How Flycheck indicates errors and warnings in the buffer fringes:

   ``left-fringe`` or ``right-fringe``
      Use the left or right fringe respectively.

   ``nil``
      Do not indicate errors and warnings in the fringe.

.. defface:: flycheck-fringe-error
             flycheck-fringe-warning
             flycheck-fringe-info

   The icon faces for ``error``, ``warning`` and ``info`` levels respectively.

Error thresholds
================

To avoid flooding a buffers with excessive highlighting, cluttering the
appearance and slowing down Emacs, Flycheck takes precautions against syntax
checkers that report a large number of errors exceeding
`flycheck-checker-error-threshold`:

.. defcustom:: flycheck-checker-error-threshold

   The maximum number of errors a syntax checker is allowed to report.

   If a syntax checker reports more errors the error information is
   **discarded**.  To not run into the same issue again on the next syntax check
   the syntax checker is automatically added to `flycheck-disabled-checkers` in
   this case to disable it for the next syntax check.

Clear results
=============

You can explicitly remove all highlighting and indication and all error
information from a buffer:

.. define-key:: C-c ! C
                M-x flycheck-clear

   Clear all reported errors, all highlighting and all indication icons from the
   current buffer.

.. define-key:: C-u C-c ! C
                C-u M-x flycheck-clear

   Like `C-c ! C` but also interrupt any syntax check currently running.  Use
   this command if you think that Flycheck is stuck.
