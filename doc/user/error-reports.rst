==========================
 Error reports in buffers
==========================

This document explains how Flycheck shows results of syntax checks in the
current buffer.

.. todo:: Mention error levels

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

.. option:: flycheck-highlighting-mode

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

.. face:: flycheck-error
          flycheck-warning
          flycheck-info

   The highlighting face for ``error``, ``warning`` and ``info`` levels
   respectively.

Fringe icons
============

.. option:: flycheck-indication-mode

   .. todo::

.. face:: flycheck-fringe-error
          flycheck-fringe-warning
          flycheck-fringe-info

Error thresholds
================

.. todo::

.. option:: flycheck-checker-error-threshold

   .. todo::

Clear results
=============

.. command:: C-c ! C
             M-x flycheck-clear

   .. todo::
