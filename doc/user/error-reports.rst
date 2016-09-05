=======================
 See errors in buffers
=======================

When a syntax check in the current buffer has finished Flycheck reports the
results of the check in the current buffer in two ways:

* Highlight errors, warnings, etc. directly in the buffer according to
  `flycheck-highlighting-mode`.
* Indicate errors, warnings, etc. in the fringe according to
  `flycheck-indication-mode`.

Additionally Flycheck indicates its current state and the number of errors and
warnings in the mode line.

The following screenshot illustrates how this looks like in the default Emacs
color theme.  It shows an info, a warning and an error annotation, from top to
bottom.  Please also note the fringe indicators on the left side and the
emphasized mode line indicator in the bottom right corner:

.. image:: /images/flycheck-error-reports.png
   :alt: Flycheck showing info, warning and error annotations
   :align: center

.. note::

   The colours of fringe icons and the whole appearance of the error highlights
   depend on the active color theme.  Although red, orange and green or blue
   seem to be somewhat standard colours for Flycheck’s annotations across many
   popular themes, please take a closer look at your color theme if you’re in
   doubt about the meaning of a Flycheck highlight.

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
`flycheck-highlighting-mode`.  By default these highlights consist of a coloured
wave underline which spans the whole symbol at the error location as in the
screenshot above but the highlights are entirely customisable.  You can change
the extents of highlighting or disable it completely with
`flycheck-highlighting-mode`, or customise Flycheck’s faces to change the style
of the underline or use different colours.

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

In GUI frames Flycheck also adds indicators to the fringe—the left or right
border of an Emacs window that is—to help you identify erroneous lines quickly.
These indicators consist of a rightward-pointing double arrow shape coloured in
the colour of the corresponding error level.

.. note::

   Flycheck extensions can define custom error levels with different fringe
   indicators.  Furthermore some Emacs distributions like Spacemacs redefine
   Flycheck’s error levels to use different indicators.  If you're using such a
   distribution please take a look at its documentation if you're unsure about
   the appearance of Flycheck's indicators.

   Note that we discourage you from changing the shape of Flycheck’s fringe
   indicators.

You can customise the location of these indicators (left or right fringe) with
`flycheck-indication-mode` which also lets you turn off these indicators
completely:

.. defcustom:: flycheck-indication-mode

   How Flycheck indicates errors and warnings in the buffer fringes:

   ``left-fringe`` or ``right-fringe``
      Use the left or right fringe respectively.

   ``nil``
      Do not indicate errors and warnings in the fringe.

The following faces control the colours of the fringe indicators.  However they
do not let you change the shape of the indicators—to achieve this you'd have to
redefine the error levels with `flycheck-define-error-level`.

.. defface:: flycheck-fringe-error
             flycheck-fringe-warning
             flycheck-fringe-info

   The icon faces for ``error``, ``warning`` and ``info`` levels respectively.

Mode line
=========

Like all minor modes Flycheck also has a mode line indicator.  You can see it in
the bottom right corner of the above screenshot.  By default the indicator shows
Flycheck’s current state via one of the following texts:

+-------------+----------------------------------------------------------------+
|``FlyC*``    |Flycheck is checking the buffer currently.                      |
+-------------+----------------------------------------------------------------+
|``FlyC``     |There are no errors or warnings in the current buffer.          |
+-------------+----------------------------------------------------------------+
|``FlyC:3/5`` |There are three errors and five warnings in the current buffer. |
+-------------+----------------------------------------------------------------+
|``FlyC-``    |Flycheck did not find a syntax checker for the current buffer.  |
|             |Take a look at the :ref:`list of supported languages            |
|             |<flycheck-languages>` and type `C-c ! v` to see what checkers   |
|             |are available for the current buffer.                           |
+-------------+----------------------------------------------------------------+
|``FlyC!``    |The last syntax check failed.  Inspect the ``*Messages*`` buffer|
|             |look for error messages, and consider :ref:`reporting a bug     |
|             |<flycheck-bug-reports>`.                                        |
+-------------+----------------------------------------------------------------+
|``FlyC?``    |The last syntax check had a dubious result.  The definition of a|
|             |syntax checker may have a bug.  Inspect the ``*Messages*``      |
|             |buffer and consider :ref:`reporting a bug                       |
|             |<flycheck-bug-reports>`.                                        |
+-------------+----------------------------------------------------------------+

You can entirely customise the mode line indicator with `flycheck-mode-line`:

.. defcustom:: flycheck-mode-line

   A “mode line construct” for Flycheck’s mode line indicator.

.. seealso::

   :infonode:`(elisp)Mode Line Data`
      Documentation of mode line constructs.
   flycheck-status-emoji_
      A Flycheck extension which puts emojis into Flycheck's mode line
      indicator.
   :flyc:`flycheck-color-mode-line`
      A Flycheck extension which colours the entire mode line according to
      Flycheck's status.

.. _flycheck-status-emoji: https://github.com/liblit/flycheck-status-emoji

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
