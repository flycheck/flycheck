=======================
 See errors in buffers
=======================

When a syntax check in the current buffer has finished Flycheck reports the
results of the check in the current buffer in two ways:

* Highlight errors, warnings, etc. directly in the buffer according to
  `flycheck-highlighting-mode` and `flycheck-highlighting-style`.
* Indicate errors, warnings, etc. in the fringe according to
  `flycheck-indication-mode`.

Additionally Flycheck indicates its current state and the number of errors and
warnings in the mode line.

The following screenshot illustrates what this looks like in the default Emacs
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
`flycheck-highlighting-mode` and `flycheck-highlighting-style`.

Most checkers report a single error position, not a range, so Flycheck typically
needs to guess how far to extend the highlighting: by default, it highlights the
whole symbol at the location reported by the checker, as in the screenshot
above, but you can change that range (or even disable highlighting completely)
using `flycheck-highlighting-mode`.

.. defcustom:: flycheck-highlighting-mode

   How Flycheck chooses which buffer region to highlight:

   ``nil``
      Do not highlight anything at all.

   ``lines``
      Highlight the whole line and discard any information about the column.

   ``columns``
      Highlight the column of the error if any, otherwise like ``lines``.

   ``symbols``
      Highlight the entire symbol around the error column if any, otherwise like
      ``columns``.  This is the default.

   ``sexps``
      Highlight the entire expression around the error column if any, otherwise
      like ``columns``.

   .. warning::

      In some major modes ``sexps`` is *very* slow, because discovering
      expression boundaries is costly.

      The built-in ``python-mode`` is known to suffer from this issue.

      Be careful when enabling this mode.

Conversely, when a checker reports a range, Flycheck uses that.

The style of the highlighting is determined by the value of
`flycheck-highlighting-style`.  By default, Flycheck highlights error text with
a face indicating the severity of the error (typically, this face applies a
coloured wavy underline).  Instead of faces, however, Flycheck can also indicate
erroneous text by inserting delimiters around it (checkers sometimes report
errors that span a large region of the buffer, making underlines distracting, so
in fact Flycheck only applies a face if the error spans less than 5 lines; this
is achieved using the ``conditional`` style described below).

.. defcustom:: flycheck-highlighting-style

   How Flycheck highlights error regions.

   ``nil``
     Do not indicate error regions.

   ``level-face``
      Apply a face to erroneous text.

   ``(delimiters BEFORE AFTER)``
      Bracket the error text between ``BEFORE`` and ``AFTER``, which can be
      strings, images, etc.  Chars are handled specially: they are repeated
      twice to form double brackets.

   ``(conditional NLINES S1 S2)``
      Choose between styles ``S1`` and ``S2``: ``S1`` if the error covers up to
      ``NLINES``, and ``S2`` otherwise.

To change the style of the underline or use different colours in the
``level-face`` style, customize the following faces, which are used depending on
the error level:

.. defface:: flycheck-error
             flycheck-warning
             flycheck-info

   The highlighting face for ``error``, ``warning`` and ``info`` levels
   respectively.

Delimiters use the same faces as the fringe icons described below, in addition
to the `flycheck-error-delimiter` face; delimited text has the
`flycheck-delimited-error` face, which is empty by default.

.. defface:: flycheck-error-delimiter

   The face applied to ``BEFORE`` and ``AFTER`` delimiters.

.. defface:: flycheck-delimited-error

   The face applied to error text in ``delimiters`` style.

Fringe and margin icons
=======================

In GUI frames, Flycheck also adds indicators to the fringe—the left or right
border of an Emacs window—to help you identify erroneous lines quickly.
These indicators consist of a rightward-pointing double arrow shape coloured in
the colour of the corresponding error level.  By default the arrow is 8 pixels
wide, but a 16-pixel version is used if the fringe is `wide enough
<https://www.gnu.org/software/emacs/manual/html_node/emacs/Fringes.html>`_.

.. note::

   Flycheck extensions can define custom error levels with different fringe
   indicators.  Furthermore some Emacs distributions like Spacemacs redefine
   Flycheck’s error levels to use different indicators.  If you're using such a
   distribution please take a look at its documentation if you're unsure about
   the appearance of Flycheck's indicators.

You can customise the location of these indicators (left or right fringe) with
`flycheck-indication-mode`, which also lets you turn off these indicators
completely; additionally, you can move these indicators into the margins instead
of the fringes:

.. defcustom:: flycheck-indication-mode

   How Flycheck indicates errors and warnings in the buffer:

   ``auto``
      Use the left fringe on graphical displays and the left margin on text
      terminals, where fringes are not available.  This is the default.

   ``left-fringe`` or ``right-fringe``
      Use the left or right fringe respectively.  Fringes can only contain
      monochrome bitmaps, so Flycheck draws small pixel-art arrows.

   ``left-margin`` or ``right-margin``
      Use the left or right margin respectively.  Margins can support all of
      Emacs' rendering facilities, so Flycheck uses the ``»`` character, which
      scales with the font size.

   ``nil``
      Do not indicate errors and warnings in the fringe or in the margin.

By default, Emacs displays fringes, but not margins.  When indicators end up
in a margin and that margin is not visible, Flycheck widens it to one column
automatically, and restores it when you disable ``flycheck-mode``.  Margins
that you or other packages configured are never touched, and neither are the
fringes.

A line whose error carries a machine-applicable fix (``C-c ! f``) gets a
distinct indicator, in the error's colour, so you can spot fixable errors at a
glance, much like an editor's "fix available" lightbulb.

.. defcustom:: flycheck-fixable-indicator

   When non-nil (the default) and indicators are shown, a fixable error's line
   uses a distinct fringe bitmap or margin string
   (``flycheck-fixable-margin-str``) instead of the usual level indicator.  Set
   to nil to use the same indicator for fixable and non-fixable errors.

To switch the indication mode interactively in the current buffer, use
``M-x flycheck-set-indication-mode``.

.. code-block:: elisp

   (setq-default flycheck-indication-mode 'left-margin)
   (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)

That function sets fringes and margins to reasonable (but opinionated) defaults,
according to ``flycheck-indication-mode``.  To set your own margin and fringe
widths, use a hook and call ``flycheck-refresh-fringes-and-margins``, like this:

.. code-block:: elisp

   ;; Show indicators in the left margin
   (setq flycheck-indication-mode 'left-margin)

   ;; Adjust margins and fringe widths…
   (defun my/set-flycheck-margins ()
     (setq left-fringe-width 8 right-fringe-width 8
           left-margin-width 1 right-margin-width 0)
     (flycheck-refresh-fringes-and-margins))

   ;; …every time Flycheck is activated in a new buffer
   (add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)

The following faces control the colours of fringe and margin indicators.

.. defface:: flycheck-fringe-error
             flycheck-fringe-warning
             flycheck-fringe-info

   The icon faces for ``error``, ``warning`` and ``info`` levels respectively.

When an error spans multiple lines, Flycheck displays a hatch pattern in the
fringes or vertical dots in the margins to indicate the extent of the error.

To change the fringe bitmap or the symbol used in the margins, use the function
``flycheck-redefine-standard-error-levels``.

Inline error messages
=====================

Beyond the fringe or margin indicators, Flycheck can render the error messages
themselves right next to the code they refer to, in the spirit of VS Code's
Error Lens and the inline diagnostics of Neovim, Helix and Zed.  Enable
``flycheck-annotate-mode`` to turn this on in the current buffer, or
``global-flycheck-annotate-mode`` to turn it on everywhere Flycheck checks:

.. code-block:: elisp

   (add-hook 'after-init-hook #'global-flycheck-annotate-mode)

.. minor-mode:: flycheck-annotate-mode

   Toggle the inline display of Flycheck error messages in the current buffer.
   The annotations appear only while ``flycheck-mode`` is on.

.. minor-mode:: global-flycheck-annotate-mode

   Toggle ``flycheck-annotate-mode`` in every buffer Flycheck may check, the
   same set of buffers ``global-flycheck-mode`` enables.

Flycheck picks the display style per line: the line at point uses
``flycheck-annotate-current-line-style`` and every other line uses
``flycheck-annotate-other-lines-style``.  By default the line at point gets the
roomy ``below`` treatment while the rest get a terse ``eol`` summary, so the
error you're working on is spelled out in full without every other line
competing for attention.

.. image:: /images/flycheck-inline-annotations.png

.. defcustom:: flycheck-annotate-current-line-style
               flycheck-annotate-other-lines-style

   The display style for the line at point and for every other line,
   respectively.  Each is one of:

   ``eol``
      Append a compact message after the code.  When a line has several
      errors, show the most severe one and a count of the rest.

      .. image:: /images/flycheck-annotate-eol.png

   ``below``
      Lay the full messages out on their own lines underneath the code, one
      per error, each pointing at its column.

      .. image:: /images/flycheck-annotate-below.png

   ``sideline``
      Like ``eol``, but flush the message to the window's right edge, the way
      ``lsp-ui-sideline`` does.  When the code leaves no room, the message
      simply follows it.

      .. image:: /images/flycheck-annotate-sideline.png

   ``nil``
      Do not annotate those lines.  Setting ``flycheck-annotate-other-lines-style``
      to ``nil`` annotates only the line at point, the way Neovim and Helix
      show diagnostics for the cursor line only.

   Additional styles can be registered through
   ``flycheck-annotate-style-functions``.

.. defcustom:: flycheck-annotate-levels

   The base error levels to display inline, either ``t`` for all levels or a
   list of level symbols such as ``'(error warning)``.  Errors of other levels
   are still highlighted and listed; they just get no inline annotation.

.. defcustom:: flycheck-annotate-current-line-levels
               flycheck-annotate-other-lines-levels

   Per-tier level filters for the line at point and for every other line.  Each
   is either ``t`` to inherit ``flycheck-annotate-levels`` or a list of level
   symbols to restrict that tier further.  Setting
   ``flycheck-annotate-other-lines-levels`` to ``'(error)`` shows only errors
   away from point while the line at point still shows everything, the way Helix
   limits its non-cursor lines.

.. defcustom:: flycheck-annotate-suppress-echo

   When non-nil (the default) and the line at point is annotated inline, the
   errors at point are not additionally shown through
   ``flycheck-display-errors-function`` (Eldoc or the echo area), to avoid
   displaying the same message twice.  Set to ``nil`` to keep both.

.. defcustom:: flycheck-annotate-background

   When non-nil, tint the whole line of each error with a subtle background in
   the colour of its most severe level, in the spirit of VS Code's Error Lens.
   The tint applies to every visible error line that passes
   ``flycheck-annotate-levels``, independent of the message style, so it shows
   even on lines whose style is ``nil``.  Off by default.

   .. image:: /images/flycheck-annotate-background.png

.. defcustom:: flycheck-annotate-format-function

   The function used to format an error for inline display.  It receives a
   single ``flycheck-error`` and returns the string to show.  The default
   renders the message and the error ID.

.. defcustom:: flycheck-annotate-style-functions

   An alist mapping style symbols to their renderer functions, used to resolve
   ``flycheck-annotate-current-line-style`` and
   ``flycheck-annotate-other-lines-style``.  Add to it to register your own
   styles.

.. defcustom:: flycheck-annotate-fix-marker

   A string shown inline before an error that carries a machine-applicable fix
   (applicable with ``flycheck-fix-error-at-point``), defaulting to
   ``"[fix] "``, or nil to show no marker.  Uses the ``flycheck-annotate-fix``
   face.

.. defface:: flycheck-annotate-error
             flycheck-annotate-warning
             flycheck-annotate-info

   The faces for inline ``error``, ``warning`` and ``info`` messages
   respectively.

.. defface:: flycheck-annotate-connector

   The face for the connectors that precede ``below``-style messages.

.. defface:: flycheck-annotate-fix

   The face for the inline ``flycheck-annotate-fix-marker`` on fixable errors.

.. defface:: flycheck-annotate-error-background
             flycheck-annotate-warning-background
             flycheck-annotate-info-background

   The whole-line tint faces for ``error``, ``warning`` and ``info`` lines,
   used when ``flycheck-annotate-background`` is non-nil.  Each sets only a
   background, with ``:extend t`` so it reaches the window edge.

.. note::

   ``flycheck-annotate-mode`` obsoletes the third-party `flycheck-inline
   <https://github.com/flycheck/flycheck-inline>`_ package, which pioneered
   inline error messages for Flycheck.  If you used it, drop it from your
   configuration and enable ``flycheck-annotate-mode`` instead.  The built-in
   mode differs in a few ways:

   * It ships with Flycheck, so there is nothing extra to install.
   * It annotates every error in the visible window, not only the error at
     point.
   * It offers two layouts, ``eol`` (after the line) and ``below`` (on their
     own lines underneath), and picks between them per line, instead of a
     single placement below the error.
   * It adds level filtering (``flycheck-annotate-levels``), a custom
     formatter (``flycheck-annotate-format-function``), echo-area suppression
     (``flycheck-annotate-suppress-echo``), and user-registrable styles
     (``flycheck-annotate-style-functions``).

Mode line
=========

Like all minor modes Flycheck also has a mode line indicator.  You can see it in
the bottom right corner of the above screenshot.  By default the indicator shows
Flycheck’s current state via one of the following texts:

+------------------+-------------------------------------------------------------+
|``FlyC``          |Current buffer has not been checked.                         |
+------------------+-------------------------------------------------------------+
|``FlyC*``         |Flycheck is checking the buffer currently.                   |
+------------------+-------------------------------------------------------------+
|``FlyC:0``        |Last check resulted in no errors, no warnings and no info    |
|                  |messages.                                                    |
+------------------+-------------------------------------------------------------+
|``FlyC:3|5|1``    |There are three errors, five warnings and one info message in|
|                  |the current buffer.                                          |
+------------------+-------------------------------------------------------------+
|``FlyC-``         |Flycheck did not find a syntax checker for the current       |
|                  |buffer.  Take a look at the :ref:`list of supported languages|
|                  |<flycheck-languages>` and type `C-c ! v` to see what checkers|
|                  |are available for the current buffer.                        |
+------------------+-------------------------------------------------------------+
|``FlyC!``         |The last syntax check failed.  Inspect the ``*Messages*``    |
|                  |buffer, look for error messages, and consider                |
|                  |:ref:`reporting a bug <flycheck-bug-reports>`.               |
+------------------+-------------------------------------------------------------+
|``FlyC.``         |The last syntax check was manually interrupted.              |
+------------------+-------------------------------------------------------------+
|``FlyC?``         |The last syntax check had a dubious result.  The definition  |
|                  |of a syntax checker may have a bug.  Inspect the             |
|                  |``*Messages*`` buffer and consider :ref:`reporting a bug     |
|                  |<flycheck-bug-reports>`.                                     |
+------------------+-------------------------------------------------------------+

You can entirely customise the mode line indicator with `flycheck-mode-line`:

.. defcustom:: flycheck-mode-line

   A “mode line construct” for Flycheck’s mode line indicator.

You can also customize the indicator for a successful run (no errors/warnings found) with `flycheck-mode-success-indicator`:

.. defcustom:: flycheck-mode-success-indicator

   Success indicator appended to `flycheck-mode-line-prefix`. Set to ":0" by default.

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

To avoid flooding a buffer with excessive highlighting, cluttering the
appearance and slowing down Emacs, Flycheck takes precautions against syntax
checkers that report a large number of errors exceeding
`flycheck-checker-error-threshold`:

.. defcustom:: flycheck-checker-error-threshold

   The maximum number of errors a syntax checker is allowed to report.

   What happens to the excessive errors is controlled by
   `flycheck-checker-error-threshold-action`.

.. defcustom:: flycheck-checker-error-threshold-action

   What to do when a syntax checker exceeds the error threshold.

   By default (``truncate``), Flycheck keeps the most severe errors up to the
   threshold and discards the rest.  The mode line signals the truncation with
   a ``+`` after the error counts (e.g. ``FlyC:400|12|3+``), and the error
   list shows how many errors were suppressed.

   When set to ``disable``, Flycheck instead discards **all** errors from the
   syntax checker and disables it in the buffer for subsequent syntax checks,
   like older Flycheck versions did.  Use :kbd:`C-u C-c ! x` to re-enable a
   disabled checker.  This avoids re-parsing excessive checker output on every
   syntax check, which can be preferable for pathological cases.

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
