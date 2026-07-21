.. _flycheck-error-list:

=================
 List all errors
=================

You can see all errors in the current buffer in Flycheck’s error list:

.. image:: /images/flycheck-error-list.png
   :align: center

The key `C-c ! l` pops up the error list:

.. define-key:: C-c ! l
                M-x flycheck-list-errors
                M-x list-flycheck-errors

   Pop up a list of errors in the current buffer.

The error list automatically updates itself after every syntax check and follows
the current buffer: If you switch to different buffer or window it automatically
shows the errors of the now current buffer.  The buffer whose errors are shown
in the error list is the *source buffer*.

Whenever the point is on an error in the *source buffer* the error list
highlights these errors—the green line in the screenshot above.

Within the error list the following key bindings are available:

==========   ====
:kbd:`RET`   Go to the current error in the source buffer
:kbd:`n`     Jump to the next error
:kbd:`p`     Jump to the previous error
:kbd:`e`     Explain the error
:kbd:`x`     Apply the error's fix, if it has one
:kbd:`f`     Filter the error list by level
:kbd:`F`     Remove the filter
:kbd:`S`     Sort the error list by the column at point
:kbd:`g`     Check the source buffer and update the error list
:kbd:`P`     Toggle between buffer and whole-project scope
:kbd:`M-1`   Show a flat list, without grouping
:kbd:`M-2`   Toggle grouping the errors by file
:kbd:`M-3`   Toggle grouping the errors by checker
:kbd:`M-4`   Toggle grouping the errors by level
:kbd:`TAB`   Collapse or expand the group at point
:kbd:`q`     Quit the error list and hide its window
==========   ====

When you jump to an error from the error list with :kbd:`RET`, Flycheck runs
`flycheck-error-list-after-jump-hook` in the source buffer.  This is useful for
post-jump actions like recentering:

.. defcustom:: flycheck-error-list-after-jump-hook

   Functions to run after jumping to an error from the error list.  For
   example, to recenter the window on the error location:

   .. code-block:: elisp

      (add-hook 'flycheck-error-list-after-jump-hook #'recenter)

See the whole project
=====================

Press :kbd:`P` to switch the error list between the *current buffer* and the
*whole project*.  In project scope the list aggregates the diagnostics of every
open Flycheck buffer in the source buffer's project, together with the
cross-file errors your checkers report but the per-buffer view discards.  A
checker like ``tsc``, ``cargo check`` or ``mypy`` that checks a whole package
reports errors for files other than the one you are editing; Flycheck normally
drops those from the buffer (see `flycheck-relevant-error-other-file-show`), but
in project scope it keeps them, so you see the whole picture in one place.
Press :kbd:`RET` on an error in another file to jump straight to it.

.. note::

   Flycheck does not run checkers on files you haven't opened; project scope
   surfaces the diagnostics it already has, from open buffers and from the
   cross-file output of the checks you run.  It does not turn Flycheck into a
   background whole-project linter.  A cross-file error therefore reflects the
   last check that reported it: it stays until the buffer that produced it is
   checked again, since Flycheck cannot tell that another file changed without
   re-running a check.

The project of a buffer is Emacs' project (see `project-current`) when one is
found, and the checker's working directory otherwise.  The current scope is
shown as ``[project]`` in the error list's mode line.

Group the errors under a header per file, syntax checker or level with
:kbd:`M-2`, :kbd:`M-3` and :kbd:`M-4`; :kbd:`M-1` switches back to a flat list.
Grouping makes a project-wide list much easier to scan.  The keys toggle each
dimension, so you can combine them: with both :kbd:`M-2` and :kbd:`M-3` the
errors nest by checker within each file.  Combined dimensions always nest in
the file, checker, level order.  The available groupings and the active ones
are shown in a strip at the top of the error list.  Press :kbd:`TAB` (or
:kbd:`RET` on a header) to collapse or expand the group at point.  You can
also click a grouping in the strip to toggle it, or a group header to collapse
or expand it.

Filter the list
===============

By default the error list shows all errors but sometimes you'd like to narrow
it down.  The error list supports three filters, which combine and stay in
effect as long as the error list buffer stays alive, until you reset them all
with :kbd:`F`:

* :kbd:`f` prompts for an error level and hides all errors of lower levels.
* :kbd:`c` prompts for a syntax checker and shows only its errors, e.g. only
  the type errors from your type checker while ignoring stylistic complaints.
* :kbd:`/` prompts for a regular expression and shows only errors whose
  message or ID matches it.

The active filters are shown in the error list's mode line.

Sort the list
=============

You can press :kbd:`S` or click on the column headings to sort the error list by
any of the following columns:

* Line
* Level
* ID
* Message and checker

Click twice or press :kbd:`S` repeatedly to flip the sort order from ascending
to descending or vice versa.

Tune error list display
=======================

By default the error list pops up in a side window at the bottom of the frame,
a quarter of the frame tall, like similar lists in contemporary IDEs.  Side
windows are not affected by :kbd:`C-x 1` (`delete-other-windows`); dismiss the
error list with :kbd:`q` in its window instead.  You can change or disable
this behavior:

.. defcustom:: flycheck-error-list-display-buffer-action

   The `display-buffer` action used to display the error list.  Set to ``nil``
   to fall back to the default behavior of `display-buffer`, where the error
   list pops up at an arbitrary place wherever Emacs finds a window for it.

Entries in the built-in option `display-buffer-alist` matching the error list
buffer take precedence over this action, so any existing window-management
configuration keeps working.  For example, to display the error list at the
bottom with a third of the frame height instead:

.. code-block:: elisp

   (add-to-list 'display-buffer-alist
                `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

.. seealso::

   Shackle_
      An Emacs package which provides an alternative way to control buffer
      display

.. _shackle: https://github.com/wasamasa/shackle
