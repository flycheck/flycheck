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
:kbd:`f`     Filter the error list by level
:kbd:`F`     Remove the filter
:kbd:`S`     Sort the error list by the column at point
:kbd:`g`     Check the source buffer and update the error list
:kbd:`q`     Quit the error list and hide its window
==========   ====

Filter the list
===============

By the default the error list shows all errors but sometimes you'd like to hide
warnings to focus only on real errors.  The error list lets you hide all errors
below a certain level with :kbd:`f`.  This key prompts for an error level and
will remove all errors of lower levels from the list.  The filter is permanent
as long as the error list buffer stays alive or the filter is reset with
:kbd:`F`.

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

By default the error list buffer pops up like any other buffer.  Flycheck does
not enforce special rules on how it's displayed and where it's located in the
frame so essentially the error list pops up at arbitrary places wherever Emacs
can find a window for it.

However you can tell Emacs to obey certain rules when displaying buffers by
customizing the built-in option `display-buffer-alist`.  You can use this option
to make the error list display like similar lists in contemporary IDEs like
VisualStudio, Eclipse, etc. with the following code in your :term:`init file`:

.. code-block:: elisp

   (add-to-list 'display-buffer-alist
                `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

This display rule tells Emacs to always display the error list at the bottom
side of the frame, occupying a third of the entire height of the frame.

.. seealso::

   Shackle_
      An Emacs package which provides an alternative way to control buffer
      display

.. _shackle: https://github.com/wasamasa/shackle
