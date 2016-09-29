======================
 Interact with errors
======================

There are a couple of things that you can do with Flycheck errors in a buffer:

* You can navigate to errors, and go to the next or previous error.
* You can display errors to read their error messages.
* You can put error messages and IDs into the kill ring.

This section documents the corresponding commands and their customisation
options.

Navigate errors
===============

By default Flycheck hooks into Emacs’ standard error navigation on :kbd:`M-g n`
(`next-error`) and :kbd:`M-g p` (`previous-error`).  When :mode:`flycheck` is
enabled these commands will jump to the next and previous Flycheck error
respectively.  See :infonode:`(emacs)Compilation Mode` for more information
about these commands.

This way you don’t need to learn special keybindings to navigate Flycheck
errors; navigation should just work out of the box.

.. note::

   Visible compilation buffers such as buffers from ``M-x compile``, ``M-x
   grep``, etc. still take *precedence* over Flycheck’s errors.

The exact behaviour of these error navigation features is very context-dependent
and can be very confusing at times so you can disable this integration:

.. defcustom:: flycheck-standard-error-navigation

   Whether to integrate Flycheck errors into Emacs’ standard error navigation.
   Defaults to ``t``, set to ``nil`` to disable.

   .. important::

      When changing the value you must disable :mode:`flycheck` and enable it
      again for the change to take effect in any buffers where :mode:`flycheck`
      is enabled.

Flycheck provides an independent set of navigation commands which will always
navigate Flycheck errors in the current buffer, regardless of visible
compilation buffers and `flycheck-standard-error-navigation`:

.. define-key:: C-c ! n
                M-x flycheck-next-error

   Jump to the next error.

   With prefix argument jump forwards by as many errors as specified by the
   prefix argument, e.g. :kbd:`M-3 C-c ! n` will move to the 3rd error from the
   current point.  With negative prefix argument move to previous errors
   instead.  Signal an error if there are no more Flycheck errors.

.. define-key:: C-c ! p
                M-x flycheck-previous-error

   Jump to the previous Flycheck error.

   With prefix argument jump backwards by as many errors as specified by the
   prefix argument, e.g. :kbd:`M-3 C-c ! p` will move to the 3rd error before
   the current point.  With negative prefix argument move to next errors
   instead.  Signal an error if there are no more Flycheck errors.

.. define-key:: M-x flycheck-first-error

   Jump to the first Flycheck error.

   With prefix argument, jump forwards to by as many errors as specified by the
   prefix argument, e.g. :kbd:`M-3 M-x flycheck-first-error` moves to the 3rd
   error from the beginning of the buffer.  With negative prefix argument move
   to the last error instead.

By default error navigation jumps to all errors but you can choose to skip over
errors with low levels:

.. defcustom:: flycheck-navigation-minimum-level

   The minimum levels of errors to consider for navigation.

   If set to an error level only navigate to errors whose level is as least as
   severe as this one. If ``nil`` navigate to all errors.

Display errors
==============

Whenever you move point to an error location Flycheck automatically displays all
Flycheck errors at point after a short delay which you can customise:

.. defcustom:: flycheck-display-errors-delay

   The number of seconds to wait before displaying the error at point. Floating
   point numbers can express fractions of seconds.

By default Flycheck shows the error messages in the minibuffer or in a separate
buffer if the minibuffer is too small to hold the whole error message but this
behaviour is entirely customisable:

.. defcustom:: flycheck-display-errors-function

   A function to display errors.

   The function is given the list of Flycheck errors to display as sole argument
   and shall display these errors to the user in some way.

Flycheck provides two built-in functions for this option:

.. defun:: flycheck-display-error-messages errors
           flycheck-display-error-messages-unless-error-list errors

   Show error messages and IDs in the echo area or in a separate buffer if the
   echo area is too small (using `display-message-or-buffer` which see).  The
   latter only displays errors when the :ref:`error list <flycheck-error-list>`
   is not visible.  To enable it add the following to your :term:`init file`:

   .. code-block:: elisp

      (setq flycheck-display-errors-function
            #'flycheck-display-error-messages-unless-error-list)

.. seealso::

   :flyc:`flycheck-pos-tip`
      A Flycheck extension to display errors in a GUI popup.

Additionally Flycheck shows errors in a GUI tooltip whenever you hover an error
location with the mouse pointer.  By default the tooltip contains the messages
and IDs of all errors under the pointer, but the contents are customisable:

.. defcustom:: flycheck-help-echo-function

   A function to create the contents of the tooltip.

   The function is given a list of Flycheck errors to display as sole argument
   and shall return a single string to use as the contents of the tooltip.

Explain errors
==============

Flycheck also has the ability to display explanations for errors, provided the
error checker is capable of producing these explanations.  Currently, only the
`rust` and `rust-cargo` checkers produce explanations.

.. define-key:: C-c ! e
                M-x flycheck-explain-error-at-point

   Display an explanation for the first explainable error at point.


Kill errors
===========

You can put errors into the kill ring with `C-c ! w`:

.. define-key:: C-c ! w
                M-x flycheck-copy-errors-as-kill

   Copy all messages of the errors at point into the kill ring.

.. define-key:: C-u C-c ! w
                C-u M-x flycheck-copy-errors-as-kill

   Like `C-c ! w` but with error IDs.

.. define-key:: M-0 C-c ! w
                M-0 M-x flycheck-copy-errors-as-kill

   Like `C-c ! w` but do not copy the error messages but only the error IDs.
