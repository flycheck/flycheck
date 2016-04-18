===============
 Syntax checks
===============

This document explains how Flycheck checks buffers.  It documents the minor
modes that Flycheck provides, shows how syntax checks work and how you can
influence what syntax checkers Flycheck selects, and illustrates the
customization options available for syntax checkers.

Syntax checks in buffers
========================

Flycheck provides two Emacs minor modes for automatic syntax checking:
:mode:`flycheck` to enable syntax checking in the current buffer, and
:mode:`global-flycheck` to enable syntax checking in all buffers whenever
possible.

.. minor-mode:: flycheck-mode

   Enable automatic syntax checking in the current buffer.

   By default :mode:`flycheck` checks a buffer whenever

   * it is enabled,
   * the buffer is saved,
   * a new line is inserted,
   * or a short time after the last change was made in a buffer.

   You can customise this behaviour with `flycheck-check-syntax-automatically`.

.. minor-mode:: global-flycheck-mode

   Enable :mode:`flycheck` in all buffers where syntax checking is possible.

   .. note::

      This mode does not enable :mode:`flycheck` in remote files (via
      TRAMP) and encrypted files.  Checking remote files may be very slow
      depending on the network connections, and checking encrypted files would
      leak confidential data to temporary files and subprocesses.

      You can manually enable :mode:`flycheck` in these buffers nonetheless, but
      we do *not* recommend this for said reasons.

   Add the following to your :term:`init file` to enable syntax checking
   permanently:

   .. code-block:: elisp

      (add-hook 'after-init-hook #'global-flycheck-mode)

   You can exclude specific major modes from syntax checking with
   `flycheck-global-modes`:

   .. option:: flycheck-global-modes

      Major modes for which :mode:`global-flycheck` turns on :mode:`flycheck`:

      ``t`` (the default)
         Turn :mode:`flycheck` on for all major modes.

      :samp:`({foo-mode} …)`
         Turn :mode:`flycheck` on for all major modes in this list,
         i.e. whenever the value of ``major-mode`` is contained in this list.

      :samp:`(not {foo-mode} …)`
         Turn :mode:`flycheck` on for all major nodes *not* in this list,
         i.e. whenever the value of ``major-mode`` is *not* contained in this
         list.

      .. note::

         :mode:`global-flycheck` never turns on :mode:`flycheck` in major modes
         whose ``mode-class`` property is ``special``, regardless of the value
         of this option.  Syntax checking simply makes no sense in special
         buffers which are typically intended for non-interactive display rather
         than editing.

      .. seealso::

         :infonode:`(elisp)Major Mode Conventions`
            Information about major modes, and modes marked as special.

.. option:: flycheck-check-syntax-automatically

   A list of events which trigger a syntax check in the current buffer:

   ``save``
      Check the buffer immediately after it was saved.

   ``new-line``
      Check the buffer immediately after a new line was inserted.

   ``idle-change``
      Check the buffer a short time after the last change.  The delay is
      customisable with `flycheck-idle-change-delay`:

      .. option:: flycheck-idle-change-delay

         Seconds to wait after the last change to the buffer before starting a
         syntax check.

   ``mode-enabled``
      Check the buffer immediately after :mode:`flycheck` was enabled.

   For instance with the following setting :mode:`flycheck` will only check the
   buffer when it was saved:

   .. code-block:: elisp

      (setq flycheck-check-syntax-automatically '(mode-enabled save))

You can also start a syntax check explicitly with `C-c ! c`:

.. command:: C-c ! c
             M-x flycheck-buffer

   Check syntax in the current buffer.

To make sure that syntax checking works correctly verify your setup:

.. command:: C-c ! v
             M-x flycheck-verify-setup

   Show a buffer with information about your :mode:`flycheck` setup for the
   current buffer.

   Lists all syntax checkers available for the current buffer, and potential
   issues with their setup.

Syntax checkers
===============

Flycheck does not check buffers on its own.  Instead it delegates this task to
external *syntax checkers* which are external programs or services that receive
the contents of the current buffer and return a list of errors in the buffer,
together with metadata that tells Flycheck how to run the program, how to pass
buffer contents to it, and how to extract errors.

.. seealso::

   :ref:`flycheck-languages`
      A complete list of all syntax checkers included in Flycheck

Like everything else in Emacs syntax checkers have online documentation which
you can access with `C-c ! ?`:

.. command:: C-c ! ?
             M-x flycheck-describe-checker

   Prompt for the name of a syntax checker and pop up a Help buffer with its
   documentation.

   The documentation includes the name of the program or service used, a list of
   major modes the checker supports and a list of all options for this syntax
   checker.

Automatic syntax checker selection
----------------------------------

Normally Flycheck automatically selects the best syntax checkers for the current
buffer from `flycheck-checkers` whenever it needs to check the buffer:

.. option:: flycheck-checkers

   A list of all syntax checkers available for syntax checking.

   A syntax checker in this list is a :term:`registered syntax checker`.

Flycheck picks the first syntax checker from this list which exists and supports
the current major mode, and runs it over the current buffer.  When the checker
has finished Flycheck whether it asks for a next syntax checker to run, and if
so, runs the next syntax checker, and so on, until there is no more syntax
checker for the current buffer.  This process repeats whenever Flycheck needs to
check the buffer according to `flycheck-check-syntax-automatically`.

For instance, the first syntax checker for Emacs Lisp is `emacs-lisp` which
checks Emacs Lisp with Emacs' own byte compiler.  This syntax checker asks for
`emacs-lisp-checkdoc` to run next, which checks for stylistic issues in Emacs
Lisp docstrings.  Thus Flycheck will first run the byte compiler and then
checkdoc in an Emacs Lisp buffer.

Manual syntax checker selection
-------------------------------

Alternatively you can tell Flycheck explicitly which syntax checker to start
with in the current buffer:

.. command:: C-c ! s
             M-x flycheck-select-checker

   Prompt for a syntax checker and use this syntax checker as the first syntax
   checker for the current buffer.

   Flycheck may still run further syntax checkers from `flycheck-checkers` if
   the selected syntax checker asks for it.

Flycheck will use the selected syntax checker as “entry point” for syntax checks
in the current buffer, just as if it had selected this syntax checker
automatically.  It will automatically run further syntax checkers from
`flycheck-checkers` if the selected syntax checker asks for it.

Under the hood `C-c ! s` sets `flycheck-checker`:

.. variable:: flycheck-checker

   The name of a syntax checker to use for the current buffer.

   If ``nil`` (the default) let Flycheck automatically select the best syntax
   checker from `flycheck-checkers`.

   If set to a syntax checker Flycheck will use this syntax checker as the first
   one in the current buffer, and run subsequent syntax checkers just as if it
   had selected this one automatically.

   If the syntax checker in this variable does not work in the current buffer
   signal an error.

   This variable is buffer-local.

We recommend to set `flycheck-checker` via :infonode:`(emacs)Directory
Variables` to enforce a specific syntax checker for a project.  For instance,
Flycheck usually prefers `javascript-eslint` for Javascript buffers, but if your
project uses `javascript-jshint` instead you can tell Flycheck to use
`javascript-jshint` for all Javascript buffers of your project with the
following command in the top-level directory of your project: :kbd:`M-x
add-dir-local-variable RET flycheck-checker RET javascript-jshint`.  A new
buffer pops up that shows the newly created entry in the directory variables.
Save this buffer and kill it.  From now on Flycheck will check all Javascript
files of this project with JSHint.

To go back to automatic selection either set `flycheck-checker` to ``nil`` or
type `C-u C-c ! s`:

.. command:: C-u C-c ! s
             C-u M-x flycheck-select-checker

   Remove any selected syntax checker and let Flycheck again select a syntax
   checker automatically.

Disabled syntax checkers
------------------------

.. todo:: Document disabled syntax checker

Customisation of syntax checkers
--------------------------------

.. todo:: Document options of syntax checkers
