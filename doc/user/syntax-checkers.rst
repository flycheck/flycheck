=================
 Syntax checkers
=================

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

.. define-key:: C-c ! ?
                M-x flycheck-describe-checker

   Prompt for the name of a syntax checker and pop up a Help buffer with its
   documentation.

   The documentation includes the name of the program or service used, a list of
   major modes the checker supports and a list of all options for this syntax
   checker.

.. _flycheck-automatic-selection:

Select syntax checkers automatically
====================================

Normally Flycheck automatically selects the best syntax checkers for the current
buffer from `flycheck-checkers` whenever it needs to check the buffer:

.. defcustom:: flycheck-checkers

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

.. _flycheck-manual-selection:

Select syntax checkers manually
===============================

Alternatively you can tell Flycheck explicitly which syntax checker to start
with in the current buffer:

.. define-key:: C-c ! s
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

.. defvar:: flycheck-checker

   The name of a syntax checker to use for the current buffer.

   If ``nil`` (the default) let Flycheck :ref:`automatically select
   <flycheck-automatic-selection>` the best syntax checker from
   `flycheck-checkers`.

   If set to a syntax checker Flycheck will use this syntax checker as the first
   one in the current buffer, and run subsequent syntax checkers just as if it
   had selected this one automatically.

   If the syntax checker in this variable does not work in the current buffer
   signal an error.

   This variable is buffer-local.

We recommend to set `flycheck-checker` via directory local variables to enforce
a specific syntax checker for a project.  For instance, Flycheck usually prefers
`javascript-eslint` for Javascript buffers, but if your project uses
`javascript-jshint` instead you can tell Flycheck to use `javascript-jshint` for
all Javascript buffers of your project with the following command in the
top-level directory of your project: :kbd:`M-x add-dir-local-variable RET
js-mode RET flycheck-checker RET javascript-jshint`.  A new buffer pops up that
shows the newly created entry in the directory variables.  Save this buffer and
kill it.  From now on Flycheck will check all Javascript files of this project
with JSHint.

.. seealso::

   :infonode:`(emacs)Locals`
      General information about local variables.

   :infonode:`(emacs)Directory Variables`
      Information about directory variables.

To go back to automatic selection either set `flycheck-checker` to ``nil`` or
type `C-u C-c ! s`:

.. define-key:: C-u C-c ! s
                C-u M-x flycheck-select-checker

   Remove any selected syntax checker and let Flycheck again :ref:`select a
   syntax checker automatically <flycheck-automatic-selection>`.

.. _flycheck-disable-checkers:

Disable syntax checkers
=======================

Even if you :ref:`select a checker manually <flycheck-manual-selection>`
Flycheck may still use a syntax checker that you’d not like to use.  To
completely opt out from a specific syntax checker disable it:

.. define-key:: C-c ! x
                M-x flycheck-disable-checker

   Prompt for a syntax checker to disable in the current buffer.

For instance if you do not care for documentation conventions of Emacs Lisp you
can opt out from `emacs-lisp-checkdoc` which checks your code against these
conventions with :kbd:`C-c ! x emacs-lisp-checkdoc`.  After the next check all
checkdoc warnings will be gone from the buffer.

Internally this command changes the buffer-local `flycheck-disabled-checkers`:

.. defcustom:: flycheck-disabled-checkers

   A list of disabled syntax checkers.  Flycheck will *never* use disabled
   syntax checkers to check a buffer.

   This option is buffer-local.  You can customise this variable with :kbd:`M-x
   customize-variable RET flycheck-disabled-checkers` or set the default value
   in your :term:`init file` to permanently disable specific syntax checkers.
   For instance:

   .. code-block:: elisp

      (setq-default flycheck-disabled-checkers '(c/c++-clang))

   will permanently disable `c/c++-clang` in all buffers.

You can also disable syntax checkers per project with directory local variables.
For instance type :kbd:`M-x add-dir-local-variable RET emacs-lisp-mode RET
flycheck-disabled-checkers RET emacs-lisp-checkdoc` in your :term:`user emacs
directory` to disable `emacs-lisp-checkdoc` for all Emacs Lisp files in your
personal configuration.

.. seealso::

   :infonode:`(emacs)Locals`
      General information about local variables.

   :infonode:`(emacs)Directory Variables`
      Information about directory variables.

To enable a disabled checker again, remove it from `flycheck-disabled-checkers`
or use `C-u C-c ! x`:

.. define-key:: C-u C-c ! x
                C-u M-x flycheck-disable-checker

   Prompt for a disabled syntax checker to enable again in the current buffer.
