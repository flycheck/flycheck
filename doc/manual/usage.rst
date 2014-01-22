=======
 Usage
=======

Syntax checking
===============

By default, Flycheck Mode checks syntax automatically when

- the mode is enabled,
- the file is saved,
- new lines are inserted,
- or some short time after the last change to the buffer.

However, you can customize automatic syntax checking with
:el:option:`flycheck-check-syntax-automatically`

.. el:option:: flycheck-check-syntax-automatically

   When Flycheck should check syntax automatically.

   This variable is list of events that may trigger syntax checks.  The
   following events are known:

   `mode-enabled`
     Check syntax immediately when `flycheck-mode` is enabled.
   `save`
     Check syntax immediately after the buffer was saved.
   `new-line`
     Check syntax immediately after a new line was inserted into the buffer.
   `idle-change`
     Check syntax a short time after the last change to the buffer.

   An syntax check is only conducted for events that are contained in this list.
   For instance, the following setting will cause Flycheck to *only* check if
   the mode is enabled or the buffer was saved, but *never* after changes to the
   buffer contents:

   .. code-block:: cl

      (setq flycheck-check-syntax-automatically '(mode-enabled save))

   If the list is empty syntax is never checked automatically.  In this case,
   use :el:command:`flycheck-buffer` to check syntax manually.

.. el:option:: flycheck-idle-change-delay

   How many seconds to wait before starting a syntax check.

   After the buffer was changed, Flycheck waits as many seconds as the value of
   this variable before starting a syntax check.  If the buffer is changed
   during this time, Flycheck starts to wait again.

   This variable has no effect, if `idle-change` is not contained in
   :el:option:`flycheck-check-syntax-automatically`.

Regardless of automatic syntax checking you can also check the buffer
manually:

.. el:command:: flycheck-buffer
   :binding: C-c ! c

   Start a syntax check in the current buffer.

Each syntax check conducts the following steps:

1. Run hooks in :el:hook:`flycheck-before-syntax-check-hook`
2. Clear error information from previous syntax checks.
3. Select a :term:`suitable syntax checker`.  See `Syntax checker selection`_,
   for more information on how syntax checkers are selected.
4. Copy the contents of the buffer to be checked to a temporary file.
5. Run the syntax checker.
6. Parse the output of the tool, and report all errors and warnings.
   See `Error Reporting`_, for more information.
7. If the buffer can be checked with another syntax checker, continue from step
   4, but with the next syntax checker.  This is called “chaining” of syntax
   checkers.
8. Run hooks in :el:hook:`flycheck-after-syntax-check-hook`.

.. el:hook:: flycheck-after-syntax-check-hook

   Functions to run after each syntax check.

   This hook is run after a syntax check was finished.

   At this point, **all** chained checkers were run, and all errors were parsed,
   highlighted and reported.  See `Error reporting`_, for more information about
   error reporting.  The variable :el:variable:`flycheck-current-errors`
   contains all errors from all syntax checkers run during the syntax check, so
   you can use the various error analysis functions.

   Note that this hook does **not** run after each individual syntax checker in
   the syntax checker chain, but only after the **last checker**.

   This variable is a normal hook. @xref{Hooks, , ,elisp}.

.. el:hook:: flycheck-before-syntax-check-hook

   Functions to run before each syntax check.

   This hook is run right before a syntax check starts.

   Error information from the previous syntax check is **not** cleared before
   this hook runs.

   Note that this hook does **not** run before each individual syntax checker in
   the syntax checker chain, but only before the **first checker**.

   This variable is a normal hook. @xref{Hooks, , ,elisp}.

There is also a hook run whenever a syntax check fails:

.. el:hook:: flycheck-syntax-check-failed-hook

   Functions to run if a syntax check failed.

   This hook is run whenever an error occurs during Flycheck's internal
   processing.  No information about the error is given to this hook.

   You should use this hook to conduct additional cleanup actions when Flycheck
   failed.

   This variable is a normal hook. @xref{Hooks, , ,elisp}.

Syntax checker selection
========================

By default Flycheck selects a :term:`suitable syntax checker` automatically from
:el:option:`flycheck-checkers`, with respect to
:el:option:`flycheck-disabled-checkers`:

.. el:option:: flycheck-checkers

   A list of :term:`syntax checker`\ s available for automatic syntax checker
   selection.  An item in this list is a :term:`registered syntax checker`.

   You may modify this list, but you should normally not need to.  Instead, it
   is intended for 3rd party extensions to tell Flycheck about new syntax
   checkers.

.. el:option:: flycheck-disabled-checkers

   A list of :term:`syntax checker`\ s explicitly excluded from automatic
   selection.

   Change this list to disable syntax checkers which you do not want to use.
   You may also use this option as a file or directory local variable to disable
   specific checkers in individual files and directories respectively.

A syntax checker in :el:option:`flycheck-checkers` and **not** in
:el:option:`flycheck-disabled-checkers` is an :term:`enabled syntax checker`.

Flycheck uses the first enabled and suitable syntax checker for the current
buffer.  See `Languages and syntax checkers` for a list of all available syntax
checkers.

If no :term:`suitable syntax checker` is found, the syntax check is *silently*
omitted.  *No* error is signalled.  Only a special indicator in the mode line
informs about the omitted syntax check.  See `Mode line` for details.

You can manually select a specific syntax checker for the current buffer, too:

.. el:command:: flycheck-select-checker
   :binding: C-c ! s

   Prompt for a syntax checker, and select it for the current buffer, by setting
   :el:variable:`flycheck-checker`.

   With prefix arg, deselect the current syntax checker if any, and re-enable
   automatic selection, by setting :el:variable:`flycheck-checker` to `nil`.

   In either case, immediately run a syntax check afterwards.

   Any :term:`syntax checker` can be selected with this command, regardless of
   whether it is enabled.

.. el:variable:: flycheck-checker

   The :term:`syntax checker` to use for the current buffer.

   The variable is buffer local, and safe as file local variable for registered
   checkers.

   If set to `nil`, automatically select a suitable syntax checker.

   If set to a :term:`syntax checker`, only use this syntax checker.  Automatic
   selection as described above is *disabled*.  If the syntax checker not
   suitable, signal an error.

   You may directly set this variable without
   :el:command:`flycheck-select-checker`, e.g. via file local variables.  For
   instance, you can use the following file local variable within a Python
   file to always use :command:`pylint` for the file:

   .. code-block:: python

      # Local Variables:
      # flycheck-checker: python-pylint
      # End:

   @xref{Specifying File Variables, , ,emacs}, for more information about file
   variables.


You can change the completion system used by
:el:command:`flycheck-select-checker`:

.. el:option:: flycheck-completion-system

   The completion system to use.

   `ido`
     Use IDO.

     IDO is a built-in alternative completion system, without good flex matching
     and a powerful UI.  You may want to install flx-ido_ to improve the flex
     matching in IDO.

   `grizzl`
     Use Grizzl_.

     Grizzl is an alternative completion system with powerful flex matching, but
     a very limited UI.

   `nil`
     Use the standard unfancy `completing-read`.

     `completing-read` has a very simple and primitive UI, and does not offer
     flex matching.  This is the default setting, though, to match Emacs'
     defaults.  With this system, you may want enable `icomplete-mode` to
     improve the display of completion candidates at least.

Each syntax checker provides documentation with information about the executable
the syntax checker uses, in which buffers it will be used for syntax checks, and
whether it can be configured.  See `Configuration`, for more information about
syntax checker configuration.

.. el:command:: flycheck-describe-checker
   :binding: C-c ! ?

   Show the documentation of a syntax checker.

.. _flx-ido: https://github.com/lewang/flx
.. _Grizzl: https://github.com/d11wtq/grizzl

Customization
=============

Error reporting
===============

Error navigation
================

Mode line
=========
