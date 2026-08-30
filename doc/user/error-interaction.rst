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
   grep``, etc. still take *precedence* over Flycheck’s errors.

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

   With prefix argument, jump forwards by as many errors as specified by the
   prefix argument, e.g. :kbd:`M-3 M-x flycheck-first-error` moves to the 3rd
   error from the beginning of the buffer.  With negative prefix argument move
   to the last error instead.

Navigation stays within the current buffer by default, but it can follow the
project's diagnostics across files:

.. defcustom:: flycheck-navigation-scope

   The scope of error navigation: ``buffer`` (the default) keeps
   `flycheck-next-error` and `flycheck-previous-error` within the current
   buffer; ``project`` continues by a single step into the project's other
   diagnostics when the buffer runs out, opening the next file at its first
   error.

.. image:: /images/flycheck-project-navigation.gif

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

You can also disable Flycheck from automatically displaying errors at point by
customising:

.. defcustom:: flycheck-auto-display-errors-after-checking

   Whether to automatically display errors at the current point after checking.

   When being set to `nil`, it will prevent Flycheck from automatically displaying
   error messages. This setting is useful when Flycheck is used together with
   `flycheck-posframe`, to prevent `flycheck-posframe` from repeatedly displaying
   errors at point.

By default Flycheck documents the errors at point through Eldoc, alongside
other Eldoc sources such as Eglot.  This honors your Eldoc customizations,
e.g. ``eldoc-echo-area-use-multiline-p`` or alternative Eldoc frontends like
``eldoc-box``.  The behaviour is entirely customisable:

.. defcustom:: flycheck-display-errors-function

   A function to display errors.

   The function is given the list of Flycheck errors to display as sole argument
   and shall display these errors to the user in some way.

Flycheck provides three built-in functions for this option:

.. defun:: flycheck-display-errors-via-eldoc errors

   The default.  Errors at point are shown through Eldoc.  Messages too
   long for the echo area (see ``eldoc-echo-area-use-multiline-p``) can be
   read in full with :kbd:`M-x eldoc-doc-buffer`.

.. defun:: flycheck-display-error-messages errors
           flycheck-display-error-messages-unless-error-list errors

   Show error messages and IDs in the echo area or in a separate buffer if the
   echo area is too small (using `display-message-or-buffer` which see).  This
   was the default before Flycheck 37.  The latter only displays errors when
   the :ref:`error list <flycheck-error-list>` is not visible.  To enable it
   add the following to your :term:`init file`:

   .. code-block:: elisp

      (setq flycheck-display-errors-function
            #'flycheck-display-error-messages-unless-error-list)

Along the same vein, Flycheck provides a way to customise how an error message is
cleared, this is especially useful if you use a Flycheck extension to display
error messages differently from the default.

.. defcustom:: flycheck-clear-displayed-errors-function

   Function to hide error message displayed by `flycheck-display-errors-function`.

   If set to a function, it will be called with no arguments to
   clear all displayed errors at point.

By default, Flycheck only provides `flycheck-clear-displayed-error-messages` to
clear the last Flycheck error message from the minibuffer:

.. defun:: flycheck-clear-displayed-error-messages

   Clear error messages displayed by `flycheck-display-error-messages`.

.. seealso::

   :mode:`flycheck-annotate`
      Show the error messages next to the code they refer to, instead of in
      the echo area.

Additionally Flycheck shows errors in a GUI tooltip whenever you hover an error
location with the mouse pointer.  By default the tooltip contains the messages
and IDs of all errors under the pointer, but the contents are customisable:

.. defcustom:: flycheck-help-echo-function

   A function to create the contents of the tooltip.

   The function is given a list of Flycheck errors to display as sole argument
   and shall return a single string to use as the contents of the tooltip.

Errors from other files
=======================

Some checkers may return errors from files other than the current buffer (e.g.,
`gcc` may complain about errors in included files).  These errors appear in the
error list, and are also added on the first line of the current buffer.  You can
jump to the incriminating files with `flycheck-previous-error`.

By default, warnings and info messages from other files are ignored, but you can
customize the minimum level:

.. defcustom:: flycheck-relevant-error-other-file-minimum-level

   The minimum level errors from other files to consider for inclusion in the
   current buffer.

   If set to an error level, only display errors from other files whose error
   level is at least as severe as this one.  If ``nil``, display all errors from
   other files.

To never show any errors from other files, set
`flycheck-relevant-error-other-file-show` to ``nil``.

.. defcustom:: flycheck-relevant-error-other-file-show

   Whether to show errors from other files.

Explain errors
==============

Flycheck also has the ability to display explanations for errors, provided the
error checker is capable of producing these explanations.  While several
checkers produce explanations, the majority do not.  Those that do are:

* `css-stylelint`
* `javascript-eslint`
* `markdown-markdownlint-cli`
* `markdown-markdownlint-cli2`
* `perl-perlcritic`
* `python-pylint`
* `python-ruff`
* `rpm-rpmlint`
* `rust`
* `rust-cargo`
* `rust-clippy`
* `sh-shellcheck`

.. define-key:: C-c ! e
                M-x flycheck-explain-error-at-point

   Display an explanation for the first explainable error at point.


Diagnostic tags
===============

Some errors say something about the code beyond how serious they are.  A
language server marks an unused import or an unreachable branch as
*unnecessary*, and an API you should stop calling as *deprecated*, through an
LSP diagnostic's ``tags``.  Flycheck keeps them with
:mode:`flycheck-eglot` and with the native ``flycheck-lsp`` checker.

A tag is not a level.  An unused import is still a warning, so its tag adds a
face rather than replacing the one the level gives it: unnecessary code is
dimmed and deprecated code struck through, on top of the usual underline.
This matches how Eglot renders the same tags under Flymake.

.. defface:: flycheck-unnecessary

   Face added to code an error marks as having no effect.

.. defface:: flycheck-deprecated

   Face added to code an error marks as deprecated.

Checkers that are not LSP-backed can set tags too, through an error's
``tags`` slot.

Related locations
=================

Many errors point at more than one place.  A "redefined variable" error
refers back to the first definition, a Rust lifetime error to the borrow that
outlives it, and so on.  Language servers report these secondary locations as
an LSP diagnostic's ``relatedInformation``; with :mode:`flycheck-eglot`
Flycheck keeps them (Flymake, which shows Eglot's diagnostics, drops them).
Other checkers can attach them too, through an error's ``relations`` slot.

.. image:: /images/flycheck-related-locations.png

Flycheck shows an error's related locations right after its message - in the
echo area, through Eldoc, in the help-echo tooltip, and, with
:mode:`flycheck-annotate`, on their own lines below the code.  Each is a
``↳`` line naming the place and its message.  In the :ref:`error list
<flycheck-error-list>` an error that carries related locations is badged with
``↳N`` (how many), listed in the badge's tooltip.

.. define-key:: C-c ! j
                M-x flycheck-visit-related-location

   Visit a related location of an error at point.  With one, jump straight to
   it; with several, prompt for which.  Visiting another file opens it, and the
   jump goes on the ``xref`` marker stack, so :kbd:`M-x xref-go-back` returns.
   Signals an error when no error at point has a related location.

Afterwards, step through the rest without prompting:

.. define-key:: M-x flycheck-next-related-location
                M-x flycheck-previous-related-location

   Visit the next (or previous) related location, cycling.  With
   ``repeat-mode`` on, :kbd:`n` and :kbd:`p` keep walking.  With a prefix
   argument, move that many locations.

.. image:: /images/flycheck-related-locations.gif

In the :ref:`error list <flycheck-error-list>` press :kbd:`j`
(``flycheck-error-list-visit-related-location``) to visit a related location
of the error on the current row; it opens in another window so the list stays
put.


Fix errors
==========

Some checkers report a machine-applicable fix along with an error - the
replacement text a tool like ``eslint --fix``, ``cargo clippy`` or a SARIF
analyzer already computes.  Flycheck keeps that fix and can apply it for you.
A fixable error gets a distinct fringe indicator, its ``[fix]`` marker in the
error list, and, under :mode:`flycheck-annotate`, a ``[fix]`` tag before
its inline message - like an editor's "fix available" lightbulb.

A language server's fixes work differently: its ``quickfix`` code action is
only fetched when you apply it, so until then Flycheck cannot tell whether
there is one.  Those errors get no fringe indicator, since it would promise a
fix that may not exist, but the error list marks them ``[fix?]``.  Press
:kbd:`C-c ! f` on one to ask the server.

.. image:: /images/flycheck-quick-fix.png

.. define-key:: C-c ! f
                M-x flycheck-fix-error-at-point

   Apply the fix of the first fixable error at point.  Signals an error if no
   error at point has a fix.

.. define-key:: C-c ! F
                M-x flycheck-fix-all-errors

   Apply every machine-applicable fix in the current buffer at once, as a
   single undoable change.  Fixes that would conflict with each other are
   skipped; the echo area reports how many were applied.

.. image:: /images/flycheck-fix-all.gif

In the :ref:`error list <flycheck-error-list>` press :kbd:`x`
(``flycheck-error-list-apply-fix``) to apply the fix of the error on the
current row, or :kbd:`X` (``flycheck-error-list-fix-all``) to apply every
fix in the list's source buffer.

:kbd:`x` works across files in the list's project scope (:kbd:`P`, see
:doc:`error-list`).  A project checker such as ``cargo-check`` reports fixable
errors for files no buffer visits; applying one opens that file, makes the
change and leaves the buffer unsaved, so you can look it over and undo it.
A file that has been written since the check is left alone rather than edited
at positions that no longer describe it, and so is one whose buffer has
unsaved changes.

A fix is applied as a single undoable change, so :kbd:`C-/` reverts it.  The
checkers that currently provide fixes are ``javascript-eslint``,
``python-ruff``, ``sh-shellcheck``, the ``rust`` checkers (from ``cargo
clippy``'s machine-applicable suggestions), and any checker whose SARIF output
includes fixes (e.g. ``dockerfile-hadolint``).  LSP servers that offer
``quickfix`` code actions provide fixes too, through ``flycheck-eglot-mode`` or
the native ``flycheck-lsp`` checker (see :doc:`syntax-checks`).


Kill errors
===========

You can put errors into the kill ring with `C-c ! C-w`:

.. define-key:: C-c ! C-w
                M-x flycheck-copy-errors-as-kill

   Copy all messages of the errors at point into the kill ring.

.. define-key:: C-u C-c ! C-w
                C-u M-x flycheck-copy-errors-as-kill

   Like `C-c ! C-w` but with error IDs.

.. define-key:: M-0 C-c ! C-w
                M-0 M-x flycheck-copy-errors-as-kill

   Like `C-c ! C-w` but do not copy the error messages but only the error IDs.
