.. _flycheck-syntax-checks:

===============
 Check buffers
===============

Flycheck provides two Emacs minor modes for automatic syntax checking:
:mode:`flycheck` to enable syntax checking in the current buffer, and
:mode:`global-flycheck` to enable syntax checking in all buffers whenever
possible.

.. minor-mode:: flycheck-mode

   Enable :ref:`automatic syntax checking <flycheck-automatic-checks>` in the
   current buffer.

.. minor-mode:: global-flycheck-mode

   Enable :mode:`flycheck` in all buffers where syntax checking is possible.

   .. note::

      Remote files (via TRAMP) are checked like local ones: command checkers
      run on the remote host, so the tools need to be installed there rather
      than locally.  Because each check spawns a process over the network,
      remote buffers are checked on fewer triggers by default; see
      `flycheck-check-syntax-automatically-remote`.

      This mode does not enable :mode:`flycheck` in encrypted files, as
      checking them would leak confidential data to temporary files and
      subprocesses.  You can manually enable :mode:`flycheck` in these buffers
      nonetheless, but we do *not* recommend it for said reason.

   Add the following to your :term:`init file` to enable syntax checking
   permanently:

   .. code-block:: elisp

      (add-hook 'after-init-hook #'global-flycheck-mode)

   You can exclude specific major modes from syntax checking with
   `flycheck-global-modes`:

   .. defcustom:: flycheck-global-modes

      Major modes for which :mode:`global-flycheck` turns on :mode:`flycheck`:

      ``t`` (the default)
         Turn :mode:`flycheck` on for all major modes.

      :samp:`({foo-mode} …)`
         Turn :mode:`flycheck` on for all major modes in this list,
         i.e. whenever the value of ``major-mode`` is contained in this list.

      :samp:`(not {foo-mode} …)`
         Turn :mode:`flycheck` on for all major modes *not* in this list,
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

.. _flycheck-automatic-checks:

Check automatically
===================

By default :mode:`flycheck` automatically checks a buffer whenever

* it is enabled,
* the buffer is saved,
* the buffer is reverted (e.g. via ``global-auto-revert-mode``),
* a new line is inserted,
* or a short time after the last change was made in a buffer.

You can customise this behaviour with `flycheck-check-syntax-automatically`:

.. defcustom:: flycheck-check-syntax-automatically

   A list of events which trigger a syntax check in the current buffer:

   ``save``
      Check the buffer immediately after it was saved.

   ``new-line``
      Check the buffer immediately after a new line was inserted.

   ``idle-change``
      Check the buffer a short time after the last change.  The delay is
      customisable with `flycheck-idle-change-delay`:

      .. defcustom:: flycheck-idle-change-delay

         Seconds to wait after the last change to the buffer before starting a
         syntax check.

   ``idle-buffer-switch``
     Check the buffer a short time after switching to it from another
     buffer.  The delay is customisable with
     `flycheck-idle-buffer-switch-delay`:

     .. defcustom:: flycheck-idle-buffer-switch-delay

        Seconds to wait after switching to a buffer before starting a
        syntax check.

      If you switch to several buffers in rapid succession, the
      behavior depends on
      `flycheck-buffer-switch-check-intermediate-buffers`:

      .. defcustom:: flycheck-buffer-switch-check-intermediate-buffers

         If non-nil, then a buffer you switch to will have a syntax
         check run even if you switch to another buffer before it
         starts.  If nil, then only the current buffer can have a
         syntax check run.  Note that syntax checks can still be run
         in other buffers due to changes to their contents.

   ``mode-enabled``
      Check the buffer immediately after :mode:`flycheck` was enabled.

   For instance with the following setting :mode:`flycheck` will only check the
   buffer when it was saved:

   .. code-block:: elisp

      (setq flycheck-check-syntax-automatically '(mode-enabled save))

.. defcustom:: flycheck-check-syntax-automatically-remote

   The same as `flycheck-check-syntax-automatically`, but for buffers visiting
   remote files (see `file-remote-p`).  Checking a remote buffer spawns a
   process on the remote host over TRAMP, which is slow, so by default the
   change-driven triggers (``idle-change``, ``new-line`` and
   ``idle-buffer-switch``) are excluded and remote buffers are only checked on
   ``save`` and ``mode-enabled``.

   Set it to ``t`` to check remote buffers on the same events as local ones.
   A manual check with :command:`flycheck-buffer` (:kbd:`C-c ! c`) always
   works regardless of this option.

When a change-driven syntax check (``idle-change`` or ``save``) is triggered
while a recently started check is still running, the running check is
interrupted and the new one starts immediately, since its results would no
longer match the buffer.  Checks triggered on every keystroke (the
``new-line`` condition) or by buffer switches coalesce behind the running
check instead, and syntax checkers that cannot be interrupted or that have
already made substantial progress are left to finish:

.. defcustom:: flycheck-interrupt-running-checks

   Whether a new syntax check interrupts a running one.  With the default
   value of ``10``, only running checks younger than ten seconds are
   interrupted; checks that have made more progress are left to complete, and
   the new check is deferred until they finish, so slow syntax checkers still
   publish their results.  Set to ``t`` to always interrupt, or ``nil`` to
   never interrupt and always defer, like older Flycheck versions did.
   Setting ``nil`` file- or directory-locally is handy for projects whose
   syntax checkers you never want interrupted.

.. _flycheck-manual-checks:

Check manually
==============

You can also start a syntax check explicitly with `C-c ! c`:

.. define-key:: C-c ! c
                M-x flycheck-buffer

   Check syntax in the current buffer.
