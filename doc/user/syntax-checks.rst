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


LSP diagnostics via Eglot
=========================

Eglot, Emacs' built-in LSP client, renders its diagnostics through Flymake and
provides no Flycheck backend of its own.  ``flycheck-eglot-mode`` bridges the
gap, so a buffer can use Eglot for LSP features while Flycheck owns the error
display, navigation and the error list:

.. code-block:: elisp

   (global-flycheck-eglot-mode 1)

.. minor-mode:: flycheck-eglot-mode

   Report the diagnostics Eglot receives from the LSP server through Flycheck,
   via the ``eglot-check`` checker, and turn Flymake off in the buffer.  Only
   active in Eglot-managed buffers.

.. minor-mode:: global-flycheck-eglot-mode

   Enable ``flycheck-eglot-mode`` in every Eglot-managed buffer.  This is the
   usual way to turn the integration on.

.. defcustom:: flycheck-eglot-exclusive

   When non-nil (the default), an Eglot buffer reports only the LSP server's
   diagnostics.  Set to nil to have ``eglot-check`` chain to the first command
   checker that supports the buffer's major mode, so an LSP server and a
   command checker both contribute.

When the server supports code actions, an Eglot diagnostic's ``quickfix``
action is offered as a Flycheck fix: press ``C-c ! f`` on the error to apply it
(or ``C-c ! F`` to apply all).  The action is fetched from the server only when
you apply a fix, so every diagnostic is shown as potentially fixable even if the
server has nothing for it.

.. defcustom:: flycheck-eglot-code-actions

   When non-nil (the default), offer LSP ``quickfix`` code actions as fixes, as
   described above.  Set to nil to turn this off (and stop marking every
   diagnostic as fixable).

This obsoletes the third-party ``flycheck-eglot`` package; drop it from your
configuration if you used it.

.. warning::

   The built-in mode uses the same names -- ``flycheck-eglot-mode`` and
   ``global-flycheck-eglot-mode`` -- as the old package.  If both are
   installed they define the same commands, so **uninstall the third-party
   ``flycheck-eglot`` package** before relying on the built-in one.


LSP diagnostics without Eglot
=============================

A growing number of linters ship their own LSP server - RuboCop
(``rubocop --lsp``), Ruff (``ruff server``), Biome (``biome lsp-proxy``) and
more.  ``flycheck-lsp-mode`` talks to one of these directly, over Emacs'
built-in ``jsonrpc`` library, and reports its diagnostics through the ``flycheck-lsp``
checker - no Eglot, and no full LSP client, involved.

This is the setup the native LSP support is built for, and it is worth
preferring when a linter offers it.  A traditional command checker spawns the
linter afresh on every check, and for a tool with a slow start (RuboCop
loading your project, say) that startup cost is paid over and over.  The
linter's own LSP server instead stays resident and lints incrementally, so
checks are much snappier - and you get that without pulling in a full language
server and client just to see diagnostics.

Turn it on everywhere with:

.. code-block:: elisp

   (global-flycheck-lsp-mode 1)

Out of the box this covers RuboCop (Ruby), Ruff (Python), Biome (JavaScript,
TypeScript, JSON, CSS) and Harper (Markdown prose) - each lints with sensible
defaults, no project configuration needed.  A server is only used when its
program is installed, so enabling the mode globally does nothing in a buffer
whose language server you don't have.

.. minor-mode:: flycheck-lsp-mode

   Start the LSP server configured for the buffer's major mode, send it the
   buffer's text, and report the diagnostics it pushes back through the ``flycheck-lsp``
   checker.  Does nothing in a buffer whose major mode has no configured,
   installed server.

.. minor-mode:: global-flycheck-lsp-mode

   Enable ``flycheck-lsp-mode`` in every buffer whose major mode has an
   installed server in `flycheck-lsp-servers`.

Which LSP integration should I use?
-----------------------------------

Flycheck can get LSP diagnostics three ways.  Pick by how much of a language
server you actually want:

.. list-table::
   :header-rows: 1
   :widths: 24 25 25 26

   * -
     - ``flycheck-lsp-mode``
     - ``flycheck-eglot-mode``
     - ``lsp-mode``
   * - Runs the server
     - Flycheck, over built-in ``jsonrpc``
     - Eglot
     - ``lsp-mode``
   * - Extra dependency
     - none
     - Eglot (built in since Emacs 29)
     - the external ``lsp-mode`` package
   * - Provides
     - diagnostics + quickfix code actions
     - full LSP (hover, completion, rename, ...)
     - full LSP
   * - Owns the buffer
     - no - runs like any checker, can chain to a command checker
     - yes
     - yes
   * - Best for
     - single-purpose linters as a Flycheck checker
     - a real language server you also edit with
     - if you already run ``lsp-mode``

In short: reach for ``flycheck-lsp-mode`` when you just want a linter that
happens to speak LSP, and for ``flycheck-eglot-mode`` (see above) when you want
a full language server whose diagnostics Flycheck should own.

Configuring servers
-------------------

.. defcustom:: flycheck-lsp-servers

   An alist mapping a major mode to the server command, as ``(MAJOR-MODE
   PROGRAM ARG...)``.  The defaults are:

   ==============================  ========  ========================
   Language / mode                 Server    Command
   ==============================  ========  ========================
   Ruby                            RuboCop   ``rubocop --lsp``
   Python                          Ruff      ``ruff server``
   JavaScript/TypeScript/JSON/CSS  Biome     ``biome lsp-proxy``
   Markdown (prose)                Harper    ``harper-ls --stdio``
   ==============================  ========  ========================

   These lint out of the box.  Other linters ship an LSP server too but need
   project configuration before they report anything - TFLint wants a
   ``.tflint.hcl`` and plugins, Buf wants a module, Taplo wants a workspace
   config it fetches over ``workspace/configuration`` (which the ``flycheck-lsp`` checker
   does not answer) - so they are left out of the defaults.  Add them once your
   project is set up::

      (add-to-list 'flycheck-lsp-servers '(terraform-mode "tflint" "--langserver"))
      (add-to-list 'flycheck-lsp-servers '(protobuf-mode "buf" "lsp" "serve"))
      (add-to-list 'flycheck-lsp-servers '(toml-mode "taplo" "lsp" "stdio"))

   A major mode maps to a single server.  To use a different one, replace the
   entry - e.g. Standard instead of RuboCop, or Oxlint instead of Biome::

      (setf (alist-get 'ruby-mode flycheck-lsp-servers) '("standardrb" "--lsp"))
      (setf (alist-get 'js-ts-mode flycheck-lsp-servers) '("oxlint" "--lsp"))

   To drop a default, remove it::

      (setq flycheck-lsp-servers
            (assq-delete-all 'css-mode flycheck-lsp-servers))

   The server must speak LSP over stdio and publish diagnostics; a full
   language server works too, but for one of those you usually want Eglot.  An
   entry whose PROGRAM is not on `exec-path` is ignored, so listing a server
   you have not installed is harmless.  Flycheck starts one process per project
   root and command, feeds it the buffer's unsaved text, and shuts it down when
   Emacs exits.

.. defcustom:: flycheck-lsp-exclusive

   When non-nil (the default), a buffer reports only the language server's
   diagnostics.  Set to nil to have ``flycheck-lsp`` chain to the first command checker
   that supports the buffer's major mode, so the server and a command checker
   both contribute.

.. defcustom:: flycheck-lsp-code-actions

   When non-nil (the default), offer the server's ``quickfix`` code actions as
   Flycheck fixes: ``C-c ! f`` applies the fix of the error at point, ``C-c !
   F`` applies all, and the error list marks fixable errors with ``[fix]``.
   Set to nil to turn this off.

   Flycheck gets these actions two ways.  Some servers (RuboCop, standardrb)
   ship each diagnostic's autocorrect inline, in the diagnostic's data;
   Flycheck uses it directly, for free.  Others (Ruff, Biome, Oxlint) answer a
   ``textDocument/codeAction`` request, which Flycheck sends only when you
   apply a fix - so with those, a diagnostic is shown as potentially fixable
   even when the server turns out to have nothing for it.

.. defcustom:: flycheck-lsp-initialize-timeout

   How many seconds to wait for a server to answer the ``initialize``
   handshake.  The handshake runs asynchronously and does not block Emacs: the
   first check of a buffer whose server is still starting reports no
   diagnostics, and the buffer is rechecked once the handshake finishes.  A
   server that does not answer in time is torn down and retried later.
   Defaults to 5.

Diagnostics from the ``flycheck-lsp`` checker carry the same detail as Eglot's - error
level, id, an explanation URL from ``codeDescription``, and the secondary
locations of ``relatedInformation`` (visit them with ``C-c ! j``).  When the
server offers code actions, they are available as fixes as well (see
``flycheck-lsp-code-actions`` above).

Full language servers
---------------------

The ``flycheck-lsp`` checker consumes only diagnostics, so nothing stops you pointing it
at a *full* language server - clojure-lsp, ruby-lsp, gopls and the like - and
letting Flycheck surface its diagnostics::

   (add-to-list 'flycheck-lsp-servers '(clojure-mode "clojure-lsp"))

This can be handy: many full servers bundle a linter (clojure-lsp runs
clj-kondo, ruby-lsp runs RuboCop), so this is one way to get that linting into
Flycheck without running a full LSP client at all.

But it goes against the grain of what this feature is for.  ``flycheck-lsp-mode``
is aimed at linters that ship their own LSP server, precisely so you can lint
without either the repeated startup cost of a command checker or the weight of a
full server-plus-client stack.  A full language server is the opposite trade:
you start a process that indexes the whole project to provide completion, hover,
rename and much more, and Flycheck throws all of that away and keeps only the
errors.  If you want those features too, run the server through Eglot and use
``flycheck-eglot-mode``; if you only want diagnostics, a dedicated linter is
lighter than a language server.

Two caveats if you do point it at a full server: some report diagnostics only
through LSP *pull* requests rather than pushing them, which the ``flycheck-lsp`` checker
does not yet support (it will report nothing), and some need extra
``initializationOptions`` or workspace configuration to lint the way you expect.
