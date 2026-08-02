.. _flycheck-troubleshooting:

=================
 Troubleshooting
=================

If syntax checking does not work as expected there are a number of steps that
you can follow to isolate and maybe fix the problem.

.. _flycheck-common-issues:

Common issues
=============

First check whether your issue is one of the common setup issues and problems.

.. _flycheck-macos-exec-path-from-shell:

Flycheck can’t find any programs in GUI Emacs on macOS
------------------------------------------------------

Try to install and configure exec-path-from-shell_ to make a GUI Emacs inherit
the ``$PATH`` environment variable from your shell configuration.

The issue is that due to the special way macOS starts GUI programs a GUI Emacs
does not inherit the environment variables from the shell configuration so Emacs
will lack some important entries in ``$PATH``, most notably ``/usr/local/bin/``
where Homebrew, NPM and many other package managers put binaries in.

The `exec-path-from-shell`_ works around this issue by extracting environment
variables from a shell session and inject them into the environment of the
running Emacs instance.

.. _exec-path-from-shell: https://github.com/purcell/exec-path-from-shell

Flycheck says it “cannot read” a syntax checker’s output
--------------------------------------------------------

The message looks like this:

.. code-block:: text

   Flycheck: cannot read python-flake8's output, so it may be misconfigured;
   C-c ! v shows what it printed

It means the tool ran, exited with a failure, and printed nothing Flycheck could
turn into errors.  Press :kbd:`C-c ! v`: the verification buffer leads with
whatever the checker actually printed, which is nearly always the answer.

By far the most common cause is the tool being missing from, or broken in, the
environment Emacs is using.  A ``python-flake8`` pointed at an interpreter
without flake8’s dependencies prints a Python traceback ending in
``ModuleNotFoundError``; a linter whose plugin failed to load says so.  Neither
is a Flycheck problem.  On macOS this is often the ``$PATH`` problem described
:ref:`above <flycheck-macos-exec-path-from-shell>`, and if you use direnv or
virtualenvs, check that the tool Emacs finds is the project’s and not a global
one.

The next most common cause is a version mismatch: newer Flycheck releases may
pass flags older tools do not understand, or parse an output format they do not
produce yet.  Update the tool before anything else.

If the tool is current and its output looks like something Flycheck ought to
have understood, that is a flaw in the checker definition, so please
:ref:`report a bug <flycheck-bug-reports>` and include what the verification
buffer showed.

A syntax checker disabled itself
--------------------------------

.. code-block:: text

   Flycheck: python-flake8 disabled itself in this buffer
   (ModuleNotFoundError: No module named 'pycodestyle'); C-u C-c ! x re-enables it

Some checkers recognise the exit status their tool uses to say it could not run
at all, as opposed to reporting findings, and step aside rather than failing the
same way on every check.  The reason in parentheses comes from the tool.

Fix whatever it names and the checker works again.  To bring it back without
restarting Emacs, use :kbd:`C-u C-c ! x`.

This is also how a linter with no configuration file behaves.  A stylelint with
nothing to lint by, for instance, disables itself instead of reporting an error
on every check.

Flycheck does nothing at all in a buffer
-----------------------------------------

Look at the mode line first; see :ref:`flycheck-mode-line-troubleshooting`
below.  ``FlyC-`` means no syntax checker applies to the buffer, which usually
means the tool is not installed or the buffer’s major mode has no checker.
:kbd:`C-c ! v` lists every checker that could apply and why each one is or is
not usable.

.. _flycheck-mode-line-troubleshooting:

Start from the mode line
========================

Flycheck says what state it is in next to the mode name, and every state that
is not a set of error counts is a single character:

==============  ==============================================================
``FlyC``        Not checked yet
``FlyC-``       No syntax checker for this buffer
``FlyC*``       A check is running
``FlyC:2|1|0``  Finished: two errors, one warning, no infos
``FlyC!``       The syntax checker could not be run
``FlyC.``       The check was interrupted
``FlyC?``       The checker returned something Flycheck could not read
==============  ==============================================================

Every one of these except the counts explains itself if you hover it, and
clicking it runs :command:`flycheck-verify-setup` on the buffer, which is where
the answer usually is.  Clicking the error counts opens the error list instead.

A trailing ``+`` on the counts means errors were suppressed because the check
produced more than `flycheck-checker-error-threshold`.

Verify your setup
=================

If your issue is none of the aforementioned :ref:`common issues
<flycheck-common-issues>` the first step is to let Flycheck check your setup:

.. define-key:: C-c ! v
                M-x flycheck-verify-setup

   Show a :term:`verification buffer` with information about your
   :mode:`flycheck` setup for the current buffer.

   The buffer contains all syntax checkers available for the current buffer and
   tells you whether Flycheck would use each one and what reasons would prevent
   Flycheck from using a checker.  It also includes information about your
   Flycheck and Emacs version and your operating system.

The following image shows a :term:`verification buffer`:

.. image:: /images/flycheck-verify-buffer.png

The buffer groups the checkers by what will actually happen to them: the one
that runs, the ones that would run if you selected them, and the ones that
cannot run as things stand.  Click a checker's name for its docstring.

* *Green* items indicate *good* configuration.  In the screenshot `python-ruff`
  and `python-pycompile` were both found.

* *Orange* items indicate a *potential* misconfiguration.  The screenshot shows
  that no configuration file was found for `python-ruff`, which is perfectly
  fine if the project has none, but not so good if you meant Flycheck to use
  one.  The section :ref:`flycheck-checker-config-files` has more information
  about configuration files.

* *Red* items indicate *bad* configuration.  Neither `python-flake8` nor
  `python-pylint` was found in the screenshot, so neither can run in that
  buffer, and each says it disabled itself rather than failing on every check.
  :kbd:`C-u C-c ! x` brings a disabled checker back once you have installed it.

A checker that is registered nowhere is called out separately, under a heading
telling you to add it to `flycheck-checkers`.

The verification buffer also shows:

* **What the last failed check reported**, at the very top, when a checker could
  not be run or printed something Flycheck could not read.  This is the tool’s
  own output, and it is usually the whole answer.
* Whether each checker is **disabled** (manually via `C-c ! x`, automatically
  due to too many errors, or because the checker :ref:`stepped aside
  <flycheck-common-issues>`).  You can re-enable disabled checkers with `C-u
  C-c ! x`.
* The **selected checker** for the current buffer (set via `C-c ! s`), if any.
* For a buffer backed by a language server, **how many diagnostics its server
  pushed** and how many of those changed anything.  A server pushing many times
  a second is a very different problem from a checker that is simply slow.
* Your **Flycheck version**, **Emacs version** and **operating system**, which
  is useful information when reporting bugs.

.. tip::

   When :ref:`reporting a bug <flycheck-bug-reports>`, include the output of
   `C-c ! v` in your report.  It gives maintainers a quick overview of your
   setup.

LSP diagnostics
===============

Diagnostics that come from a language server, through either
``flycheck-eglot-mode`` or ``flycheck-lsp-mode``, fail differently from command
checkers: there is no exit status and no output to read, just a server that may
or may not be saying anything.  See :ref:`flycheck-syntax-checks` for how the
two integrations work.

**Nothing appears in an Eglot buffer.**  Eglot renders its diagnostics through
Flymake by default and provides no Flycheck backend, so Flycheck shows nothing
until you bridge them with ``global-flycheck-eglot-mode``.  Check the mode is
actually on in the buffer; it only activates where Eglot manages one.

**Nothing appears with the native checker.**  ``flycheck-lsp-mode`` does nothing
in a buffer whose major mode has no entry in `flycheck-lsp-servers`, or whose
server program is not installed.  The verification buffer says which of the two
it is.  A server that fails its ``initialize`` handshake is torn down and
retried on the next check, so a buffer that never produces diagnostics may be
failing to start the server at all.

**Only one of two servers reports.**  Both bridges chain onward only when their
``exclusive`` option is nil; see :ref:`flycheck-lsp-alongside-eglot`.

**Emacs slows down or stutters.**  A language server publishes diagnostics
whenever it likes, and some publish continuously while they index or build a
project.  Each push that carries something new re-runs the check that publishes
it.  Run :kbd:`C-c ! v` while it is happening and look at the push counts: a
server pushing many times a second is the problem, not the checks themselves.
Turning `global-flycheck-annotate-mode` off tells you whether the cost is in the
number of checks or in what each one draws.

For the underlying protocol traffic, Eglot keeps its own event log.  Set
``eglot-events-buffer-config`` (``eglot-events-buffer-size`` on Emacs 29 and
earlier), reproduce, and read the ``*EGLOT … events*`` buffer.

Debug syntax checkers
=====================

If a syntax checker fails although it successfully verified you need to take a
closer look.  Flycheck provides you with a command that lets you run a single
syntax checker just the way Flycheck would run it:

.. define-key:: C-c ! C-c
                M-x flycheck-compile

   Prompt for a syntax checker and run it as a shell command, showing the whole
   output in a separate buffer.

   .. note::

      This only works for command checkers.  A checker backed by a language
      server has no command line to reproduce; see `LSP diagnostics`_ above.

The output of this command can provide you helpful clues about what’s going on.
It also helps to compare the output of the command in Emacs with what happens if
you run the same command in a terminal.

If all else fails…
==================

…please do :ref:`ask for help <flycheck-get-help>`.  You can ask questions on
Stack Exchange or open an issue on GitHub, and we try to help you as fast and as
well as possible.
