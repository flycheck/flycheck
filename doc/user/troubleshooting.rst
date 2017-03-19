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

Flycheck can’t find any programs in GUI Emacs on MacOS
------------------------------------------------------

Try to install and configure exec-path-from-shell_ to make a GUI Emacs inherit
the ``$PATH`` environment variable from your shell configuration.

The issue is that due to the special way MacOS starts GUI programs a GUI Emacs
does not inherit the environment variables from the shell configuration so Emacs
will lack some important entries in ``$PATH``, most notably ``/usr/local/bin/``
where Homebrew, NPM and many other package managers put binaries in.

The `exec-path-from-shell`_ works around this issue by extracting environment
variables from a shell session and inject them into the environment of the
running Emacs instance.

.. _exec-path-from-shell: https://github.com/purcell/exec-path-from-shell

Flycheck warns about “non-zero exit code, but no errors”
--------------------------------------------------------

Make sure that you have the latest version of the syntax checker installed,
particularly if the message started appearing after you updated Flycheck.

Newer releases of Flycheck may require newer versions of syntax checking tools.
For instance Flycheck might now pass a command line flag that older versions do
not understand, or attempt to parse an updated output format.  In these cases
the syntax checker will show an error message about an unknown flag, or emit
output that Flycheck does not understand, which prompts Flycheck to warn that
even though the syntax checker appeared to not have successfully checked the
buffer content there are no errors to be found.

If you *are* using the latest version then this message most likely indicates a
flaw in the syntax checker definition.  In this case please :ref:`report a bug
<flycheck-bug-reports>` to us so that we can fix the issue.  Please don’t forget
to say that you are using the latest version!

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

The buffer shows all syntax checkers for the current buffer.  Note that you can
click on the syntax checker names to show the docstring for a syntax checker.

* *Green* items indicate *good* configuration.  In the screenshot both
  `python-flake8` and `python-pycompile` exist.

* *Orange* items indicate a *potential* misconfiguration.  The screenshot shows
  that no configuration file was found for `python-flake8` which is perfectly
  fine if there’s no flake8 configuration file in the project, but not so good
  if you’d like Flycheck to use a configuration file for flake8.  The section
  :ref:`flycheck-checker-config-files` has more information about configuration
  files.

  Likewise the buffer warns you that a ``demo`` syntax checker (which is not
  part of Flycheck of course) isn’t registered in `flycheck-checkers`.  If you’d
  like Flycheck to automatically use this syntax checker you should fix this
  issue by adding it to `flycheck-checkers` but otherwise it’s safe to ignore
  this warning.

* *Red* items indicate *bad* configuration.  `python-pylint` wasn’t found in the
  screenshot, so you’ll not be able to use pylint in the current buffer.

Debug syntax checkers
=====================

If a syntax checker fails although it successfully verified you need to take a
closer look.  Flycheck provides you with a command that lets you run a single
syntax checker just the way Flycheck would run it:

.. define-key:: C-c ! C-c
                M-x flycheck-compile

   Prompt for a syntax checker and run in as a shell command, showing the whole
   output in a separate buffer.

   .. important::

      The current implementation this command suffers from a couple of issues,
      so we’d like to have a replacement in GH-854_ and we could use your help!
      If you’d like to help out with this task please join the discussion in
      that issue.

      .. _GH-854: https://github.com/flycheck/flycheck/issues/854

The output of this command can provide you helpful clues about what’s going on.
It also helps to compare the output of the command in Emacs with what happens if
you run the same command in a terminal.

If all else fails…
==================

…please do :ref:`ask for help <flycheck-get-help>`.  We have many different
channels, from Twitter to a chat room to StackOverflow, whatever suits you best,
and we try to help you as fast and as well as possible.
