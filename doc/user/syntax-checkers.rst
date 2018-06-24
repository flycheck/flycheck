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
has finished, Flycheck looks for the next syntax checker to run, and if there is
one, Flycheck runs the next syntax checker, and so on, until there is no more
syntax checker for the current buffer.  This process repeats whenever Flycheck
needs to check the buffer according to `flycheck-check-syntax-automatically`.

.. important::

   Under some circumstances—for instance if the syntax checker is not installed—
   Flycheck automatically :ref:`disables syntax checkers
   <flycheck-disable-checkers>` in the current buffer and will thus not even
   consider them in any future checks in the current buffer.

   In the `verification buffer <C-c ! v>` these syntax checkers are marked as
   “disabled” just as if you had disabled them manually with `C-c ! x`, and
   likewise you can re-enable automatically disabled syntax checkers with `C-u
   C-c ! x`.

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
flycheck-disabled-checkers RET (emacs-lisp-checkdoc)` in your :term:`user emacs
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

.. _flycheck-checker-options:

Configure syntax checkers
=========================

Many syntax checkers provide command line flags to change their behaviour.
Flycheck wraps important flags as regular Emacs user options.

The :ref:`list of supported languages <flycheck-languages>` includes all options
for each syntax checker.  You can change these options in the Customize
interface under :menuselection:`programming --> tools --> flycheck -->
flycheck-options`, however we recommend to use Directory Variables to configure
syntax checkers per project.

.. seealso::

   :infonode:`(emacs)Directory Variables`
      Information about directory variables.

.. _flycheck-checker-config-files:

Configuration files
-------------------

Some syntax checkers can additionally read configuration from files.  Flycheck
can find configuration files of syntax checkers and use them when invoking the
syntax checker program:

.. defcustom:: flycheck-local-config-file-functions

   Functions to call to find a configuration file for a syntax checker.  Each
   function gets the name of a configuration file and shall return the absolute
   path to a file if one exists.  The default value leads to the following
   steps:

   1. If the name is an absolute path, use it.
   2. If the name exists in any ancestor directory, use the nearest one.
   3. If the name exists in ``$HOME``, use it.

   This option is an abnormal hook, see :infonode:`(elisp)Hooks`.

Flycheck takes the names of configuration files from user options defined for
syntax checkers that support configuration files.  Like above the :ref:`list of
languages <flycheck-languages>` also lists all supported configuration file
options.  You can also change these in Customize, under
:menuselection:`programming --> tools --> flycheck --> flycheck-config-files`,
but again we recommend to use Directory Variables.

We also recommend to prefer configuration files over options as you can usually
commit the configuration files to your source control repository to share them
with other contributors so that all contributors can use the same configuration
for syntax checking and linting.

.. _flycheck-checker-executables:

Change syntax checker executables
=================================

Flycheck normally tries to run syntax checker tools by their standard name from
`exec-path`.  Sometimes, though, you need to use a different version of a tool,
or probably don't even have a tool available globally—this frequently occurs in
Javascript project where dependencies including linter tools are typically
installed into a local ``node_modules`` directory:

.. define-key:: M-x flycheck-set-checker-executable

   Prompt for a syntax checker and an executable file and make Flycheck use the
   executable file for the syntax checker in the current buffer.

   Internally this command sets a variable named
   :samp:`flycheck-{checker}-executable` where :samp:`{checker}` is the name of
   the syntax checker entered on the prompt, e.g. `c/c++-clang`.

   Flycheck defines these :term:`executable options` for every syntax checker
   that runs an external command.  You can change these variables with directory
   variables or set them in custom Emacs Lisp code such as mode hooks.

   .. seealso::

      :infonode:`(emacs)Directory Variables`
         Information about directory variables.

.. _flycheck-checker-chains:

Configuring checker chains
==========================

In any given buffer where Flycheck is enabled, only one checker may be run at a
time.  However, any number of checkers can be run in sequence.  In such a
sequence, after the first checker has finished running and its errors have been
reported, the next checker of the sequence runs and its errors are reported,
etc. until there are no more checkers in the sequence.  This sequence is called
a *checker chain*.

Some checkers chains are already setup by default in Flycheck: e.g.,
`emacs-lisp` will be followed by `emacs-lisp-checkdoc`, and `python-mypy` will
be followed by `python-flake8`.

When defining a checker, you can specify which checkers may run after it by
setting the ``:next-checkers`` property (see the docstring of
`flycheck-define-generic-checker`).

For a given checker, several next checkers may be specified.  Flycheck will run
the first (in order of declaration) whose error level matches (see below) and
which can be used in the current buffer.

You can also customize the next checker property by calling
`flycheck-add-next-checker` in your Emacs configuration file.

.. defun:: flycheck-add-next-checker checker next &optional append

   Set *next* to run after *checker*.  Both arguments are syntax checker
   symbols.

   For example, the following will make `python-pylint` run after
   `python-flake8`:

   .. code-block:: elisp

      (flycheck-add-next-checker 'python-flake8 'python-pylint)

   *Next* may also be a cons cell ``(level . next-checker)``, where
   *next-checker* is a symbol denoting the syntax checker to run after
   *checker*, and *level* is an error level.  The *next-checker* will then only
   be run if there is no current error whose level is more severe than *level*.
   If *level* is ``t``, then *next-checker* is run regardless of the current
   errors.

   For instance, if you wanted to run `python-pylint` only if `python-flake8`
   produced no errors (only warnings and info diagnostics), then you would
   rather use:

   .. code-block:: elisp

      (flycheck-add-next-checker 'python-flake8 '(warning . python-pylint))
