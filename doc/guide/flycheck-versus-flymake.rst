.. _flycheck-versus-flymake:

=========================
 Flycheck versus Flymake
=========================

.. default-role:: code

This article provides information about Flycheck compares to the *built-in*
Flymake mode.  It does not consider the improved `Flymake fork`_ or third-party
extensions such as `flymake-easy`_ or `flymake-cursor`_, but references them at
appropriate places.

We aim for this comparison to be neutral and complete, but do not provide any
guarantee for completeness or correctness of the following information.
Moreover, we consider Flycheck superior to Flymake in all aspects.  As such, you
may find this page biased towards Flycheck.  Please excuse this as well as any
factual mistake or lack of information.  Please suggest improvements.

.. contents:: Table of contents
   :local:

Overview
========

This table intends to give an overview about the differences and similarities
between Flycheck and the default install of Flymake. It is not a direct
comparision to third-party extensions such as flymake-easy, flymake-cursor, or
forks of Flymake. For a more comprehensive look compared to those extensions,
please read the details in the main article and the footnotes.

Please do *not* only use this table to make your personal judgment, but also
read the detailed review in the following sections, at least with regards to the
features you are interested in.

+----------------------------------+---------------------+---------------------+
|                                  |Flycheck             |Flymake              |
+----------------------------------+---------------------+---------------------+
|Supported Emacs versions          |24+                  |22+                  |
+----------------------------------+---------------------+---------------------+
|Part of Emacs                     |no [#]_              |yes                  |
+----------------------------------+---------------------+---------------------+
|Automatically enabled in supported|yes                  |no                   |
|languages                         |                     |                     |
+----------------------------------+---------------------+---------------------+
|Checks after                      |save, newline, change|save, newline, change|
+----------------------------------+---------------------+---------------------+
|Checks in background              |yes                  |yes                  |
+----------------------------------+---------------------+---------------------+
|Automatic syntax checker selection|By major mode and    |By file name patterns|
|                                  |custom predicates    |[#]_                 |
+----------------------------------+---------------------+---------------------+
|Manual syntax checker selection   |yes                  |**no**               |
+----------------------------------+---------------------+---------------------+
|Multiple syntax checkers per      |yes                  |**no** [#]_          |
|buffer                            |                     |                     |
+----------------------------------+---------------------+---------------------+
|Supported languages               |> 30                 |~5 [#]_              |
+----------------------------------+---------------------+---------------------+
|Checking remote files via Tramp   |no                   |partly?              |
|                                  |                     |                     |
|                                  |                     |                     |
+----------------------------------+---------------------+---------------------+
|Definition of new syntax checkers |Via a single         |By a function        |
|                                  |declarative macro    |definition and       |
|                                  |                     |various variables    |
|                                  |                     |[#]_                 |
+----------------------------------+---------------------+---------------------+
|Customization of syntax checkers  |yes                  |no                   |
|                                  |                     |                     |
|                                  |                     |                     |
+----------------------------------+---------------------+---------------------+
|Error levels                      |Errors, warnings,    |Errors and warnings  |
|                                  |information, and     |[#]_                 |
|                                  |custom levels        |                     |
+----------------------------------+---------------------+---------------------+
|Error parsing                     |Regular expressions, |Regular expressions  |
|                                  |or custom error      |                     |
|                                  |parsers (e.g. XML,   |                     |
|                                  |JSON, etc.)          |                     |
+----------------------------------+---------------------+---------------------+
|Multiline error messages          |yes                  |**no** [#]_          |
+----------------------------------+---------------------+---------------------+
|Error highlighting in the buffer  |yes                  |yes                  |
+----------------------------------+---------------------+---------------------+
|Fringe icons for errors           |yes                  |yes                  |
+----------------------------------+---------------------+---------------------+
|Error messages display            |Via tooltip and echo |In tooltip [#]_      |
|                                  |area, fully          |                     |
|                                  |customizable         |                     |
+----------------------------------+---------------------+---------------------+
|List all errors in the buffer     |yes                  |**no**               |
+----------------------------------+---------------------+---------------------+
|Resource consumption              |low                  |high [#]_            |
+----------------------------------+---------------------+---------------------+
|Unit tests                        |all syntax checkers, |**none?**            |
|                                  |and large parts of   |                     |
|                                  |the underlying       |                     |
|                                  |processing           |                     |
+----------------------------------+---------------------+---------------------+

Detailed review
===============

Relation to Emacs
-----------------

**Flymake** is part of GNU Emacs since GNU Emacs 22.  As such, contributions to
Flymake are subject to the FSF policies on GNU projects.  Most notably,
contributors are required to assign their copyright to the FSF by signing a
contributor agreement.

**Flycheck** is not part of GNU Emacs, and is **unlikely to ever be**.  However,
it is free software as well, and publicly developed on the well-known code
hosting platform Github_.  Contributing to Flycheck does not require a copyright
assignments.

Enabling syntax checking
------------------------

**Flymake** is not enabled automatically for supported languages.  It must be be
enabled for each mode individually and **carefully**, because it does not deal
well with unavailable syntax checker tools.  In a GUI frame, it signals errors
in GUI dialogs.  In a TTY frame, it does not signal any error at all, but
instead silently hangs.  The same occurs, when a syntax checker tool becomes
unavailable after Flymake Mode is enabled, for instance, because the underlying
tool was uninstalled.

.. only:: not format_texinfo

   .. figure:: /images/flymake-error.png
      :scale: 75%
      :align: center

      Flymake showing a GUI dialog to inform that a syntax checker tool is not
      available

The third-party library `flymake-easy`_ provides an alternate way to enable
Flymake Mode, which gracefully handles unavailable syntax checkers.  It does not
check whether the tool still exists before a syntax check, though, and thus does
still exposes above behavior, when a tool becomes unavailable after the mode was
enabled.

**Flycheck** provides a global mode (see :command:`global-flycheck-mode`), which
enables syntax checking in every supported language.  If a syntax checking tool
is not available, Flycheck fails gracefully, does not enable syntax checking,
and just indicates the failure in the mode line.

Syntax checkers
---------------

Built-in syntax checkers
~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** supports Java, Makefiles, Perl, PHP, TeX/LaTeX and XML.  Notably, it
does *not* support Emacs Lisp.  A third-party `Flymake fork`_ supports more
languages, though.  Furthermore there are many recipes for other languages on
the `Flymake page`_ in the EmacsWiki, and many extension packages for other
languages in the popular ELPA archive MELPA_.

**Flycheck** provides support for |#flycheck-languages| languages (see
:ref:`supported-languages`) with |#flycheck-checkers| syntax checkers, most of
them contributed by the community.  Notably, Flycheck does *not* support Java
and Makefiles.

Definition of new syntax checkers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** does not provide a single function to define a new syntax checker.
Instead, one has to define an “init” function, which returns the command, and
add this function to `flymake-allowed-file-name-masks`.  Additionally, one has
to add the error patterns to `flymake-err-line-patterns`.  As such, defining a
syntax checker is difficult for users who are not familiar with Emacs Lisp.
`flymake-easy`_ provides an easier way to define new syntax checkers, though.

**Flycheck** provides a single function :macro:`flycheck-define-checker` to
define a new syntax checker.  This function uses a declarative syntax, which is
easy to understand even for users unfamiliar with Emacs Lisp.  In fact, many
syntax checkers in Flycheck were `contributed by the community`_.

For example, the Perl checker in Flymake is defined as follows.

.. code-block:: cl

   (defun flymake-perl-init ()
     (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
            (local-file  (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
       (list "perl" (list "-wc " local-file))))

   (defcustom flymake-allowed-file-name-masks
     '(;; …
       ("\\.p[ml]\\'" flymake-perl-init)
       ;; …
       ))

   (defvar flymake-err-line-patterns ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
     (append
      '(;; …
        ;; perl
        ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
        ;; …
        )
      ;; …
     ))

Whereas Flycheck's definition of the same checker looks like this:

.. code-block:: cl

   (flycheck-define-checker perl
     "A Perl syntax checker using the Perl interpreter.

   See URL `http://www.perl.org'."
     :command ("perl" "-w" "-c" source)
     :error-patterns
     ((error line-start (minimal-match (message))
             " at " (file-name) " line " line
             (or "." (and ", " (zero-or-more not-newline))) line-end))
     :modes (perl-mode cperl-mode))

Customization of syntax checkers
--------------------------------

**Flymake** does not provide built-in means to customize syntax checkers.
Instead, when defining a new syntax checker, the user needs to declare
customization variables explicitly and explicitly check their value in the init
function.

**Flycheck** provides built-in functions to add customization variables to
syntax checkers and splice the value of these variables into the argument list
of a syntax checking tool.  Many syntax checkers in Flycheck provide
customization variables.  For instance, you can customize the enabled warnings
for C with :option:`flycheck-clang-warnings`.  Flycheck also tries to
automatically find configuration files for syntax checkers.

Executables of syntax checkers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** does not provide built-in means to change the executable of a syntax
checker.

**Flycheck** implicitly defines a variable to set the path of a syntax checker
tool for each defined syntax checker, and provides the interactive command
:command:`flycheck-set-checker-executable` to change the executable used in a
buffer.

Syntax checker selection
------------------------

**Flymake** selects syntax checkers based on file name patterns in
`flymake-allowed-file-name-masks`.  Effectively, this duplicates the existing
logic Emacs uses to choose the right major mode, but lacks its flexibility and
power.  For instance, Flymake cannot pick a syntax checker based on the shebang
of a file.

**Flycheck** uses the major mode to select a syntax checker.  This reuses the
existing sophisticated logic Emcas uses to choose and configure major modes.
Flycheck can easily select a Python syntax checker for a Python script without
file extension, but with proper shebang, simply because Emacs correctly chooses
Python Mode for such a file.

Custom predicates
~~~~~~~~~~~~~~~~~

**Flymake** does not allow for custom predicates to implement more complex logic
for syntax checker selection.  For instance, Flymake cannot use different syntax
checkers for buffer depending on the value of a local variable.

However, `flymake-easy`_ patches Flymake to allow for custom syntax checkers per
buffer.  This does not happen automatically though.  The user still needs to
explicitly register a syntax checker in a major mode hook.

**Flycheck** supports custom predicate function.  It uses these to implement the
shell script syntax checkers, for instance.  Emacs uses a single major mode for
various shell script types (e.g. Bash, Zsh, POSIX Shell, etc.), so Flycheck
additionally uses a custom predicate to look at the value of the variable
`sh-shell` in Sh Mode buffers, to determine which shell to use for syntax
checking.

Manual selection
~~~~~~~~~~~~~~~~

**Flymake** does not provide means to manually select a specific syntax checker,
either interactively, or via local variables.

**Flycheck** provides the local variable :variable:`flycheck-checker` to
explicitly use a specific syntax checker for a buffer, and the command
:command:`flycheck-select-checker` to set this variable interactively.

Multiple syntax checkers per buffer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** can only use a single syntax checker per buffer.  Effectively, the
user can only use a single tool to check a buffer, for instance either PHP Mess
Detector or PHP CheckStyle.  Third party extensions to Flycheck work around this
limitation by supplying custom shell scripts to call multiple syntax checking
tools at once.

**Flycheck** can easily apply multiple syntax checkers per buffer.  For
instance, Flycheck will check PHP files with PHP CLI first to find syntax
errors, then with PHP MessDetector to additionally find idiomatic and semantic
errors, and eventually with PHP CheckStyle to find stylistic errors.  The user
will see all errors reported by all of these utilities in the buffer.

Error levels
------------

**Flymake** supports error and warning messages.  The pattern of warning
messages is *hard-coded* in Emacs 24.3, and only became customizable in upcoming
Emacs 24.4.  The patterns to parse messages are kept separate from the actual
syntax checker.

The third-party `Flymake fork`_ also supports info messages, and makes the
pattern of warning messages customizable as well.

**Flycheck** supports error, warning and info messages.  The patterns to parse
messages of different levels are part of the syntax checker definition, and thus
specific to each syntax checker.  Flycheck allows to define new error levels for
use in custom syntax checkers with :function:`flycheck-define-error-level`.

Error parsing
-------------

**Flymake** parses the output of syntax checker tools with regular expressions
only.  As it splits the output by lines regardless of the regular expressions,
it does not support error messages spanning multiple lines (such as returned by
the Emacs Lisp byte compiler or by the Glasgow Haskell Compiler).

`flymake-easy`_ overrides internal Flymake functions to support multiline error
messages.

**Flycheck** can use regular expressions as well as custom parsing functions.
By means of such functions, it can parse JSON, XML or other structured output
formats.  Flycheck includes some ready-to-use parsing functions for well-known
output formats, such as Checkstyle XML.  By parsing structured output format,
Flycheck can handle arbitrarily complex error messages.  Also, with regular
expressions it uses the error patterns to split the output into tokens, and thus
handles multiline messages just as well.

Error message display
---------------------

In GUI frames, **Flymake** shows error messages in a tool tip, if the user
hovers the mouse over an error location.  It does not provide means to show
error messages in a TTY frame, or with the keyboard only.

.. only:: not format_texinfo

   .. figure:: /images/flymake-tooltip.png
      :scale: 75%
      :align: center

      Tooltip showing a Flymake error message

The third-party library `flymake-cursor`_ shows Flymake error messages at point
in the echo area, by overriding internal Flymake functions.

**Flycheck** shows error message tool tips as well, but also displays error
messages in the echo area, if the point is at an error location.  This feature
is fully customizable via :option:`flycheck-display-errors-function`.

.. only:: not format_texinfo

   .. figure:: /images/flycheck-tooltip-and-echo-area.png
      :scale: 75%
      :align: center

      Tooltip and echo area showing a Flycheck error message

   .. figure:: /images/flycheck-echo-area-in-tty-frame.png
      :scale: 75%
      :align: center

      Echo area showing a Flycheck error messages in a TTY frame.

Error list
----------

**Flymake** does not provide means to list all errors in the current buffer.

**Flycheck** can list all errors in the current buffer in a separate window.
This error list is automatically updated after each syntax check, and follows
the focus.

.. only:: not format_texinfo

   .. figure:: /images/flycheck-error-list.png
      :scale: 75%
      :align: center

      Listing all errors in the current buffer

Resource consumption
--------------------

Syntax checking
~~~~~~~~~~~~~~~

**Flymake** starts a syntax check after every change, regardless of whether the
buffer is visible in a window or not.  It does not limit the number of
concurrent syntax checks.  As such, Flymake starts many concurrent syntax
checks, if many buffers are changed at the same time (e.g. after a VCS revert),
which is known to freeze Emacs temporarily.

The third-party `Flymake fork`_ limits the number of concurrent syntax checks.
It does not take care to check visible buffers first, though.

**Flycheck** does not conduct syntax checks in buffers which are not visible in
any window.  Instead it defers syntax checks in such buffers until after the
buffer is visible again.  Hence, Flycheck does only start as many concurrent
syntax checks as there are visible windows in the current Emacs session.

Checking for changes
~~~~~~~~~~~~~~~~~~~~

**Flymake** uses a *separate* timer (in `flymake-timer`) to periodically check
for changes in each buffer.  These timers run even if the corresponding buffers
do not change.  This is known to cause considerable CPU load with many open
buffers.

The third-party `Flymake fork`_ uses a single global timer to check for changes.
This greatly reduces the CPU load, but still consumes some marginal CPU, even if
Emacs is idle and not in use currently.

**Flycheck** does not use timers at all to check for changes.  Instead it
registers a handler for Emacs' built-in `after-change-functions` hook, which is
run after changes to the buffer.  This handler is only invoked when the buffer
actually changed, and starts a one-shot timer to delay the syntax check until
the editing stopped for a short time, to save resources and avoid checking
half-finished editing.

Unit tests
----------

**Flymake** does not have a test suite at all.

**Flycheck** has unit tests for all built-in syntax checkers, and for large
parts of the underlying machinery and API.  Contributed syntax checkers are
required to have test cases.  The tests are continuously run on `Travis CI`_.

.. [#] Flycheck is **unlikely to ever become part of Emacs**.
.. [#] The 3rd party library flymake-easy_ allows to use syntax checkers per
       major mode.
.. [#] Various 3rd party packages thus use custom shell scripts to call multiple
       syntax checking tools at once.
.. [#] However, the `Flymake page`_ in the EmacsWiki provides recipes for many
       other languages, although of varying quality.  Furthermore, the popular
       ELPA archive MELPA provides many packages which add more languages to
       Flymake.  There is also a `Flymake fork`_, which supports more languages
       out of the box, among other fixes and improvements.
.. [#] flymake-easy_ provides a function to define a new syntax checker, which
       sets all required variables at once.
.. [#] The `Flymake fork`_ adds support for info messages.
.. [#] flymake-easy_ *overrides* internal functions of Flymake to add support
       for multiline error messages.
.. [#] The 3rd party library flymake-cursor_ shows Flymake error messages at
       point in the echo area.
.. [#] A third-party `Flymake fork`_ mostly fixes the performance and resource
       consumption issues in Flymake.

.. _flymake-easy: https://github.com/purcell/flymake-easy
.. _contributed by the community: https://github.com/flycheck/flycheck/issues?labels=checker&milestone=&page=1&state=closed
.. _flymake page: http://www.emacswiki.org/emacs/FlyMake
.. _flymake fork: https://github.com/illusori/emacs-flymake
.. _flymake-cursor: http://www.emacswiki.org/emacs/flymake-cursor.el
.. _Vagrant: http://www.vagrantup.com/
.. _Puppet: http://puppetlabs.com/
.. _MELPA: http://melpa.milkbox.net/
.. _Github: https://github.com/flycheck/flycheck
.. _Travis CI: https://travis-ci.org/flycheck/flycheck
