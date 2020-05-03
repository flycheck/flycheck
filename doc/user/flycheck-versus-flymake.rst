.. _flycheck-versus-flymake:

=========================
 Flycheck versus Flymake
=========================

This article compares Flycheck to the *built-in* Flymake mode.  It does not
consider third-party extensions such as flymake-easy_, but references them at
appropriate places.

We aim for this comparison to be fair and comprehensive, but it may contain
stale information.  Please report any inaccuracy you might find, and feel free
to `edit this page`_ and improve it.

.. note::

   This comparison was updated at the time of the Emacs 26.1 release, which
   contains an overhaul of Flymake.  If you are using Emacs 25.3 or below, you
   can still access the comparison between Flycheck and the legacy Flymake
   `here`_.

.. _flymake-easy: https://github.com/purcell/flymake-easy
.. _edit this page: https://github.com/flycheck/flycheck/edit/master/doc/user/flycheck-versus-flymake.rst
.. _here: /en/31/

Overview
========

This table gives an overview of the differences and similarities between
Flycheck and Flymake.  The rest of this page describes each point in more
detail.

+---------------------------+-----------------------+-----------------------+
|                           |Flycheck               |Flymake                |
+===========================+=======================+=======================+
|Supports Emacs versions    ||min-emacs|            |26.1+                  |
+---------------------------+-----------------------+-----------------------+
|Built-in                   |no                     |yes                    |
+---------------------------+-----------------------+-----------------------+
|`Supported languages`_     |100+ built-in,         |10 built-in,           |
|                           |200+ w/ 3rd-party      |50+ w/ 3rd party       |
+---------------------------+-----------------------+-----------------------+
|`Automatic syntax          |built-in               |manual                 |
|checking`_                 |                       |                       |
+---------------------------+-----------------------+-----------------------+
|Check triggers             |save, newline, change, |save, newline, change  |
|                           |buffer switch          |                       |
+---------------------------+-----------------------+-----------------------+
|Asynchronous checking      |yes, always            |yes, for some modes    |
+---------------------------+-----------------------+-----------------------+
|`Automatic syntax checker  |by major mode and      |no                     |
|selection <Syntax checker  |custom predicates      |                       |
|selection_>`_              |                       |                       |
+---------------------------+-----------------------+-----------------------+
|`Multiple syntax checkers  |yes (configurable      |yes (all at once)      |
|per buffer`_               |chain)                 |                       |
+---------------------------+-----------------------+-----------------------+
|`Definition of new         |single declarative     |arbitrary function     |
|syntax checkers`_          |macro                  |[#]_                   |
+---------------------------+-----------------------+-----------------------+
|Configuration debugging    |built-in (C-c ! v)     |none                   |
+---------------------------+-----------------------+-----------------------+
|`Error identifiers`_       |yes                    |no                     |
+---------------------------+-----------------------+-----------------------+
|`Error explanations`_      |yes                    |no                     |
+---------------------------+-----------------------+-----------------------+
|`Error parsing helpers     |for regexp, JSON and   |none                   |
|<Error parsing_>`_         |XML                    |                       |
+---------------------------+-----------------------+-----------------------+
|Fringe icons for errors    |yes                    |yes                    |
+---------------------------+-----------------------+-----------------------+
|Error highlighting         |faces, brackets, mixed |faces only             |
+---------------------------+-----------------------+-----------------------+
|`Error indicators          |fringes (incl HiDPI),  |fringes only           |
|<margins>`_                |margins                |                       |
+---------------------------+-----------------------+-----------------------+
|`Error message display`_   |tooltip, echo area,    |tooltip, echo area     |
|                           |fully customizable     |                       |
|                           |(e.g. tooltip, popup   |                       |
|                           |w/ 3rd party packages) |                       |
+---------------------------+-----------------------+-----------------------+
|List of all errors         |yes; filterable by     |yes                    |
|                           |error level            |                       |
+---------------------------+-----------------------+-----------------------+

Detailed review
===============

Relation to Emacs
-----------------

**Flymake** has been part of GNU Emacs since GNU Emacs 22.  As such,
contributions to Flymake are subject to the FSF policies on GNU projects.  Most
notably, contributors are required to assign their copyright to the FSF.

**Flycheck** is not part of GNU Emacs.  However, it is free software as well,
and publicly developed on the well-known code hosting platform :gh:`Github
<flycheck/flycheck>`.  Contributing to Flycheck does not require a copyright
assignment, only an explicit agreement that your contributions will be licensed
under the GPL.

Automatic syntax checking
-------------------------

**Flymake** is not enabled automatically for supported languages.  It must be
enabled for each mode individually, or by, e.g., adding to a hook that enables
it for all ``prog-mode`` buffers.  If no backends for the major mode are
available, Flymake will non-intrusively tell you in the modeline.

**Flycheck** provides a global mode `global-flycheck-mode` which enables syntax
checking in every supported language, where it is safe to do so (remote and
encrypted buffers are excluded by default).

Syntax checkers
---------------

Supported languages
~~~~~~~~~~~~~~~~~~~

**Flymake** comes with support for Emacs Lisp, Ruby (``ruby`` for syntax check
and ``rubocop`` for lints), Python and Perl.  In addition, backends written for
the legacy Flymake are compatible with the new implementation.

**Flycheck** provides support for `over 50 languages <flycheck-languages>` with
over 100 syntax checkers, most of them contributed by the community.

Definition of new syntax checkers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** backends are single functions which report diagnostics to a callback
function given as argument.

**Flycheck** provides a single function `flycheck-define-checker` to define a
new syntax checker.  This function uses a declarative syntax which is easy to
understand even for users unfamiliar with Emacs Lisp.  In fact, most syntax
checkers in Flycheck were contributed by the community.

For example, the Perl checker in Flycheck is defined as follows:

.. code-block:: elisp

   (flycheck-define-checker perl
     "A Perl syntax checker using the Perl interpreter.

   See URL `http://www.perl.org'."
     :command ("perl" "-w" "-c" source)
     :error-patterns
     ((error line-start (minimal-match (message))
             " at " (file-name) " line " line
             (or "." (and ", " (zero-or-more not-newline))) line-end))
     :modes (perl-mode cperl-mode))

The whole process is described in :ref:`adding-a-checker`.

Customization of syntax checkers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** does not provide built-in means to customize syntax checkers.
Instead, when defining a new syntax checker the user needs to declare
customization variables explicitly and check their value in the init function.

**Flycheck** provides built-in functions to add customization variables to
syntax checkers and splice the value of these variables into the argument list
of a syntax checking tool.  Many syntax checkers in Flycheck provide
customization variables.  For instance, you can customize the enabled warnings
for C with `flycheck-clang-warnings`.  Flycheck also tries to automatically find
configuration files for syntax checkers.

Executables of syntax checkers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** does not provide built-in means to change the executable of a syntax
checker.

**Flycheck** defines a variable to set the path of a syntax checker tool for
each defined syntax checker and provides the interactive command
`flycheck-set-checker-executable` to change the executable used in a buffer.
The process used to locate checker configuration files can also be customized
using `flycheck-locate-config-file-functions`, allowing you to store your
personal checker configuration files in your ``.emacs.d`` folder.

Syntax checker selection
------------------------

**Flymake** runs all functions added to the `flymake-diagnostic-functions` hook.

**Flycheck** uses the major mode and checker-specific predicates to
 automatically select a syntax checker.

Custom predicates
~~~~~~~~~~~~~~~~~

**Flymake** may allow for backends to implement custom logic to decide whether
to run the check or not.  There are no easily-defined predicate functions.

**Flycheck** supports custom predicate functions.  For instance, Emacs uses
a single major mode for various shell script types (e.g. Bash, Zsh, POSIX Shell,
etc.), so Flycheck additionally uses a custom predicate to look at the value of
the variable `sh-shell` in Sh Mode buffers to determine which shell to use for
syntax checking.

Manual selection
~~~~~~~~~~~~~~~~

**Flymake** users may manually select a specific backend by overriding the value
of the backends list.

**Flycheck** provides the local variable `flycheck-checker` to explicitly use a
specific syntax checker for a buffer and the command `flycheck-select-checker`
to set this variable interactively.

Multiple syntax checkers per buffer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Flymake** will use all the backends added to the
`flymake-diagnostic-functions` hook to check a buffer; all backends are started
at the same time, but errors are reported in the buffer as soon as a backend
returns them.  Backends can also be written to first report errors for the
visible region of the buffer, and collect errors for hidden regions later.

**Flycheck** can also apply multiple syntax checkers per buffer, but checkers
run in sequence rather than concurrently.  For instance, Flycheck will check PHP
files with PHP CLI first to find syntax errors, then with PHP MessDetector to
additionally find idiomatic and semantic errors, and eventually with PHP
CheckStyle to find stylistic errors.  The user will see all errors reported by
all of these tools in the buffer.  These checker-chains are configurable (see
:ref:`flycheck-checker-chains`), so it's possible to run an advanced style
checker only if a basic syntax checker returned no errors (this avoids
accumulating too many false positives and improves performance).

Errors
------

Error identifiers
~~~~~~~~~~~~~~~~~

**Flymake** does not include special treatment for error identifiers.

**Flycheck** supports identifiers for different kinds of errors, if a syntax
checker provides these.  The identifiers appear in the error list and in error
display, and can be copied independently, for instance for use in an inline
suppression comment or to search the web for a particular kind of error.

Error explanations
~~~~~~~~~~~~~~~~~~

Some **Flycheck** checkers can use error identifiers to provide error
explanations in an help buffer (see `flycheck-explain-error-at-point`).

.. _margins:

Error indicators
~~~~~~~~~~~~~~~~

Both **Flymake** and **Flycheck** indicate errors in the buffer (using overlays)
and in the fringes.  Flycheck includes fringe bitmaps for HiDPI screens, and
also supports displaying indicators in the margins instead of the fringes (this
behavior can be customized using `flycheck-indication-mode`, and
`flycheck-highlighting-mode`).

Error parsing
~~~~~~~~~~~~~

**Flymake** lets backend parse error messages from
tools.  There are no built-in helpers for defining error patterns, or for
parsing JSON or XML formats.

**Flycheck** checkers can use regular expressions as well as custom parsing functions.
The preferred way to define a checker is to use the `rx` syntax, extended with
custom forms for readable error patterns.  Flycheck includes some ready-to-use
parsing functions for common output formats, such as Checkstyle XML, or JSON
interleaved with plain text.

Error message display
~~~~~~~~~~~~~~~~~~~~~

**Flymake** shows error messages in a tool tip if the user hovers
the mouse over an error location, or in the echo area if the user navigates to
the error with `flymake-goto-next-error`.

**Flycheck** shows error message in tool tips as well, and also displays error
messages in the echo area if the point is at an error location.  This feature is
fully customizable via `flycheck-display-errors-function`, and several
`extensions <flycheck-extensions>` already provide alternative way to display
errors.

.. rubric:: Footnotes

.. [#] `flymake-easy`_ provides a function to define a new syntax checker, which
       sets all required variables at once.
