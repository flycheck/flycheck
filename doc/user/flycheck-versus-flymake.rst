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

   Flymake was significantly overhauled in Emacs 26.1 and has continued to
   evolve since.  As Flycheck and Flymake are both moving targets we can't
   guarantee that the data here will always reflect the current state of both
   projects.  If you notice that something is incorrect, please let our team
   know.

.. _flymake-easy: https://github.com/purcell/flymake-easy
.. _edit this page: https://github.com/flycheck/flycheck/edit/master/doc/user/flycheck-versus-flymake.rst

Overview
========

This table gives an overview of the differences and similarities between
Flycheck and Flymake.  The rest of this page describes each point in more
detail.

.. list-table::
   :header-rows: 1
   :widths: 26 38 36

   * -
     - Flycheck
     - Flymake
   * - Supports Emacs versions
     - |min-emacs|
     - 26.1+
   * - Built-in
     - no
     - yes
   * - `Supported languages`_
     - 100+ built-in, 200+ w/ 3rd-party
     - ~10 built-in, 50+ w/ 3rd party
   * - `Automatic syntax checking`_
     - built-in
     - manual
   * - Check triggers
     - save, newline, change, buffer switch, revert
     - save, newline, change
   * - Asynchronous checking
     - yes
     - yes
   * - `Automatic syntax checker selection <Syntax checker selection_>`_
     - by major mode and custom predicates
     - no
   * - `Multiple syntax checkers per buffer`_
     - yes (configurable chain)
     - yes (all at once)
   * - `Definition of new syntax checkers`_
     - single declarative macro [#]_
     - arbitrary function
   * - Configuration debugging
     - built-in (``C-c ! v``)
     - none
   * - `Error identifiers`_
     - yes
     - no
   * - `Error explanations`_
     - yes
     - no
   * - `Error parsing helpers <Error parsing_>`_
     - for regexp, JSON, XML and SARIF
     - none
   * - Fringe icons for errors
     - yes
     - yes
   * - Error highlighting
     - faces, brackets, mixed
     - faces only
   * - `Error indicators <margins>`_
     - fringes (incl. HiDPI), margins
     - fringes only
   * - `Error message display`_
     - tooltip, echo area, inline, fully customizable
     - tooltip, echo area, inline eol (30+)
   * - `Inline diagnostics`_
     - eol, below, sideline, background tint
     - end-of-line summary (30+)
   * - `Applying fixes`_
     - yes (``C-c ! f``, fix-all, LSP code actions)
     - via Eglot code actions
   * - List of all errors
     - yes; grouping, filtering, project-wide
     - yes; project-wide (28+)
   * - `Native LSP diagnostics`_
     - yes (built-in ``lsp`` checker)
     - no (needs Eglot)
   * - `Supported by Eglot`_
     - yes (``flycheck-eglot-mode``)
     - yes (built-in)
   * - `Supported by lsp-mode`_
     - yes
     - yes

Detailed review
===============

Relation to Emacs
-----------------

**Flymake** has been part of GNU Emacs since GNU Emacs 22.  As such,
contributions to Flymake are subject to the FSF policies on GNU projects.  Most
notably, contributors are required to assign their copyright to the FSF.

**Flycheck** is not part of GNU Emacs.  However, it is free software as well,
and publicly developed on the well-known code hosting platform :gh:`GitHub
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
checking in every supported language, including remote files over TRAMP
(encrypted buffers are excluded by default).

Syntax checkers
---------------

Supported languages
~~~~~~~~~~~~~~~~~~~

**Flymake** comes with built-in backends for Emacs Lisp, Ruby (``ruby`` for
syntax check and ``rubocop`` for lints), Python, Perl, and a handful of others.
The community provides additional backends via third-party packages.  When used
with Eglot, Flymake gains support for any language with an LSP server.

**Flycheck** provides support for `over 60 languages <flycheck-languages>` with
over 100 syntax checkers, most of them contributed by the community.  When used
with ``lsp-mode`` or ``flycheck-eglot``, Flycheck also gains LSP-based checking.

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
explanations in a help buffer (see `flycheck-explain-error-at-point`).

Applying fixes
~~~~~~~~~~~~~~

**Flymake** has no built-in facility to apply fixes.  Code actions are available
only through a separate client such as Eglot (``eglot-code-actions``).

**Flycheck** can apply the machine-applicable fixes that checkers suggest:
``C-c ! f`` fixes the error at point and ``C-c ! F`` fixes the whole buffer at
once, as a single undoable change.  ``eslint --fix``, Clippy's suggestions,
``ruff``, ``shellcheck``, SARIF-based checkers and LSP ``quickfix`` code actions
all feed this, and fixable errors get a distinct "fix available" indicator.

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

**Flymake** lets backends parse error messages from
tools.  There are no built-in helpers for defining error patterns, or for
parsing JSON or XML formats.

**Flycheck** checkers can use regular expressions as well as custom parsing functions.
The preferred way to define a checker is to use the `rx` syntax, extended with
custom forms for readable error patterns.  Flycheck includes some ready-to-use
parsing functions for common output formats, such as Checkstyle XML, JSON
interleaved with plain text, or SARIF.

Error message display
~~~~~~~~~~~~~~~~~~~~~

**Flymake** shows error messages in a tool tip if the user hovers
the mouse over an error location, or in the echo area if the user navigates to
the error with `flymake-goto-next-error`.

**Flycheck** shows error messages in tool tips as well, and also displays error
messages in the echo area if the point is at an error location.  This feature is
fully customizable via `flycheck-display-errors-function`, and several
`extensions <flycheck-extensions>` already provide alternative ways to display
errors.

Inline diagnostics
~~~~~~~~~~~~~~~~~~

**Flymake**, since Emacs 30, can show a compact diagnostic summary at the end of
the offending line with ``flymake-show-diagnostics-at-end-of-line``.

**Flycheck** renders error messages inline as well, with more layout control,
via ``flycheck-annotate-mode`` (or ``global-flycheck-annotate-mode`` to enable it
everywhere Flycheck checks).  Messages can appear as a compact summary at the end
of the line, stacked on their own lines below the code with connectors to the
offending column, or flushed to the window's right edge; error lines can also get
an Error Lens-style background tint.

Supported by Eglot
~~~~~~~~~~~~~~~~~~

`Eglot <https://github.com/joaotavora/eglot>`_ is the built-in LSP client in
Emacs (since version 29).  It uses Flymake internally to display diagnostics
from the LSP server.

Eglot does not provide built-in Flycheck support, but Flycheck bridges the gap
itself with ``flycheck-eglot-mode`` (see :doc:`syntax-checks`).  It routes
Eglot's LSP diagnostics through Flycheck, so you can use Eglot for LSP features
while keeping Flycheck for error display, navigation and your existing non-LSP
checkers.  This obsoletes the third-party ``flycheck-eglot`` package.

Supported by lsp-mode
~~~~~~~~~~~~~~~~~~~~~

`lsp-mode <https://github.com/emacs-lsp/lsp-mode>`_ is a popular alternative to
Eglot that supports both Flycheck and Flymake out of the box.

Native LSP diagnostics
~~~~~~~~~~~~~~~~~~~~~~

Both Eglot and ``lsp-mode`` are full LSP clients.  **Flymake** relies on one of
them (usually Eglot) for any LSP diagnostics.

**Flycheck** can also talk to a diagnostics language server directly, over the
built-in ``jsonrpc`` library, with no Eglot or ``lsp-mode`` involved.  Enable
``global-flycheck-lsp-mode`` and Flycheck starts the server itself and reports
its diagnostics; this suits single-purpose LSP linters such as RuboCop, Ruff,
Biome and Harper.  See :doc:`syntax-checks`.

.. rubric:: Footnotes

.. [#] `flymake-easy`_ provides a function to define a new syntax checker, which
       sets all required variables at once.
