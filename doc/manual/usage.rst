.. _usage:

=======
 Usage
=======

.. _flycheck-mode:

.. require:: flycheck

Flycheck Mode
=============

Syntax checking is done in the Flycheck minor mode:

.. command:: flycheck-mode

   Toggle Flycheck Mode for the current buffer.

.. option:: flycheck-mode

   Whether Flycheck Mode is enabled in the current buffer.

.. command:: global-flycheck-mode

   Toggle Flycheck Mode for **all** live buffers, and for new buffers.

   With Global Flycheck Mode, Flycheck Mode is automatically enabled in all
   buffers, for which a :term:`suitable syntax checker` exists and is enabled.

   .. note::

      Flycheck Mode will **not** be enabled in buffers for remote or encrypted
      files.  The former is flaky and might be very slow, and the latter might
      leak confidential data to temporary directories.

      You can still explicitly enable Flycheck Mode in such buffers with
      :command:`flycheck-mode`.  This is **not** recommended though.

.. option:: global-flycheck-mode

   Whether Flycheck Mode is enabled globally.

If you like Flycheck Mode, you may want to enable it permanently, either by
customizing :option:`global-flycheck-mode`, or by adding the following code to
your :file:`init.el`:

.. code-block:: cl

   (add-hook 'after-init-hook #'global-flycheck-mode)

.. _syntax-checking:

Syntax checking
===============

By default, :ref:`flycheck-mode` checks syntax automatically when

- the mode is enabled,
- the file is saved,
- new lines are inserted,
- or some short time after the last change to the buffer.

However, you can customize automatic syntax checking with
:option:`flycheck-check-syntax-automatically`:

.. option:: flycheck-check-syntax-automatically
   :auto:

.. option:: flycheck-idle-change-delay
   :auto:

Regardless of automatic syntax checking you can also check the buffer
manually:

.. command:: flycheck-buffer
   :binding: C-c ! c
   :auto:

Each syntax check conducts the following steps:

1. Run hooks in :hook:`flycheck-before-syntax-check-hook`
2. Clear error information from previous syntax checks.
3. Select a :term:`suitable syntax checker`.  See
   :ref:`syntax-checker-selection`, for more information on how syntax checkers
   are selected.
4. Copy the contents of the buffer to be checked to a temporary file.
5. Run the syntax checker.
6. Parse the output of the tool, and report all errors and warnings.  See
   :ref:`error-reporting`, for more information.
7. If the buffer can be checked with another syntax checker, continue from step
   4, but with the next syntax checker.  This is called “chaining” of syntax
   checkers.
8. Run hooks in :hook:`flycheck-after-syntax-check-hook`.

.. hook:: flycheck-after-syntax-check-hook
   :auto:

   See :ref:`error-reporting`, for more information about error reporting.

.. hook:: flycheck-before-syntax-check-hook
   :auto:

There is also a hook run whenever a syntax check fails:

.. hook:: flycheck-syntax-check-failed-hook
   :auto:

.. _syntax-checker-selection:

Syntax checker selection
========================

By default Flycheck selects a :term:`suitable syntax checker` automatically from
:option:`flycheck-checkers`, with respect to
:option:`flycheck-disabled-checkers`:

.. option:: flycheck-checkers
   :auto:

   An item in this list is a :term:`registered syntax checker`.

.. option:: flycheck-disabled-checkers
   :auto:

A syntax checker in :option:`flycheck-checkers` and **not** in
:option:`flycheck-disabled-checkers` is an :term:`enabled syntax checker`.

Flycheck uses the first enabled and suitable syntax checker for the current
buffer.  See `Languages and syntax checkers` for a list of all available syntax
checkers.

If no :term:`suitable syntax checker` is found, the syntax check is *silently*
omitted.  *No* error is signalled.  Only a special indicator in the mode line
informs about the omitted syntax check.  See `Mode line` for details.

You can manually select a specific syntax checker for the current buffer, too:

.. command:: flycheck-select-checker
   :binding: C-c ! s

   Select the syntax checker for the current buffer by setting
   :variable:`flycheck-checker`, and run a syntax check with the new syntax
   checker.

   Prompt for a syntax checker and set :variable:`flycheck-checker`.

   Any :term:`syntax checker` can be selected with this command, regardless of
   whether it is enabled.

.. command:: flycheck-select-checker
   :prefix-arg: C-u
   :binding: C-c ! s
   :noindex:

   Deselect the current syntax checker, and run a syntax check with an
   automatically selected syntax checker.

   Set :variable:`flycheck-checker` to `nil`.

.. function:: flycheck-select-checker
   :noindex:
   :auto:

.. variable:: flycheck-checker
   :auto:

You can change the completion system used by
:command:`flycheck-select-checker`:

.. option:: flycheck-completion-system
   :auto:

Each syntax checker provides documentation with information about the executable
the syntax checker uses, in which buffers it will be used for syntax checks, and
whether it can be configured.  See `Configuration`, for more information about
syntax checker configuration.

.. command:: flycheck-describe-checker
   :binding: C-c ! ?

   Show the documentation of a syntax checker.

.. _syntax-checker-configuration:

Syntax checker configuration
============================

.. _syntax-checker-executables:

Syntax checker executables
--------------------------

For each syntax checker, there is a buffer-local, customizable variable
:varcode:`flycheck-{checker}-executable`, where :var:`checker` is the name of
the syntax checker.

The value of this variable is either nil, or a string.  In the former case,
Flycheck uses the default executable from the syntax checker definition when
executing the syntax checker.  In the latter case, it uses the value of the
variable as executable.

Use these variables to override the executable from the definition per buffer.
For instance, you could use a different Emacs version with the `emacs-lisp` or
`emacs-lisp-checkdoc`.

You can either set these variables directly in your :file:`init.el`, or change
them interactively:

.. command:: flycheck-set-checker-executable
   :binding: C-c ! e

   Set the executable of a syntax checker in the current buffer.

   Prompt for a syntax checker and an executable file, and set the
   executable variable of the syntax checker.

.. command:: flycheck-set-checker-executable
   :prefix-arg: C-u
   :binding: C-c ! e
   :noindex:

   Reset the executable of a syntax checker in the current buffer.

   Prompt for a syntax checker and reset its executable to the default.

.. _syntax-checker-options:

Syntax checker options
----------------------

Some syntax checkers can be configured via options.  The following options are
provided by Flycheck (in Emacs, use :kbd:`C-h v` or :kbd:`M-x describe-variable`
on the variable name for detailed help):

.. FIXME: Move these into the languages document, under each specific syntax
   checker.

.. option:: flycheck-clang-definitions

   Additional preprocessor definitions for :flyc-checker:`c/c++-clang`.

.. option:: flycheck-clang-include-path

   Include search path for :flyc-checker:`c/c++-clang`.

.. option:: flycheck-clang-includes

   Additional include files for :flyc-checker:`c/c++-clang`.

.. option:: flycheck-clang-language-standard

   The language standard for :flyc-checker:`c/c++-clang`.

.. option:: flycheck-clang-ms-extensions

   Whether to enable Microsoft extensions in :flyc-checker:`c/c++-clang`.

.. option:: flycheck-clang-no-rtti

   Whether to disable RTTI in :flyc-checker:`c/c++-clang`.

.. option:: flycheck-clang-standard-library

   The standard library to use for :flyc-checker:`c/c++-clang`.

.. option:: flycheck-clang-warnings

   Additional warnings to enable in :flyc-checker:`c/c++-clang`.

.. option:: flycheck-cppcheck-checks

   Additional checks to use in :flyc-checker:`c/c++-cppcheck`.

.. option:: flycheck-dmd-include-path

   Include search path for :flyc-checker:`d-dmd`.

.. option:: flycheck-emacs-lisp-initialize-packages

   Whether to initialize packages (see `package-initialize`) before invoking the
   byte compiler in the :flyc-checker:`emacs-lisp` syntax checker.

   When `nil`, never initialize packages.  When `auto`,
   initialize packages only when checking files from the user's Emacs
   configuration in `user-emacs-directory`.  For any other non-nil
   value, always initialize packages.

.. option:: flycheck-emacs-lisp-load-path

   The `load-path` to use while checking with :flyc-checker:`emacs-lisp`.

   The directory of the file being checked is always added to
   `load-path`, regardless of the value of this variable.

   Note that changing this variable can lead to wrong results of the
   syntax check, e.g. if an unexpected version of a required library
   is used.

.. option:: flycheck-emacs-lisp-package-user-dir

   The package directory for the :flyc-checker:`emacs-lisp` syntax checker.

.. option:: flycheck-eslint-rulesdir

   A directory with custom rules for the :flyc-checker:`javascript-eslint`
   syntax checker.

.. option:: flycheck-flake8-maximum-complexity

   The maximum McCabe complexity the :flyc-checker:`python-flake8` syntax
   checker allows without reporting a warning.

.. option:: flycheck-flake8-maximum-line-length

   The maximum length of a line in characters the :flyc-checker:`python-flake8`
   syntax checker allows without reporting an error.

.. option:: flycheck-ghc-no-user-package-database

   Whether to disable the user package database for :flyc-checker:`haskell-ghc`.

.. option:: flycheck-ghc-package-databases

   A list of package database for :flyc-checker:`haskell-ghc`.

.. option:: flycheck-ghc-search-path

   A list of module directories for the search path of
   :flyc-checker:`haskell-ghc`.

.. option:: flycheck-go-vet-print-functions

   A comma-separated list of print-like functions to check for format string
   issues in the :flyc-checker:`go-vet` syntax checker.

.. option:: flycheck-perlcritic-verbosity

   The verbosity of :flyc-checker:`perl-perlcritic` as integer.

.. option:: flycheck-phpcs-standard

   The coding standard :flyc-checker:`php-phpcs` shall use.

.. option:: flycheck-phpmd-rulesets

   The rule sets :flyc-checker:`php-phpmd` shall use.

.. option:: flycheck-rubocop-lint-only

   Whether to disable style checks for :flyc-checker:`ruby-rubocop`.

.. option:: flycheck-rust-library-path

   The library path for :flyc-checker:`rust`.

.. option:: flycheck-sass-compass

   Whether to enable the Compass CSS framework for :flyc-checker:`sass`.

.. option:: flycheck-scss-compass

   Whether to enable the Compass CSS framework for :flyc-checker:`scss`.

.. option:: flycheck-sphinx-warn-on-missing-references

   Whether to warn about missing references in :flyc-checker:`rst-sphinx`

.. _syntax-checker-configuration-files:

Syntax checker configuration files
----------------------------------

.. FIXME: Move these into the languages document, under each specific syntax
   checker.

Some syntax checkers also read configuration files, denoted by associated
*configuration file variables*:

.. option:: flycheck-chktexrc

   The configuration file for the :flyc-checker:`tex-chktex` syntax checker.

.. option:: flycheck-coffeelintrc

   The configuration file for the :flyc-checker:`coffee-coffeelint` syntax
   checker.

.. option:: flycheck-eslintrc

   The configuration file for the :flyc-checker:`javascript-eslint` syntax
   checker.

.. option:: flycheck-flake8rc

   The configuration file for the :flyc-checker:`python-flake8` syntax checker.

.. option:: flycheck-gjslintrc

   The configuration file for the :flyc-checker:`javascript-gjslint` syntax
   checker.

.. option:: flycheck-jshintrc

   The configuration file for the :flyc-checker:`javascript-jshint` syntax
   checker.

.. option:: flycheck-pylintrc

   The configuration file for the :flyc-checker:`python-pylint` syntax checker.

.. option:: flycheck-rubocoprc

   The configuration file for the :flyc-checker:`ruby-rubocop` syntax checker.

.. option:: flycheck-tidyrc

   The configuration file for the :flyc-checker:`html-tidy` syntax checker.

The value of these variables is either a string or `nil`.  In the former case,
locate the configuration file using the functions in
:option:`flycheck-locate-config-file-functions`:

.. option:: flycheck-locate-config-file-functions
   :auto:

With the default value of this variable, configuration files are located by the
following algorithm:

1. If the configuration file variable contains a path a directory
   separator, expand the path against the buffer's default directory and
   use the resulting path as configuration file.
2. If Projectile_ is available and the current buffer is part of a project
   project, search the configuration file in the root directory of the project.
3. If the buffer has a file name, search the buffer's directory and any
   ancestors thereof for the configuration file.
4. Eventually attempt to locate the configuration file in the user's home
   directory.

If any of these steps succeeds, the subsequent steps are not executed.

If the configuration file is found, pass it to the syntax checker upon
invocation.

If the configuration file is not found, or if the value of the variable is nil,
invoke the syntax checker without a configuration file.

Customize these variables using :kbd:`M-x customize-group RET
flycheck-config-files`.  Use `flycheck-describe-checker` to find out whether a
syntax checker has a configuration file.

You may also use these variables as file-local variables.  For instance, the
following checks the Javascript file always with :file:`.jshintrc` from the
parent directory:

.. code-block:: javascript

   // Local variables:
   // flycheck-jshintrc: "../.jshintrc"
   // End:

.. _Projectile: https://github.com/bbatsov/projectile

.. _error-reporting:

Error reporting
===============

Errors and warnings from a syntax checker are

- reported in the mode line or in a popup buffer, depending on the length
  of the error messages,
- indicated according to :option:`flycheck-indication-mode`,
- and highlighted in the buffer with the corresponding faces, according to
  :option:`flycheck-highlighting-mode`

.. face:: flycheck-error
          flycheck-warning
          flycheck-info

   The faces to use to highlight errors, warnings and info messages
   respectively.

   .. note::

      The default faces provided by GNU Emacs are ill-suited to highlight errors
      because these are relatively pale and do not specify a background color or
      underline.  Hence highlights are easily overlook and even **invisible**
      for white space.

   For best error highlighting customize these faces, or choose a color
   theme that has reasonable Flycheck faces.  For instance, the Solarized_ and
   Zenburn_ themes are known to have good Flycheck faces.

.. option:: flycheck-highlighting-mode
   :auto:

   This variable determines how to highlight errors:

   `columns`
       Highlights the error column.  If the error does not have a column,
       highlight the whole line.

   `symbols`
       Highlights the symbol at the error column, if there is any, otherwise
       behave like @code{columns}.  This is the default.

   `sexps`
       Highlights the expression at the error column, if there is any, otherwise
       behave like `columns`.  Note that this mode can be **very** slow in some
       major modes.

   `lines`
       Highlights the whole line of the error.

   `nil`
      Do not highlight errors at all.  However, errors will still be reported
      in the mode line and in error message popups, and indicated according to
      :option:`flycheck-indication-mode`.

.. face:: flycheck-fringe-error
          flycheck-fringe-warning
          flycheck-fringe-info

   The faces of fringe indicators for errors, warnings and info messages
   respectively.

.. option:: flycheck-indication-mode
   :auto:

You can also completely customize error processing by hooking into Flycheck:

.. hook:: flycheck-process-error-functions
   :auto:

   .. seealso::

      :ref:`error-api`

If you hover a highlighted error with the mouse, a tooltip with the top-most
error message will be shown.

Flycheck also displays errors under point after a short delay:

.. option:: flycheck-display-errors-delay
   :auto:

The error is displayed via :option:`flycheck-display-errors-function`:

.. option:: flycheck-display-errors-function
   :auto:

The default function displays the error messages in the echo area:

.. function:: flycheck-display-error-messages
   :auto:

You can also work with the error messages at point, and copy them into the kill
ring or search them on Google:

.. command:: flycheck-copy-messages-as-kill
   :binding: C-c ! C-w

   Copy all Flycheck error messages at the current point into kill ring.

.. command:: flycheck-google-messages
   :binding: C-c ! /

   Google for all Flycheck error messages at the current point.

   If there are more than :option:`flycheck-google-max-messages` errors at
   point, signal an error, to avoid spamming your browser with Google tabs.

   Requires the `Google This`_ library, which is available on MELPA_.

.. option:: flycheck-google-max-messages
   :auto:

You can also show a list with all errors in the current buffer:

.. command:: flycheck-list-errors
             list-flycheck-errors
   :binding: C-c ! l

   List all errors in the current buffer in a separate buffer.

   The error list automatically refreshes after a syntax check, and follows the
   current buffer and window, that is, if you switch to another buffer or
   window, the error list is updated to show the errors of the new buffer or
   window.

When you move the point in the current buffer while the error list is visible,
all errors at point and at the current line are highlighted in the error list
with :face:`flycheck-error-list-highlight-at-point` and
:face:`flycheck-error-list-highlight` respectively.

.. face:: flycheck-error-list-highlight-at-point
   :auto:

.. face:: flycheck-error-list-highlight
   :auto:

Ultimately, you can clear all reported errors at once:

.. command:: flycheck-clear
   :binding: C-c ! C

   Clear all Flycheck errors and warnings in the current buffer.

   You should not normally need this command, because Flycheck checks the buffer
   periodically anyway.


.. _Solarized: https://github.com/bbatsov/solarized-emacs
.. _Zenburn: https://github.com/bbatsov/zenburn-emacs
.. _Google This: https://github.com/Bruce-Connor/emacs-google-this
.. _MELPA: http://melpa.milkbox.net/

.. _error-navigation:

Error navigation
================

Flycheck integrates into standard error navigation commands of Emacs.  If **no**
compilation buffer (including those from :kbd:`M-x compile`, :kbd:`M-x grep`,
:kbd:`M-x occur`, etc.) is visible, :kbd:`M-g n` (`next-error`) and :kbd:`M-g p`
(`previous-error`) will navigate between Flycheck warnings and errors in the
current buffer.  See :infonode:`(emacs)Compilation Mode` for more information
about these commands.

You can disable this integration by setting
:option:`flycheck-standard-error-navigation` to nil:

.. option:: flycheck-standard-error-navigation
   :auto:

Visible compilation buffers take precedence over Flycheck navigation.  If such a
buffer is visible, :kbd:`M-g n` and :kbd:`M-g p` will ignore Flycheck errors and
warnings, and navigate errors (or generally results) reported by the compilation
buffer instead.

To address this issue, Flycheck provides independent error navigation commands,
which are not affected by :option:`flycheck-standard-error-navigation`:

.. command:: flycheck-next-error
   :binding: C-c ! n

   Jump to the next Flycheck error.

   With prefix argument, jump forwards by as many errors as specified by
   the prefix argument, e.g. :kbd:`M-3 C-c ! n` will move to the 3rd error
   from the current point.

.. command:: flycheck-previous-error
   :binding: C-c ! p

   Jump to the previous Flycheck error.

   With prefix argument, jump backwards by as many errors as specified by
   the prefix argument, e.g. :kbd:`M-3 C-c ! p` will move to the 3rd
   previous error from the current point.

.. command:: flycheck-first-error

   Jump to the first Flycheck error.

   With prefix argument, jump forwards to by as many errors as specified by
   the prefix argument, e.g. :kbd:`M-3 M-x flycheck-first-error` moves to
   the 3rd error from the beginning of the buffer.

.. _mode-line:

Mode line
=========

Flycheck indicates its state in the mode line:

`FlyC`
    There are no errors in the current buffer.

`FlyC*`
    A syntax check is being performed currently.

`FlyC:3/4`
    There are three errors and four warnings in the current buffer.

`FlyC-`
    Automatic syntax checker selection did not find a suitable syntax checker.
    See :ref:`syntax-checker-selection` for more information.

`FlyC!`
    The syntax check failed.  Inspect the `*Messages*` buffer for details.

`FlyC?`
    The syntax check had a dubious result.  The definition of the syntax checker
    may be flawed.  Inspect the `*Messages*` buffer for details.

    This indicator should **never** be displayed for built-in syntax checkers.
    If it is, please report an issue to the Flycheck developers, as by
    :ref:`reporting-issues`.
