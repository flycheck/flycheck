=======
 Usage
=======

Syntax checking
===============

By default, Flycheck Mode checks syntax automatically when

- the mode is enabled,
- the file is saved,
- new lines are inserted,
- or some short time after the last change to the buffer.

However, you can customize automatic syntax checking with
:el:option:`flycheck-check-syntax-automatically`

.. el:option:: flycheck-check-syntax-automatically

   When Flycheck should check syntax automatically.

   This variable is list of events that may trigger syntax checks.  The
   following events are known:

   `mode-enabled`
       Check syntax immediately when `flycheck-mode` is enabled.

   `save`
       Check syntax immediately after the buffer was saved.

   `new-line`
       Check syntax immediately after a new line was inserted into the buffer.

   `idle-change`
       Check syntax a short time after the last change to the buffer.

   An syntax check is only conducted for events that are contained in this list.
   For instance, the following setting will cause Flycheck to *only* check if
   the mode is enabled or the buffer was saved, but *never* after changes to the
   buffer contents:

   .. code-block:: cl

      (setq flycheck-check-syntax-automatically '(mode-enabled save))

   If the list is empty syntax is never checked automatically.  In this case,
   use :el:command:`flycheck-buffer` to check syntax manually.

.. el:option:: flycheck-idle-change-delay

   How many seconds to wait before starting a syntax check.

   After the buffer was changed, Flycheck waits as many seconds as the value of
   this variable before starting a syntax check.  If the buffer is changed
   during this time, Flycheck starts to wait again.

   This variable has no effect, if `idle-change` is not contained in
   :el:option:`flycheck-check-syntax-automatically`.

Regardless of automatic syntax checking you can also check the buffer
manually:

.. el:command:: flycheck-buffer
   :binding: C-c ! c

   Start a syntax check in the current buffer.

Each syntax check conducts the following steps:

1. Run hooks in :el:hook:`flycheck-before-syntax-check-hook`
2. Clear error information from previous syntax checks.
3. Select a :term:`suitable syntax checker`.  See `Syntax checker selection`_,
   for more information on how syntax checkers are selected.
4. Copy the contents of the buffer to be checked to a temporary file.
5. Run the syntax checker.
6. Parse the output of the tool, and report all errors and warnings.
   See `Error Reporting`_, for more information.
7. If the buffer can be checked with another syntax checker, continue from step
   4, but with the next syntax checker.  This is called “chaining” of syntax
   checkers.
8. Run hooks in :el:hook:`flycheck-after-syntax-check-hook`.

.. el:hook:: flycheck-after-syntax-check-hook

   Functions to run after each syntax check.

   This hook is run after a syntax check was finished.

   At this point, **all** chained checkers were run, and all errors were parsed,
   highlighted and reported.  See `Error reporting`_, for more information about
   error reporting.  The variable :el:variable:`flycheck-current-errors`
   contains all errors from all syntax checkers run during the syntax check, so
   you can use the various error analysis functions.

   Note that this hook does **not** run after each individual syntax checker in
   the syntax checker chain, but only after the **last checker**.

   This variable is a normal hook. @xref{Hooks, , ,elisp}.

.. el:hook:: flycheck-before-syntax-check-hook

   Functions to run before each syntax check.

   This hook is run right before a syntax check starts.

   Error information from the previous syntax check is **not** cleared before
   this hook runs.

   Note that this hook does **not** run before each individual syntax checker in
   the syntax checker chain, but only before the **first checker**.

   This variable is a normal hook. @xref{Hooks, , ,elisp}.

There is also a hook run whenever a syntax check fails:

.. el:hook:: flycheck-syntax-check-failed-hook

   Functions to run if a syntax check failed.

   This hook is run whenever an error occurs during Flycheck's internal
   processing.  No information about the error is given to this hook.

   You should use this hook to conduct additional cleanup actions when Flycheck
   failed.

   This variable is a normal hook. @xref{Hooks, , ,elisp}.

Syntax checker selection
========================

By default Flycheck selects a :term:`suitable syntax checker` automatically from
:el:option:`flycheck-checkers`, with respect to
:el:option:`flycheck-disabled-checkers`:

.. el:option:: flycheck-checkers

   A list of :term:`syntax checker`\ s available for automatic syntax checker
   selection.  An item in this list is a :term:`registered syntax checker`.

   You may modify this list, but you should normally not need to.  Instead, it
   is intended for 3rd party extensions to tell Flycheck about new syntax
   checkers.

.. el:option:: flycheck-disabled-checkers

   A list of :term:`syntax checker`\ s explicitly excluded from automatic
   selection.

   Change this list to disable syntax checkers which you do not want to use.
   You may also use this option as a file or directory local variable to disable
   specific checkers in individual files and directories respectively.

A syntax checker in :el:option:`flycheck-checkers` and **not** in
:el:option:`flycheck-disabled-checkers` is an :term:`enabled syntax checker`.

Flycheck uses the first enabled and suitable syntax checker for the current
buffer.  See `Languages and syntax checkers` for a list of all available syntax
checkers.

If no :term:`suitable syntax checker` is found, the syntax check is *silently*
omitted.  *No* error is signalled.  Only a special indicator in the mode line
informs about the omitted syntax check.  See `Mode line` for details.

You can manually select a specific syntax checker for the current buffer, too:

.. el:command:: flycheck-select-checker
   :binding: C-c ! s

   Prompt for a syntax checker, and select it for the current buffer, by setting
   :el:variable:`flycheck-checker`.

   With prefix arg, deselect the current syntax checker if any, and re-enable
   automatic selection, by setting :el:variable:`flycheck-checker` to `nil`.

   In either case, immediately run a syntax check afterwards.

   Any :term:`syntax checker` can be selected with this command, regardless of
   whether it is enabled.

.. el:variable:: flycheck-checker

   The :term:`syntax checker` to use for the current buffer.

   The variable is buffer local, and safe as file local variable for registered
   checkers.

   If set to `nil`, automatically select a suitable syntax checker.

   If set to a :term:`syntax checker`, only use this syntax checker.  Automatic
   selection as described above is *disabled*.  If the syntax checker not
   suitable, signal an error.

   You may directly set this variable without
   :el:command:`flycheck-select-checker`, e.g. via file local variables.  For
   instance, you can use the following file local variable within a Python
   file to always use :command:`pylint` for the file:

   .. code-block:: python

      # Local Variables:
      # flycheck-checker: python-pylint
      # End:

   @xref{Specifying File Variables, , ,emacs}, for more information about file
   variables.


You can change the completion system used by
:el:command:`flycheck-select-checker`:

.. el:option:: flycheck-completion-system

   The completion system to use.

   `ido`
       Use IDO.

       IDO is a built-in alternative completion system, without good flex
       matching and a powerful UI.  You may want to install flx-ido_ to improve
       the flex matching in IDO.

   `grizzl`
       Use Grizzl_.

       Grizzl is an alternative completion system with powerful flex matching,
       but a very limited UI.

   `nil`
       Use the standard unfancy `completing-read`.

       `completing-read` has a very simple and primitive UI, and does not offer
       flex matching.  This is the default setting, though, to match Emacs'
       defaults.  With this system, you may want enable `icomplete-mode` to
       improve the display of completion candidates at least.

Each syntax checker provides documentation with information about the executable
the syntax checker uses, in which buffers it will be used for syntax checks, and
whether it can be configured.  See `Configuration`, for more information about
syntax checker configuration.

.. el:command:: flycheck-describe-checker
   :binding: C-c ! ?

   Show the documentation of a syntax checker.

.. _flx-ido: https://github.com/lewang/flx
.. _Grizzl: https://github.com/d11wtq/grizzl

Customization
=============

Syntax checker executables
--------------------------

For each syntax checker, there is a buffer-local, customizable variable
`flycheck-CHECKER-executable`, where `CHECKER` is the name of the syntax
checker.

The value of this variable is either nil, or a string.  In the former case,
Flycheck uses the default executable from the syntax checker definition when
executing the syntax checker.  In the latter case, it uses the value of the
variable as executable.

Use these variables to override the executable from the definition per buffer.
For instance, you could use a different Emacs version with the `emacs-lisp` or
`emacs-lisp-checkdoc`.

You can either set these variables directly in your :file:`init.el`, or change
them interactively:

.. el:command:: flycheck-set-checker-executable
   :binding: C-c ! e

   Set the executable of a syntax checker in the current buffer.

   Prompt for a syntax checker, and an executable file, and set the
   corresponding executable variable.

   With prefix arg, prompt for a syntax checker and reset its executable to the
   default.

Syntax checker options
----------------------

Syntax checker options
======================

Some syntax checkers can be configured via options.  The following options are
provided by Flycheck (in Emacs, use :kbd:`C-h v` or :kbd:`M-x describe-variable`
on the variable name for detailed help):

.. el:option:: flycheck-clang-definitions

   Additional preprocessor definitions for `c/c++-clang`.

.. el:option:: flycheck-clang-include-path

   Include search path for `c/c++-clang`.

.. el:option:: flycheck-clang-includes

   Additional include files for `c/c++-clang`.

.. el:option:: flycheck-clang-language-standard

   The language standard for `c/c++-clang`.

.. el:option:: flycheck-clang-ms-extensions

   Whether to enable Microsoft extensions in `c/c++-clang`.

.. el:option:: flycheck-clang-no-rtti

   Whether to disable RTTI in `c/c++-clang`.

.. el:option:: flycheck-clang-standard-library

   The standard library to use for `c/c++-clang`.

.. el:option:: flycheck-clang-warnings

   Additional warnings to enable in `c/c++-clang`.

.. el:option:: flycheck-cppcheck-checks

   Additional checks to use in `c/c++-cppcheck`.

.. el:option:: flycheck-emacs-lisp-initialize-packages

   Whether to initialize packages (see `package-initialize`) before
   invoking the byte compiler in the `emacs-lisp` syntax checker.

   When `nil`, never initialize packages.  When `auto`,
   initialize packages only when checking files from the user's Emacs
   configuration in `user-emacs-directory`.  For any other non-nil
   value, always initialize packages.

.. el:option:: flycheck-emacs-lisp-load-path

   The `load-path` to use while checking with `emacs-lisp`.

   The directory of the file being checked is always added to
   `load-path`, regardless of the value of this variable.

   Note that changing this variable can lead to wrong results of the
   syntax check, e.g. if an unexpected version of a required library
   is used.

.. el:option:: flycheck-emacs-lisp-package-user-dir

   The package directory for the `emacs-lisp` syntax checker.

.. el:option:: flycheck-eslint-rulesdir

   A directory with custom rules for the `javascript-eslint` syntax checker.

.. el:option:: flycheck-flake8-maximum-complexity

   The maximum McCabe complexity the `python-flake8` syntax checker allows
   without reporting a warning.

.. el:option:: flycheck-flake8-maximum-line-length

   The maximum length of a line in characters the `python-flake8` syntax checker
   allows without reporting an error.

.. el:option:: flycheck-ghc-no-user-package-database

   Whether to disable the user package database for `haskell-ghc`.

.. el:option:: flycheck-ghc-package-databases

   A list of package database for `haskell-ghc`.

.. el:option:: flycheck-ghc-search-path

   A list of module directories for the search path of `haskell-ghc`.

.. el:option:: flycheck-phpcs-standard

   The coding standard `php-phpcs` shall use.

.. el:option:: flycheck-phpmd-rulesets

   The rule sets `php-phpmd` shall use.

.. el:option:: flycheck-rubocop-lint-only

   Whether to disable style checks for `ruby-rubocop`.

.. el:option:: flycheck-sass-compass

   Whether to enable the Compass CSS framework for `sass`.

.. el:option:: flycheck-scss-compass

   Whether to enable the Compass CSS framework for `scss`.

.. el:option:: flycheck-sphinx-warn-on-missing-references

   Whether to warn about missing references in `rst-sphinx`

Syntax checker configuration files
----------------------------------

Some syntax checkers also read configuration files, denoted by associated
*configuration file variables*:

.. el:option:: flycheck-chktexrc

   The configuration file for the `tex-chktex` syntax checker.

.. el:option:: flycheck-coffeelintrc

   The configuration file for the `coffee-coffeelint` syntax checker.

.. el:option:: flycheck-eslintrc

   The configuration file for the `javascript-eslint` syntax checker.

.. el:option:: flycheck-flake8rc

   The configuration file for the `python-flake8` syntax checker.

.. el:option:: flycheck-gjslintrc

   The configuration file for the `javascript-gjslint` syntax checker.

.. el:option:: flycheck-jshintrc

   The configuration file for the `javascript-jshint` syntax checker.

.. el:option:: flycheck-pylintrc

   The configuration file for the `python-pylint` syntax checker.

.. el:option:: flycheck-rubocoprc

   The configuration file for the `ruby-rubocop` syntax checker.

.. el:option:: flycheck-tidyrc

   The configuration file for the `html-tidy` syntax checker.

The value of these variables is either a string or `nil`.  In the former case,
locate the configuration file using the functions in
:el:option:`flycheck-locate-config-file-functions`:

.. el:option:: flycheck-locate-config-file-functions

   Functions to locate syntax checker configuration files.

   Each function in this hook must accept two arguments: The value of the
   configuration file variable, and the syntax checker symbol.  It must
   return either a string with an absolute path to the configuration file,
   or nil, if it cannot locate the configuration file.

   The functions in this hook are called in order of appearance, until a
   function returns non-nil.  The configuration file returned by that
   function is then given to the syntax checker if it exists.

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

Error reporting
===============

Error navigation
================

Mode line
=========
