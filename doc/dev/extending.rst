====================
 Extending Flycheck
====================

.. require:: flycheck

This chapter explains how to add new syntax checkers to Flycheck, and
provides examples for common use cases.

If you define a new syntax checker, *please* contribute it to Flycheck to make
it available to other users, too.  See :ref:`contributing-syntax-checkers` for
more information.

.. _defining-new-syntax-checkers:

Defining new syntax checkers
============================

You define new syntax checkers with :macro:`flycheck-define-checker`:

.. macro:: flycheck-define-checker
   :auto:

.. macro:: flycheck-def-config-file-var
   :auto:

.. macro:: flycheck-def-option-var
   :auto:

.. _registering-new-syntax-checkers:

Registering new syntax checkers
===============================

After a syntax checker was defined, it should be registered for automatic
selection, by adding it to :option:`flycheck-checkers`, e.g.

.. code-block:: cl

   (add-to-list 'flycheck-checkers 'my-new-syntax-checker)

.. note::

   :option:`flycheck-checker` and :command:`flycheck-select-checker` are
   reserved for **user customization**.  Do **not** use them in Flycheck
   extensions.

   Specifically, please do **not** provide a hook function which selects the
   syntax checker explicitly by assigning to :option:`flycheck-checker` or by
   calling :function:`flycheck-select-checker`, e.g.

   .. code-block:: cl

      (defun enable-my-new-syntax-checker ()
        (setq flycheck-checker 'my-new-syntax-checker)
        (flycheck-buffer))

      (add-hook 'my-major-mode-hook #'enable-my-new-syntax-checker)

   This circumvents the entire automatic selection of Flycheck, and prevents the
   user from effectively customizing Flycheck.

.. _error-levels:

Error levels
============

Flycheck provides some built-in error levels:

`error`
   Severe errors which cannot be ignored
`warning`
   Potential errors which can be ignored
`info`
   Informational annotations

You can define new error levels with :function:`flycheck-define-error-level`.

.. _error-parsers:

Error parsers
=============

Syntax checkers may use more sophisticated error parsing by given the
`:error-parser` argument to :function:`flycheck-define-checker`.  See
:ref:`builtin-error-parsers` for a list of builtin parsers.

You may also define your own error parsers.  An error parser is any function
that takes the following three arguments and returns a list of
:cl-struct:`flycheck-error` objects (see :ref:`error-api` for more information):

`output`
   The complete output of the syntax checker as string.
`checker`
   A symbol denoting the executed syntax checker.
`buffer`
   A buffer object referring to the buffer that was syntax-checked.

Flycheck provides some utility functions to implement your own error parsers.
See :ref:`error-parser-api` for details.

.. _extending-syntax-checkers:

Extending syntax checkers
=========================

There are some means to extend defined syntax checkers:

.. function:: flycheck-add-next-checker
   :auto:

Examples
========

.. _basic-syntax-checkers:

Basic syntax checkers
---------------------

As explained in the previous chapter :ref:`defining-new-syntax-checkers`, a
syntax checker is declared with :macro:`flycheck-define-checker`.

We will use this function to define a syntax checker using the PHP CodeSniffer
utility for the PHP programming language:

.. code-block:: cl

   (flycheck-define-checker php-phpcs
     "A PHP syntax checker using PHP_CodeSniffer.

   See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
     :command ("phpcs" "--report=checkstyle" source)
     :error-parser flycheck-parse-checkstyle
     :modes php-mode)

   (add-to-list 'flycheck-checkers 'php-phpcs)

First we specify the `:command` to execute.  The first element of the command
list is the name of the executable, `phpcs` in our case.  This command is
checked for existence with `executable-find` before using this syntax checker.
If the command does not exist, the syntax checker is *not* used.

The executable is following by the arguments, in this case some options and the
symbol `source`.  This symbol is replaced with the file to check.  This file is
a temporary file created in the system temporary directory and containing the
contents of the buffer to check.

Next we specify how to parse the output of the syntax checker.  PHP CodeSniffer
provides an option to output errors in an XML format similar to the Java tool
Checkstyle, so we use the built-in :dfn:`error parser`
:function:`flycheck-parse-checkstyle` to parse the output.

Then we enable the syntax checker in PHP editing modes.

Ultimately, we make the new syntax checker available for automatic selection by
adding it to :option:`flycheck-checkers`.

If the syntax checker does not provide any sort of structured output format, we
have to parse the error messages from the textual output.  To do so, we can use
:dfn:`error patterns`, like in the Pylint syntax checker for the Python
programming language:

.. code-block:: cl

   (flycheck-define-checker python-pylint
     "A Python syntax and style checker using Pylint.

   See URL `http://pypi.python.org/pypi/pylint'."
     :command ("epylint" source-inplace)
     :error-patterns
     ((warning line-start (file-name) ":" line
               ": Warning (W" (zero-or-more not-newline) "): "
               (message) line-end)
      (error line-start (file-name) ":" line
             ": Error (E" (zero-or-more not-newline) "): "
             (message) line-end)
      (error line-start (file-name) ":" line ": [F] " (message) line-end))
     :modes python-mode)

Again, there is a `:command`, however we use the `source-inplace` symbol this
time.  This symbol causes the temporary file to be created in the same directory
as the original file, making information from the source code tree available to
the syntax checker.  In case of Pylint, these are the Python packages from the
source code tree.

Next we give the list of `:error-patterns` to parse errors.  These patterns
extract the error location and the error message from the output of `epylint`.
An error pattern is a list containing a regular expression with match groups to
extract the error information, and an error level.

Eventually we enable the syntax checker in `python-mode`.

Syntax checkers with predicates
-------------------------------

In the previous examples the syntax checkers were specific to certain major
modes.  However, this is not always the case.  For instance, GNU Emacs provides
a single mode only for shell scripting in various Shell languages.  A syntax
checker for a specific shell must check whether the edited shell script is
written for the right shell:

.. code-block:: cl

   (flycheck-define-checker zsh
     "A Zsh syntax checker using the Zsh shell.

   See URL `http://www.zsh.org/'."
     :command ("zsh" "-n" "-d" "-f" source)
     :error-patterns
     ((error line-start (file-name) ":" line ": " (message) line-end))
     :modes sh-mode
     :predicate (lambda () (eq sh-shell 'zsh)))

This syntax checker for the Zsh shell is enabled in `sh-mode` as specified by
`:modes`, however it specifies an additional `:predicate` to determine whether
the right shell is in use.  Hence this syntax checker is only used if a Zsh
shell scripting is being edited in `sh-mode`, but not if a Bash or POSIX Shell
script is being edited.

A syntax checker may even go as far as not having `:modes` at all.  For
instance, there is no special JSON mode, but syntax checking JSON is still
desirable.  Hence a JSON syntax checker may use the `:predicate` to check the
file extension:

.. code-block:: cl

   (flycheck-define-checker json-jsonlint
     "A JSON syntax and style checker using jsonlint.

   See URL `https://github.com/zaach/jsonlint'."
     :command ("jsonlint" "-c" "-q" source)
     :error-patterns
     ((error line-start
             (file-name)
             ": line " line
             ", col " column ", "
             (message) line-end))
     :predicate
     (lambda ()
       (or
        (eq major-mode 'json-mode)
        (and buffer-file-name
             (string= "json" (file-name-extension buffer-file-name))))))

This syntax checker is now used whenever a file ends with `.json`, regardless of
the major mode.

Configuration files for syntax checkers
---------------------------------------

Some syntax checkers can be configured using configuration files given
by an option.  Flycheck provides built-in support to handle such
configuration files:

.. code-block:: cl

   (flycheck-def-config-file-var flycheck-jshintrc javascript-jshint ".jshintrc")

   (flycheck-define-checker javascript-jshint
     "A JavaScript syntax and style checker using jshint.

   See URL `http://www.jshint.com'."
     :command ("jshint" "--checkstyle-reporter"
               (config-file "--config" flycheck-jshintrc) source)
     :error-parser flycheck-parse-checkstyle
     :modes (js-mode js2-mode js3-mode))

As you can see, we define a syntax checker for Javascript, using the `jshint`
utility.  This utility accepts a configuration file via the `--config` option.

To use a configuration file with jshint, we first declare the variable
`flycheck-jshintrc` that provides the name of the file, as explained in
:ref:`syntax-checker-configuration-files`.

In the `:command` we use a `config-file` form to pass the configuration file to
the syntax checker.  If the configuration file is found, its path will be passed
to the syntax checker, using the option specified after the `config-file`
symbol.  Otherwise the whole element is simply omitted from the command line.

Some Syntax checkers can also be configured using options passed on the command
line.  Flycheck supports this case, too.  We will use this facility to extend
the PHP CodeSniffer syntax checker from the :ref:`basic-syntax-checkers` section
with support for coding standards:

.. code-block:: cl

   (flycheck-def-option-var flycheck-phpcs-standard nil phpcs
     "The coding standard for PHP CodeSniffer."
     :type '(choice (const :tag "Default standard" nil)
                    (string :tag "Standard name" nil)))
   (put 'flycheck-phpcs-standard 'safe-local-variable #'stringp)

   (flycheck-declare-checker php-phpcs
     "A PHP syntax checker using PHP_CodeSniffer."
     :command '("phpcs" "--report=checkstyle"
                (option "--standard=" flycheck-phpcs-standard concat)
                source)
     :error-parser 'flycheck-parse-checkstyle
     :modes 'php-mode)

The syntax checker is pretty much the same as before, except that a new element
was added to `:command`.  This element passes the value of the new option
variable :option:`flycheck-phpcs-standard` to the syntax checker.  This variable
is declared with the special macro :macro:`flycheck-def-option-var` at the
beginning.

Chaining syntax checkers
------------------------

For many languages, more than a single syntax checker is applicable.  For
instance, Emacs Lisp can be checked for syntactic corrections with the byte code
compiler, and for adherence to the Emacs Lisp documentation style using
Checkdoc.  PHP, too, can be syntax checked with the PHP parser, and verified
against coding styles using PHP CodeSniffer.

To support such cases, syntax checkers can be :term:`chained <chaining>` using
the `:next-checkers`.  The standard PHP syntax checker uses this to run PHP
CodeSniffer if there are no syntax errors:

.. code-block:: cl

   (flycheck-define-checker php
     "A PHP syntax checker using the PHP command line interpreter.

   See URL `http://php.net/manual/en/features.commandline.php'."
     :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
               "-d" "log_errors=0" source)
     :error-patterns
     ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
             (message) " in " (file-name) " on line " line line-end))
     :modes (php-mode php+-mode)
     :next-checkers ((warning . php-phpcs)))

Now PHP CodeSniffer will check the coding style, but only if PHP CodeSniffer is
a :term:`registered syntax checker`, and if `php` only emitted errors with
`warning` level or less, that is, no errors.
