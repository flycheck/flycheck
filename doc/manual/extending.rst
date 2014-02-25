====================
 Extending Flycheck
====================

This chapter explains how to add new syntax checkers to Flycheck, and
provides examples for common use cases.

If you define a new syntax checker, *please* contribute it to Flycheck to make
it available to other users, too.  See :ref:`contributing-syntax-checkers` for
more information.

.. _defining-new-syntax-checkers:

Defining new syntax checkers
============================

You define new syntax checkers with :macro:`flycheck-define-checker`:

.. macro:: flycheck-define-checker symbol docstring &rest properties

   Define :var:`symbol` as new syntax checker with :var:`docstring` and
   :var:`properties`.

   :var:`docstring` provides documentation for the syntax checker.  Use
   :command:`flycheck-describe-checker` to view the documentation of a syntax
   checker.

   The following :var:`properties` constitute a syntax checker.
   :flyc:checkprop:`:command` is mandatory.  A syntax checker must also have
   either :flyc:checkprop:`:modes` or :flyc:checkprop:`:predicate`, and either
   :flyc:checkprop:`:error-patterns` or :flyc:checkprop:`:error-parser`.
   :flyc:checkprop:`:next-checkers` is entirely optional.

   If a mandatory property is missing, or if any property has an invalid value,
   a *compile-time* error is signalled.

   Any syntax checker defined with this macro is eligible for manual syntax
   checker selection with :command:`flycheck-select-checker`.  To make the new
   syntax checker available for automatic selection, you need to register it in
   :option:`flycheck-checkers`.  See :ref:`syntax-checker-selection` for more
   information.

   .. flyc:checker-property:: :command ({executable} [{arg} ...])

      An unquoted list describing the syntax checker command to execute.

      :var:`executable` must be a string with the executable of this
      syntax checker.

      A customizable, buffer-local variable
      :varcode:`flycheck-{symbol}-executable` is implicitly defined to allow
      overriding of the executable.  See :ref:`syntax-checker-executables` for
      details about these variables.  If this variable is non-nil, Flycheck uses
      the value of the variable as executable, otherwise it falls back to
      :var:`executable`.  In either case, the executable is checked with
      `executable-find` before use.

      Each :var:`arg` is an argument to the executable, either as string,
      or as one of the following special symbols and forms.

      .. note::

         These special forms and symbols do **not** apply recursively.  Within
         the body of a cell or the result of the `eval` form, special forms and
         symbols are **not** replaced!

     `source`
        The file to check.

        This file is a temporary file with the contents of the buffer to
        check, created in the system's temporary directory.

        If the buffer to check has a file name, the non-directory component
        of the name of the temporary file will be the same as in the buffer's
        file name, to support languages which enforce specific rules on the
        file name (e.g. the file must be named after the containing feature,
        class, etc.).

        If the buffer to check has no file name, the name of the temporary
        file is random.

        This symbol is the **preferred** way to pass the input file to your
        syntax checker.

     `source-inplace`
        The file to check.

        This file is a temporary file with the contents of the buffer to check,
        created **in the same directory** as the buffer's file.  The name of the
        file is random.

        If the buffer has no file name, this symbol is equivalent to `source`.

        This symbol should be used if and only if the syntax check **needs other
        files** from the source directory (e.g. include files in C, or Python
        modules).

     `source-original`
        The file name of the current buffer, as by `buffer-file-name`, or an
        empty string, if the buffer has no file name.

        This symbol is provided to **additionally** pass the real name of the
        file to check to a syntax checker.

        It should **never** be used as primary input to the syntax checker,
        unless both `source` and `source-original` are absolutely not
        applicable.  In this case, be sure to add a predicate to your syntax
        checker that inhibits the syntax check, if the buffer has no file name,
        or was modified, for otherwise the syntax check will fail or return
        out-dated errors:

        .. code-block:: cl

           :predicate (lambda () (and (buffer-file-name)
                                      (not (buffer-modified-p)))

     `temporary-directory`
        The path to an existing temporary directory, which is unique on each
        execution of the syntax checker.

        Use this symbol if you need to move files created by a syntax checker
        out of the way.

        See the declaration of the :flyc:checker:`elixir` syntax checker for an
        application of this symbol.

     `temporary-file-name`
        The path to a temporary file, which is unique on each execution of the
        syntax checker.  The file is @emph{not} created automatically.

        Use this symbol if you need to move files created by a syntax checker
        out of the way.

        See the declaration of the @code{go-build} syntax checker for an
        application of this symbol.

     :varcode:`(config-file {option} {variable})`
        A configuration file for this syntax checker.

        :var:`option` is a string containing the option that specifies a
        configuration file to the syntax checker tool.

        :var:`variable` is a symbol referring to a variable from which to obtain
        the name or path of the configuration file.  See
        :ref:`syntax-checker-configuration-files` for more information about
        syntax checker configuration.  You need to define this variable
        *explicitly* with :macro:`flycheck-def-config-file-var`.

        If the configuration file is found, pass the :var:`option` and the
        absolute path to the configuration file to the syntax checker.
        Otherwise the cell is simply dropped from the arguments of the syntax
        checker.

        If :var:`option` ends with a `=` character, :var:`option` and the
        absolute path to the configuration file are concatenated and given as a
        single argument to the syntax checker.  Otherwise, :var:`option` and the
        configuration file path are given as two separate arguments.

     :varcode:`(option {option} {variable} [{filter}])`
        The value of a variable.

        :var:`option` is a string containing the option for with to specify the
        value.  :var:`filter` is an optional function to be applied to the value
        of :var:`variable` before use.  See :ref:`option-filters` for a list of
        built-in option filters.

        :var:`variable` is a symbol referring to a variable whose value to use.
        :See ref:`syntax-checker-configuration` for more information about
        :syntax checker configuration.  You need to define this variable
        :*explicitly* with :macro:`flycheck-def-option-var`.

        If :var:`variable` is not `nil` after applying :var:`filter`, pass
        :var:`option` and the value of :var:`variable` after applying
        :var:`filter`.  Otherwise the cell is simply dropped from the arguments
        of the syntax checker.

        An :var:`option` ending with a `=` character is treated like in a
        `config-file` cell.

     :varcode:`(option-list {option} {variable} [{prepend-fn} [{filter}]])`
        Like :var:`option`, but for lists of options.

        :var:`option` is a string containing the option to specify.

        :var:`variable` is a variable whose value must be a list.

        :varcode:`prepend-fn` is a function called with :var:`option` as first
        and the item of the list as second argument.  It should return the
        result of prepending :var:`option` to the item, either as list or as
        string.  If omitted, it defaults to `list`, so by default, :var:`option`
        will be prepended as a separate argument.

        :var:`filter` is an optional function to be applied to each item in the
        list before use.  See :ref:`option-filters` for a list of built-in
        option filters.

        For each item in the value of :var:`variable`, which is not `nil` after
        applying :var:`filter`, pass :var:`option` the the item after applying
        :var:`filter`, as returned by :var:`prepend-fn`.

        Nil items are simply ignored.

     :varcode:`(option-flag {option} {variable})`
        Like `option`, but for boolean flags.

        :var:`option` is a string containing the option to
        specify. :code:`variable` is a symbol referring to a variable.

        If :var:`variable` is non-nil, pass :var:`option` to the syntax checker.
        Otherwise just ignore this argument.

     :varcode:`(eval {form})`
        The result of evaluating `form`.

        :var:`form` is an arbitrary Emacs Lisp form.  It is evaluated literally
        *each time* the syntax checker is executed.  Special forms and symbols
        in :var:`form` are *not* replaced!

        :var:`form` must evaluate either to a string, a list of strings or nil.
        For any other result, signal an error.

        If the result of :var:`form` is not nil, pass the result to the syntax
        checker *literally*.  Special symbols and forms in the result of
        :var:`form` are *not* replaced.

   .. flyc:checker-property:: :error-patterns (({level} {sexp}) ...)

      An unquoted list of one or more error patterns to parse the output of the
      syntax checker :flyc:checkprop:`:command`.

      :var:`level` is a Flycheck error level, and denotes the severity of errors
      matched by the pattern.  This mainly affects the visual representation of
      matched errors in buffers.

      Flycheck provides the built-in error levels `error`, `warning` and `info`.
      You can define your own error levels with
      :function:`flycheck-define-error-level`.

      The :var:`level` is followed by one or more `rx` :var:`sexp` elements.
      See the docstring of the function `rx` for more information.  In addition
      to the standard forms of `rx`, Flycheck supports the following additional
      forms to make specific parts of the error output available to Flycheck:

      `line`
         Matches the line number the error refers to, as a sequence of one or
         more digits.

      `column`
         Matches a column number the error refers to, as a sequence of one or
         more digits.

      :varcode:`(file-name {sexp} ...)`
         Matches the file name the error refers to.  :var:`sexp` matches the
         name of the file.  If no :var:`sexp` is given, use a default body of
         `(minimal-match (one-or-more not-newline))`, which is equivalent to
         `".+?"`.

      :varcode:`(message {sexp} ...)`
         Matches the error message to be shown to the user.  :var:`sexp` matches
         the text of the message.  If no :var:`sexp` is given, use a default
         body of `(one-or-more not-newline)`, which is equivalent to `".+"`.

      Each of these items is optional, however error messages without a `line`
      will be ignored and are not shown in the buffer.

      The patterns are applied in the order of declaration to the whole output
      of the syntax checker.  Output already matched by a pattern will not be
      matched by subsequent patterns.  In other words, the first pattern wins.

   .. flyc:checker-property:: :error-parser {function}
                         :error-parser (lambda (output checker buffer) {body} ...)

      A function to parse the output of the syntax checker, either as *unquoted*
      function symbol or `lambda` form.

      The function must accept three arguments :var:`output`, :var:`checker` and
      :var:`buffer`, where :var:`output` is the output of the syntax checker as
      string, :var:`checker` the syntax checker that was used, and :var:`buffer`
      a buffer object representing the checker buffer.

      The function must return a list of :cl-struct:`flycheck-error` objects
      parsed from :var:`output`.  See :ref:`error-api` for information about
      :cl-struct:`flycheck-error`.  See :ref:`error-parsers` for a list of
      built-in error parsers.

      If this property is given, it takes precedence over
      :flyc:checkprop:`:error-patterns`.  To use an error parser together with
      patterns, you must manually call
      :function:`flycheck-parse-with-patterns` in your error parser to apply
      the error patterns.  You can then manipulate the
      :cl-struct:`flycheck-error` objects returned by this function.

   .. flyc:checker-property:: :modes {mode}
                         :modes ({mode} ...)

      An unquoted major mode symbol or an unquoted list thereof.

      If given, this syntax checker is only used, if the major mode of the
      buffer to check is equal (as in `eq`) to any given :var:`mode`.

      If :flyc:checkprop:`:predicate` is given, it is additionally called in
      buffers of any given :var:`mode`.

   .. flyc:checker-property:: :predicate {function}
                         :predicate (lambda () {body} ...)

      A function to determines whether to use this syntax checker in the current
      buffer, either as unquoted function symbol or as `lambda` form.  The
      syntax checker is only used if this function returns non-nil when called
      in the buffer to check.

      If :flyc:checkprop:`:modes` is given, the function is only called in
      matching major modes.  Thus, if :flyc:checkprop:`:modes` and
      :flyc:checkprop:`:predicate` are given, **both** must match for this
      syntax checker to be used.

   .. flyc:checker-property:: :next-checkers ({item} ...)

      An unquoted list defining the syntax checker to run after this checker.

      Flycheck tries all items in the order of declaration.  Each :var:`item` is
      either a syntax checker symbol or a cons cell :varcode:`({predicate}
      . {checker})`.

      In the former case, the :var:`item` is used, if the syntax checker is
      enabled and suitable for the current buffer.  In the latter case, the
      :var:`predicate` must match additionally.

      :var:`predicate` is either `no-errors` or `warnings-only`:

      `no-errors`
         The syntax :var:`checker` is only considered if the current syntax
         checker reported no errors at all.

      `warnings-only`
         The syntax :var:`checker` is only considered if the current syntax
         checker only reported warnings, but no errors.

.. macro:: flycheck-def-config-file-var symbol checker &optional filename

   Define :var:`symbol` as configuration file variable for a syntax
   :var:`checker`, with a default value of :var:`filename`.

   :var:`symbol` is declared as customizable, buffer-local variable using
   `defcustom`, to provide a configuration file for the given syntax
   :var:`checker`.  The variable has the customization type :var:`string`, :and
   gets a comprehensive docstring, including a reference to :var:`checker`.

   :var:`filename` is used as initial value for the variable.  If omitted, the
   initial value is nil.

   Use this macro together with the `config-file` form in the
   :flyc:checkprop:`:command` of a syntax checker.

.. macro:: flycheck-def-option-var symbol initial-value checker &optional custom-args

   Define :var:`symbol` as option variable for a syntax :var:`checker`, with the given
   :var:`initial-value`.

   :var:`symbol` is declared as customizable variable, buffer-local variable
   using `defcustom`, to provide an option for the given syntax :var:`checker`.
   :var:`symbol` gets a comprehensive docstring, including a reference to
   :var:`checker`.

   :var:`custom-args` are forwarded to `defcustom`.  Use them to declare the
   customization type, etc.

   Use this macro together with the `option`, `option-list` and `option-flag`
   forms in the :flyc:checkprop:`:command` of a syntax checker.

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

You can define new error levels with :function:`flycheck-define-error-level`:

.. function:: flycheck-define-error-level level &rest properties

   Define a new error `level` with `properties`.

   The following `properties` constitute an error level:

   :varcode:`:overlay-category {category}`
      The overlay :var:`category` for `level` as symbol.

      An overlay category is a symbol whose properties provide the default
      values for overlays of this category.  See :infonode:`(elisp)Overlay
      Properties` for more information about overlay properties and categories.

      A category for an error level overlay should at least define the `face`
      property, for error highlighting.  Other useful properties for error level
      categories are `priority` to influence the stacking of multiple error
      level overlays, and `help-echo` to define a default error messages for
      errors without messages.

   :varcode:`:fringe-face {face}`
      A :var:`face` to use for fringe indicators for `level` as symbol.

   :varcode:`:fringe-bitmap {bitmap}`
      A fringe :var:`bitmap` to use for fringe indicators for `level` as symbol.

      See :infonode:`(elisp)Fringe Bitmaps` for a list of built-in fringe
      bitmaps, and instructions on how to define new bitmaps.

.. _error-parsers:

Error parsers
=============

Syntax checkers may use more sophisticated error parsing by using the
:flyc:checkprop:`:error-parser` property.  See
:ref:`defining-new-syntax-checkers` for information about syntax checker
definition.

Flycheck provides the following error parsers for use by syntax checkers:

.. function:: flycheck-parse-with-patterns output checker buffer

   Parse :var:`output` with the :flyc:checkprop:`:error-patterns` of the syntax
   :var:`checker`.

.. function:: flycheck-parse-checkstyle output checker buffer

   Parse :var:`output` as Checkstyle_ XML.

You may also define your own error parsers.  An error parser is any function
that takes the following three arguments and returns a list of
:cl-struct:`flycheck-error` objects (see :ref:`error-api` for more
information):

`output`
   The complete output of the syntax checker as string.
`checker`
   A symbol denoting the executed syntax checker.
`buffer`
   A buffer object referring to the buffer that was syntax-checked.

Flycheck provides some utility functions to implement your own error parsers.
See :ref:`error-parser-api`.

.. _Checkstyle: http://checkstyle.sourceforge.net/

.. _option-filters:

Option filters
==============

Flycheck provides the following built-in option filters for use with the
`option`, `option-list` and `option-flag` forms in the
:flyc:checkprop:`:command` of a syntax checker definition:

.. function:: flycheck-option-int value

   Like the built-in `number-to-string`, but returns `nil` when :var:`value` is
   `nil`.

.. function:: flycheck-option-comma-separated-list value &optional separator filter

   Convert :var:`value` into a list separated by :var:`separator`.
   :var:`filter` is an optional function to apply to each item in :var:`value`
   first.

.. _extending-syntax-checkers:

Extending syntax checkers
=========================

There are some means to extend defined syntax checkers:

.. function:: flycheck-add-next-checker checker next-checker &optional append

   Add a :var:`next-checker` to run after :var:`checker`.

   :var:`checker` is a syntax checker symbol.  :var:`next-checker` is either a
   syntax checker symbol or a cons cell in the format of a single entry to the
   :flyc:checkprop:`:next-checkers` property.

   :var:`next-checker` is prepended before other checkers to run after
   :var:`checker`, unless :var:`append` is non-nil.

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

First we specify the :flyc:checkprop:`:command` to execute.  The first element
of the command list is the name of the executable, `phpcs` in our case.  This
command is checked for existence with `executable-find` before using this syntax
checker.  If the command does not exist, the syntax checker is *not* used.

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

Again, there is a :flyc:checkprop:`:command`, however we use the `source-inplace`
symbol this time.  This symbol causes the temporary file to be created in the
same directory as the original file, making information from the source code
tree available to the syntax checker.  In case of Pylint, these are the Python
packages from the source code tree.

Next we give the list of :flyc:checkprop:`:error-patterns` to parse errors.
These patterns extract the error location and the error message from the output
of `epylint`.  An error pattern is a list containing a regular expression with
match groups to extract the error information, and an error level.

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
:flyc:checkprop:`:modes`, however it specifies an additional
:flyc:checkprop:`:predicate` to determine whether the right shell is in use.
Hence this syntax checker is only used if a Zsh shell scripting is being edited
in `sh-mode`, but not if a Bash or POSIX Shell script is being edited.

A syntax checker may even go as far as not having :flyc:checkprop:`:modes` at
all.  For instance, there is no special JSON mode, but syntax checking JSON is
still desirable.  Hence a JSON syntax checker may use the
:flyc:checkprop:`:predicate` to check the file extension:

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

In the :flyc:checkprop:`:command` we use a `config-file` form to pass the
configuration file to the syntax checker.  If the configuration file is found,
its path will be passed to the syntax checker, using the option specified after
the `config-file` symbol.  Otherwise the whole element is simply omitted from
the command line.

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
                (option "--standard=" flycheck-phpcs-standard)
                source)
     :error-parser 'flycheck-parse-checkstyle
     :modes 'php-mode)

The syntax checker is pretty much the same as before, except that a new element
was added to :flyc:checkprop:`:command`.  This element passes the value of the
new option variable :option:`flycheck-phpcs-standard` to the syntax checker.
This variable is declared with the special macro
:macro:`flycheck-def-option-var` at the beginning.

Chaining syntax checkers
------------------------

For many languages, more than a single syntax checker is applicable.  For
instance, Emacs Lisp can be checked for syntactic corrections with the byte code
compiler, and for adherence to the Emacs Lisp documentation style using
Checkdoc.  PHP, too, can be syntax checked with the PHP parser, and verified
against coding styles using PHP CodeSniffer.

To support such cases, syntax checkers can be :dfn:`chained` using the
:flyc:checkprop:`:next-checkers`.  The standard PHP syntax checker uses this to
run PHP CodeSniffer if there are no syntax errors:

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
     :next-checkers ((warnings-only . php-phpcs)))

Now PHP CodeSniffer will check the coding style, whenever a PHP syntax check did
not result in any errors, if PHP CodeSniffer syntax checker is usable *and*
registered.
