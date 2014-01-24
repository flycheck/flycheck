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

You define new syntax checkers with :el:macro:`flycheck-define-checker`:

.. el:macro:: flycheck-define-checker symbol docstring &rest properties

   Define `symbol` as new syntax checker with `docstring` and `properties`.

   `docstring` provides documentation for the syntax checker.  Use
   :el:command:`flycheck-describe-checker` to view the documentation of a syntax
   checker.

   The following `properties` constitute a syntax checker.
   :checkprop:`:command` is mandatory.  A syntax checker must also have either
   :checkprop:`:modes` or :checkprop:`:predicate`, and either
   :checkprop:`:error-patterns` or :checkprop:`:error-parser`.
   :checkprop:`:next-checkers` is entirely optional.

   If a mandatory property is missing, or if any property has an invalid value,
   a *compile-time* error is signalled.

   Any syntax checker defined with this macro is eligible for manual syntax
   checker selection with :el:command:`flycheck-select-checker`.  To make the
   new syntax checker available for automatic selection, you need to register it
   in :el:option:`flycheck-checkers`.  See :ref:`syntax-checker-selection` for
   more information.

   .. checker-property:: :command ({executable} [{arg} ...])

      An unquoted list describing the syntax checker command to execute.

      :samp:`{executable}` must be a string with the executable of this syntax
      checker.

      A customizable, buffer-local variable :samp:`flycheck-{symbol}-executable`
      is implicitly defined to allow overriding of the executable.  See
      :ref:`syntax-checker-executables` for details about these variables.  If
      this variable is non-nil, Flycheck uses the value of the variable as
      executable, otherwise it falls back to :samp:`{executable}`.  In either
      case, the executable is checked with `executable-find` before use.

      Each :samp:`{arg}` is an argument to the executable, either as string, or
      as one of the following special symbols and forms.

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

        See the declaration of the :checker:`elixir` syntax checker for an
        application of this symbol.

     `temporary-file-name`
        The path to a temporary file, which is unique on each execution of the
        syntax checker.  The file is @emph{not} created automatically.

        Use this symbol if you need to move files created by a syntax checker
        out of the way.

        See the declaration of the @code{go-build} syntax checker for an
        application of this symbol.

     :samp:`(config-file {option} {variable})`
        A configuration file for this syntax checker.

        :samp:`{option}` is a string containing the option that specifies a
        configuration file to the syntax checker tool.

        :samp:`{variable}` is a symbol referring to a variable from which to
        obtain the name or path of the configuration file.  See
        :ref:`syntax-checker-configuration-files` for more information about
        syntax checker configuration.  You need to define this variable
        *explicitly* with :el:macro:`flycheck-def-config-file-var`.

        If the configuration file is found, pass the :samp:`{option}` and the
        absolute path to the configuration file to the syntax checker.
        Otherwise the cell is simply dropped from the arguments of the syntax
        checker.

        If :samp:`{option}` ends with a `=` character, :samp:`{option}` and the
        absolute path to the configuration file are concatenated and given as a
        single argument to the syntax checker.  Otherwise, :samp:`{option}` and
        the configuration file path are given as two separate arguments.

     :samp:`(option {option} {variable} [{filter}])`
        The value of a variable.

        :samp:`{option}` is a string containing the option for with to specify
        the value.  :samp:`{filter}` is an optional function to be applied to
        the value of :samp:`{variable}` before use.  See :ref:`option-filters` for
        a list of built-in option filters.

        :samp:`{variable}` is a symbol referring to a variable whose value to use.
        :See ref:`syntax-checker-configuration` for more information about
        :syntax checker configuration.  You need to define this variable
        :*explicitly* with :el:macro:`flycheck-def-option-var`.

        If :samp:`{variable}` is not `nil` after applying :samp:`{filter}`, pass
        :samp:`{option}` and the value of :samp:`{variable}` after applying
        :samp:`{filter}`.  Otherwise the cell is simply dropped from the
        arguments of the syntax checker.

        An :samp:`{option}` ending with a `=` character is treated like in a
        `config-file` cell.

     :samp:`(option-list {option} {variable} [{prepend-fn} [{filter}]])`
        Like :samp:`{option}`, but for lists of options.

        :samp:`{option}` is a string containing the option to specify.

        :samp:`{variable}` is a variable whose value must be a list.

        :samp:`prepend-fn` is a function called with :samp:`{option}` as first
        and the item of the list as second argument.  It should return the
        result of prepending `{option}` to the item, either as list or as
        string.  If omitted, it defaults to `list`, so by default,
        :samp:`{option}` will be prepended as a separate argument.

        :samp:`{filter}` is an optional function to be applied to each item in
        the list before use.  See :ref:`option-filters` for a list of built-in
        option filters.

        For each item in the value of :samp:`{variable}`, which is not `nil`
        after applying :samp:`{filter}`, pass :samp:`{option}` the the item
        after applying :samp:`{filter}`, as returned by :samp:`{prepend-fn}`.

        Nil items are simply ignored.

     :samp:`(option-flag {option} {variable})`
        Like `option`, but for boolean flags.

        :samp:`{option}` is a string containing the option to
        specify. :samp:`{variable}` is a symbol referring to a variable.

        If :samp:`{variable}` is non-nil, pass :samp:`{option}` to the syntax
        checker.  Otherwise just ignore this argument.

     :samp:`(eval {form})`
        The result of evaluating `form`.

        `form` is an arbitrary Emacs Lisp form.  It is evaluated literally *each
        time* the syntax checker is executed.  Special forms and symbols in
        `form` are *not* replaced!

        `form` must evaluate either to a string, a list of strings or nil.  For
        any other result, signal an error.

        If the result of `form` is not nil, pass the result to the syntax
        checker *literally*.  Special symbols and forms in the result of `form`
        are *not* replaced.

   .. checker-property:: :error-patterns (({level} {sexp}) ...)

      An unquoted list of one or more error patterns to parse the output of the
      syntax checker :checkprop:`:command`.

      :samp:`{level}` is a Flycheck error level, and denotes the severity of
      errors matched by the pattern.  This mainly affects the visual
      representation of matched errors in buffers.

      Flycheck provides the built-in error levels `error`, `warning` and `info`.
      You can define your own error levels with
      :el:function:`flycheck-define-error-level`.

      The :samp:`{level}` is followed by one or more `rx` :samp:`{sexp}`
      elements.  See the docstring of the function `rx` for more information.
      In addition to the standard forms of `rx`, Flycheck supports the following
      additional forms to make specific parts of the error output available to
      Flycheck:

      `line`
         Matches the line number the error refers to, as a sequence of one or
         more digits.

      `column`
         Matches a column number the error refers to, as a sequence of one or
         more digits.

      :samp:`(file-name {sexp} ...)`
         Matches the file name the error refers to.  :samp:`{sexp}` matches the
         name of the file.  If no :samp:`{sexp}` is given, use a default body of
         `(minimal-match (one-or-more not-newline))`, which is equivalent to
         `".+?"`.

      :samp:`(message {sexp} ...)`
         Matches the error message to be shown to the user.  :samp:`{sexp}`
         matches the text of the message.  If no :samp:`{sexp}` is given, use a
         default body of `(one-or-more not-newline)`, which is equivalent to
         `".+"`.

      Each of these items is optional, however error messages without a `line`
      will be ignored and are not shown in the buffer.

      The patterns are applied in the order of declaration to the whole output
      of the syntax checker.  Output already matched by a pattern will not be
      matched by subsequent patterns.  In other words, the first pattern wins.

   .. checker-property:: :error-parser {function}
                         :error-parser (lambda (output checker buffer) {body} ...)

      A function to parse the output of the syntax checker, either as *unquoted*
      function symbol or `lambda` form.

      The function must accept three arguments `output`, `checker` and `buffer`,
      where `output` is the output of the syntax checker as string, `checker`
      the syntax checker that was used, and `buffer` a buffer object
      representing the checker buffer.

      The function must return a list of :el:cl-struct:`flycheck-error` objects
      parsed from `output`.  See :ref:`error-api` for information about
      :el:cl-struct:`flycheck-error`.  See :ref:`error-parsers` for a list of
      built-in error parsers.

      If this property is given, it takes precedence over
      :checkprop:`:error-patterns`.  To use an error parser together with
      patterns, you must manually call
      :el:function:`flycheck-parse-output-with-patterns` in your error parser to
      apply the error patterns.  You can then manipulate the
      :el:cl-struct:`flycheck-error` objects returned by this function.

   .. checker-property:: :modes {mode}
                         :modes ({mode} ...)

      An unquoted major mode symbol or an unquoted list thereof.

      If given, this syntax checker is only used, if the major mode of the
      buffer to check is equal (as in `eq`) to any given :samp:`{mode}`.

      If :checkprop:`:predicate` is given, it is additionally called in buffers
      of any given :samp:`{mode}`.

   .. checker-property:: :predicate {function}
                         :predicate (lambda () {body} ...)

      A function to determines whether to use this syntax checker in the current
      buffer, either as unquoted function symbol or as `lambda` form.  The
      syntax checker is only used if this function returns non-nil when called
      in the buffer to check.

      If :checkprop:`:modes` is given, the function is only called in matching
      major modes.  Thus, if :checkprop:`:modes` and :checkprop:`:predicate` are
      given, **both** must match for this syntax checker to be used.

   .. checker-property:: :next-checkers ({item} ...)

      An unquoted list defining the syntax checker to run after this checker.

      Flycheck tries all items in the order of declaration.  Each :samp:`{item}`
      is either a syntax checker symbol or a cons cell :samp:`({predicate}
      . {checker})`.

      In the former case, the :samp:`{item}` is used, if the syntax checker is
      enabled and suitable for the current buffer.  In the latter case, the
      :samp:`{predicate}` must match additionally.

      :samp:`{predicate}` is either `no-errors` or `warnings-only`:

      `no-errors`
         The syntax :samp:`{checker}` is only considered if the current syntax
         checker reported no errors at all.

      `warnings-only`
         The syntax :samp:`{checker}` is only considered if the current syntax
         checker only reported warnings, but no errors.

.. el:macro:: flycheck-def-config-file-var symbol checker &optional filename

   Define `symbol` as configuration file variable for a syntax `checker`, with a
   default value of `filename`.

   `symbol` is declared as customizable, buffer-local variable using
   `defcustom`, to provide a configuration file for the given syntax `checker`.
   The variable has the customization type `string`, and gets a comprehensive
   docstring, including a reference to `checker`.

   `filename` is used as initial value for the variable.  If omitted, the
   initial value is nil.

   Use this macro together with the `config-file` form in the
   :checkprop:`:command` of a syntax checker.

.. el:macro:: flycheck-def-option-var symbol initial-value checker &optional custom-args

   Define `symbol` as option variable for a syntax `checker`, with the given
   `initial-value`.

   `symbol` is declared as customizable variable, buffer-local variable using
   `defcustom`, to provide an option for the given syntax `checker`.  `symbol`
   gets a comprehensive docstring, including a reference to `checker`.

   `custom-args` are forwarded to `defcustom`.  Use them to declare the
   customization type, etc.

   Use this macro together with the `option`, `option-list` and `option-flag`
   forms in the :checkprop:`:command` of a syntax checker.
