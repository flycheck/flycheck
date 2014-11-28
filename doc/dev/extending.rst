====================
 Extending Flycheck
====================

.. require:: flycheck

This chapter explains how to add new syntax checkers and how to extend built-in
syntax checkers, by example.  For a detailed reference on the involved types,
functions and macros, please refer to the :ref:`flycheck-api`.

.. note::

   If you define a new syntax checker or have an extension to a built-in syntax
   checker, please report it to Flycheck (see :ref:`reporting-issues`), so that
   we can consider it for inclusion to make it available to all other users of
   Flycheck.

   If you would like to contribute your extension or your new syntax checker to
   Flycheck as a patch or pull request, please see :ref:`contributing-code` and
   :ref:`contributing-syntax-checkers`.

.. contents:: Contents
   :local:

.. _defining-new-syntax-checkers:

Defining new syntax checkers
============================

Flycheck provides the macro :macro:`flycheck-define-checker` to define a new
syntax checker.  The following example defines a simple syntax checker for the
popular Pylint_ tool for Python:

.. code-block:: cl

   (flycheck-define-checker python-pylint
     "A Python syntax and style checker using Pylint.

   See URL `http://www.pylint.org/'."
     :command ("pylint"
               "--msg-template" "{path}:{line}:{column}:{C}:{msg} ({msg_id})"
               source)
     :error-patterns
     ((error line-start (file-name) ":" line ":" column ":"
             (or "E" "F") ":" (message) line-end)
      (warning line-start (file-name) ":" line ":" column ":"
               (or "W" "R") ":" (message) line-end)
      (info line-start (file-name) ":" line ":" column ":"
            "C:" (message) line-end))
     :modes python-mode)

The first argument to :macro:`flycheck-define-checker` is the *name* of a syntax
checker, by which we can refer to this particular syntax checker.

Next comes the *docstring*, which should provide a bit of information about the
syntax checker.  It's a good idea to provide a link to the homepage of the
syntax checker tool here.  You can view this docstring in Emacs with
:command:`flycheck-describe-checker` or :kbd:`C-c ! ?`, e.g. :kbd:`C-c ! ?
texinfo`.

Eventually we specify the *properties* of the new syntax checker.  These
properties tell Flycheck when to use your new syntax checker, how to run it, and
how to parse its output:

- The `:command` specifies the command Flycheck should run to check the buffer.
  It's a simple list containing the executable and its arguments.

  In our example we first the `--msg-template` option to Pylint to configure a
  comprehensive and parseable output format.

  Then we use the “special” `source` argument to pass the contents of the buffer
  as input file to `pylint`.  Whenever it sees the `source` argument, Flycheck
  creates a temporary file, fills it with the current contents of the buffer and
  passes that file to the syntax checker.  This allows to check the real
  contents of a buffer even if the buffer is not saved to disk.

- The `:error-patterns` tell Flycheck how to parse the output of the command in
  order to obtain error locations.  Each pattern has a *level*, followed by `rx`
  forms which specify a regular expression to find an error in the output of the
  command.

  Flycheck understands three error levels by default:  `error` is for critical
  errors that absolutely require the user's attention (e.g. syntax errors),
  `warning` is for issues that can be ignored, but should not (e.g. unused
  variables), and `info` is for other messages that provide information about
  the buffer, but do not immediately require action from the user.

  .. seealso:: :function:`flycheck-define-error-level`; to define custom error
               levels

  Flycheck provides special `rx` forms to extract the relevant information from
  each error:

  - The `(file-name)` and `(message)` forms match a sequence of any character
    save new line as file name and message of the error.  Both optionally accept
    further `rx` forms, to specify an alternative regular expression to match
    the file name or the message, for instance to parse multi-line error
    messages.
  - The `line` and `column` forms match a sequence of one or more digits as line
    and column respectively of the error.

  .. seealso:: :function:`flycheck-rx-to-string`; for a list of all special `rx`
               forms provided by Flycheck and their reference

- The `:modes` property denotes the major modes, in which Flycheck may use this
  syntax checker.  JSHint checks Javascript, so the `:modes` of our example
  specifies Emacs' builtin Javascript Mode, and the popular 3rd-party JS2 Mode.

.. seealso:: :function:`flycheck-substitute-argument`; for a complete list of
             all special arguments

.. _Pylint: http://www.pylint.org/

Finding the right error patterns
--------------------------------

Finding the right error patterns is the hardest part of a syntax checker
definition.  For a first version, you just run the tool on a file, look at its
output and write a regular expression to match it.  `M-x shell` comes handy
here.

However, as you start to debug and refine your patterns, this quickly becomes
cumbersome.  Flycheck provides an easier way to test a syntax checker: Evaluate
the syntax checker definition with :kbd:`C-M-x` and run
:command:`flycheck-compile`.

.. command:: flycheck-compile
   :binding: C-c ! C-c

   Run a syntax checker on the current buffer in a fresh Compilation Mode
   buffer.  Prompt for a syntax checker to run.

This command runs the command like a normal syntax check would do, but instead
of highlighting errors within the buffer it shows a new buffer in Compilation
Mode, which contains the entire output of the command and highlights everything
that matches a pattern.

Sometimes however an output format doesn't lend itself to error patterns.  In
this case, you need to write a more sophisticated parser yourself.  See
:ref:`parsing-structured-output-format` for more information.

Trying a new syntax checker
---------------------------

After evaluating a syntax checker definition you can try whether it works for
normal syntax checks by selecting it manually with :kbd:`C-c ! s`
(:command:`flycheck-select-checker`).  If anything breaks, you can unselect the
syntax checker again with :kbd:`C-u C-c ! s` and fix the error without further
affecting Flycheck.

Once you have confirmed that your new syntax checker works flawlessly, you can
make it available for automatic syntax checking by registering it.

.. _registering-new-syntax-checkers:

Registering new syntax checkers
-------------------------------

To register a new syntax checker for automatic syntax checking, just add it to
:option:`flycheck-checkers`:

.. code-block:: cl

   (add-to-list 'flycheck-checkers 'flycheck-jshint)

Flycheck will try all syntax checkers in this variable when checking a buffer
automatically, and check the buffer with the first syntax checker in this list
whose `:modes` contains the current major mode.

.. note::

   Do **not** use :option:`flycheck-checker` and
   :command:`flycheck-select-checker` to enable your own syntax checker in
   Flycheck extensions.  They are reserved for **user customization**.

   Specifically, please do **not** provide a hook function which selects the
   syntax checker explicitly by assigning to :option:`flycheck-checker` or by
   calling :function:`flycheck-select-checker`.  In other words, this is
   **bad**:

   .. code-block:: cl

      (defun enable-my-new-syntax-checker ()
        (setq flycheck-checker 'my-new-syntax-checker)
        (flycheck-buffer))

      (add-hook 'my-major-mode-hook #'enable-my-new-syntax-checker)

   This circumvents the entire automatic selection of Flycheck, and prevents the
   user from effectively customizing Flycheck.

   Instead, just register your syntax checker in :option:`flycheck-checkers` and
   let Flycheck automatically pick the best syntax checker.  In other words,
   this is **good**:

   .. code-block:: cl

      (add-to-list 'flycheck-checkers 'my-new-syntax-checker)

Advanced syntax checker definitions
===================================

.. _parsing-structured-output-format:

Parsing structured output format
--------------------------------

If your syntax checker tool offers some structured output format as alternative
to human-readable free text, you can use an `:error-parser` function instead of
writing an error pattern.  For instance, JSHint_ offers the widely spread
Checkstyle XML output format which Flycheck supports out of the box:

.. code-block:: cl

   (flycheck-define-checker javascript-jshint
     "A JavaScript syntax and style checker using jshint.

   See URL `http://www.jshint.com'."
     :command ("jshint" "--checkstyle-reporter" source)
     :error-parser flycheck-parse-checkstyle
     :modes (js-mode js2-mode js3-mode))

As you can see, there are no patterns in this definition.  Instead Flycheck
calls the function :function:`flycheck-parse-checkstyle` to parse the output.
This function parses the XML to extract the errors.  It's built-in into
Flycheck, so if your tool supports Checkstyle XML, error parsing comes **for
free** in Flycheck.

.. seealso:: :ref:`api-error-parsers`; for more information about error parsers

.. _JSHint: http://www.jshint.com/

Passing options and configuration files to syntax checkers
----------------------------------------------------------

Many linting tools provide a rich set of options to configure their analysis.
Flycheck makes it to define proper Emacs options and map them to options of
commands.

For instance, the Rubocop_ tool checks Ruby for semantic and stylistic issues.
Since style is mainly a matter of taste, it has a special linting mode in which
all stylistic checks are disabled (error patterns omitted for readability):

.. code-block:: cl

   (flycheck-define-checker ruby-rubocop
     "A Ruby syntax and style checker using the RuboCop tool.

   See URL `http://batsov.com/rubocop/'."
     :command ("rubocop" "--format" "emacs"
               (option-flag "--lint" flycheck-rubocop-lint-only)
               source)
     :error-patterns ...
     :modes (ruby-mode))

Note the special `option-flag` argument, which splices the value of the boolean
Emacs option `flycheck-rubocop-lint-only` into the command: If the variable is
non-nil, Flycheck adds the `--lint` option to the final command line, other
Flycheck omits the entire argument.

Flycheck also supports other special `option-` arguments for plain values or
lists of values.

.. seealso:: flycheck-substitute-argument; for a list of all special `option-`
             arguments

Flycheck also provides a convenience macro :macro:`flycheck-def-option-var` to
declare these options:

.. code-block:: cl

   (flycheck-def-option-var flycheck-rubocop-lint-only nil ruby-rubocop
     "Whether to only report code issues in Rubocop.

   When non-nil, only report code issues in Rubocop, via `--lint'.
   Otherwise report style issues as well."
     :safe #'booleanp
     :type 'boolean)

Essentially, this macro is just a wrapper around the built-in `defcustom`, which
additionally keeps track of the syntax checker the option belongs to, and adds
the option to the appropriate custom group.  You can pass arbitrary custom
keywords to this macro as we did in this example: `:type` marks this option as
boolean flag, and `:safe` allows the use as file-local variable, if the value is
boolean.

By a similar mechanism you can also pass paths to configuration files to a
syntax checker tool.  The aforementioned `Pylint`_ reads a configuration file
for instance:

.. code-block:: cl

   (flycheck-define-checker python-pylint
     "A Python syntax and style checker using Pylint.

   This syntax checker requires Pylint 1.0 or newer.

   See URL `http://www.pylint.org/'."
     ;; -r n disables the scoring report
     :command ("pylint" "-r" "n"
               "--msg-template" "{path}:{line}:{column}:{C}:{msg} ({msg_id})"
               (config-file "--rcfile" flycheck-pylintrc)
               source)
     :error-patterns ...
     :modes python-mode)

The special `config-file` argument passes a configuration file from
`flycheck-pylintrc` to `pylint`, if the value of the variable is non-nil.

Flycheck provides a sophisticated logic to find an appropriate configuration
file.  See :ref:`syntax-checker-configuration-files` and
:ref:`api-configuration-files` for details.

.. _rubocop: https://github.com/bbatsov/rubocop

Controlling the use of a syntax checker
---------------------------------------

If you need more control about when a syntax checker is used for syntax
checking, you can supply a custom `:predicate` function.  Consider the following
syntax checker for Zsh scripts in Sh Mode:

.. code-block:: cl

   (flycheck-define-checker sh-zsh
     "A Zsh syntax checker using the Zsh shell.

   See URL `http://www.zsh.org/'."
     :command ("zsh" "-n" "-d" "-f" source)
     :error-patterns
     ((error line-start (file-name) ":" line ": " (message) line-end))
     :modes sh-mode
     :predicate (lambda () (eq sh-shell 'zsh)))

Sh Mode also supports Bash and other shells besides Zsh, so we additionally
provide a `:predicate` that checks whether the current buffer has the right
shell.

You can even omit `:modes` and only use a predicate to determine whether a
syntax checker is applicable for the current buffer.

Applying more than one syntax checker
-------------------------------------

Frequently, we would like to use multiple syntax checkers in a buffer.  For
instance, we might want to check the syntax of a script with `sh-zsh` from the
previous section, and then use Shellcheck_ to check for questionable code such
as unquoted variable expansions, if there are no syntax errors.  Flycheck
supports this scenario by *chaining* syntax checkers.

Suppose we defined a syntax checker for Shellcheck called `sh-shellcheck` as
follows:

.. code-block:: cl

   (flycheck-define-checker sh-shellcheck
     "A shell script syntax and style checker using Shellcheck.

   See URL `https://github.com/koalaman/shellcheck/'."
     :command ("shellcheck" "-f" "checkstyle"
               "-s" (eval (symbol-name sh-shell))
               source)
     :modes sh-mode
     :error-parser flycheck-parse-checkstyle)

.. note::

   Note how we use the special `eval` argument to put the result of an arbitrary
   Emacs Lisp expression into the command line of `shellcheck`, in order to tell
   Shellcheck what shell the script is written for.

We can now arrange for this syntax checker to be used after `sh-zsh` with
:function:`flycheck-add-next-checker`:

.. code-block:: cl

   (flycheck-add-next-checker 'sh-zsh '(warning . sh-shellcheck))

The first item of the cons cell in the second argument is the *maximum error
level* in the buffer, for which `sh-shellcheck` is still applicable.  With
`warning` Flycheck will run `sh-shellcheck` after `sh-zsh` if there are
`warning` or `info` level errors from `sh-zsh`, but not if there are any errors
with level `error`, such as syntax errors.

Flycheck will only use a chained syntax checker if it is registered in
:option:`flycheck-checkers`, so we need to :ref:`register our new syntax checker
<registering-new-syntax-checkers>`:

.. code-block:: cl

   (add-to-list 'flycheck-checkers 'sh-shellcheck 'append)

Note that unlike before we **append** the new syntax checker at the end of
`flycheck-checkers`.  This ensures that Flycheck does not try `sh-shellcheck`
*before* `sh-zsh`.

.. warning::

   Make sure to append chained syntax checkers to :option:`flycheck-checkers`.

   Flycheck tries all syntax checkers in this list in **order of appearance**,
   so if you add your new chained syntax checker at the beginning, it will
   likely be used right away, before any prior syntax checkers.

You also can specify chained syntax checkers directly in
:macro:`flycheck-define-checker` with the `:next-checkers` property.  Instead of
calling :function:`flycheck-add-next-checker`, we could also have added this
property to the definition of `sh-zsh`:

.. code-block:: cl

   (flycheck-define-checker sh-zsh
     "A Zsh syntax checker using the Zsh shell.

   See URL `http://www.zsh.org/'."
     :command ("zsh" "-n" "-d" "-f" source)
     :error-patterns ...
     :modes sh-mode
     :predicate (lambda () (eq sh-shell 'zsh))
     :next-checkers ((warning . sh-shellcheck)))

.. note::

   If you control the definition of both syntax checkers, this style is
   **preferable** to :function:`flycheck-add-next-checker`.  Use this function
   only if you cannot change the definition of the prior syntax checker.

.. _Shellcheck: https://github.com/koalaman/shellcheck/

Other ways to extend Flycheck
=============================

Use arbitrary functions to check buffers
----------------------------------------

Beyond commands, Flycheck also supports arbitrary functions as syntax checkers
with :function:`flycheck-define-generic-checker`.

Hooking into Flycheck
---------------------

Flycheck has a rich hook interface which you can use for your own extensions.

Status changes
~~~~~~~~~~~~~~

:hook:`flycheck-before-syntax-check-hook` and `flycheck-after-syntax-check-hook`
run before and after syntax checks, and let you update your Emacs instance
according to Flycheck's state.  For instance, flycheck-color-mode-line_ uses
these hooks to colour your mode-line according to the result of the last syntax
check.  Additionally, :hook:`flycheck-status-changed-functions` runs on every
single status change of Flycheck, and provides a fine-grained reporting about
what Flycheck is currently doing.

Error processing
~~~~~~~~~~~~~~~~

The functions in :hook:`flycheck-process-error-functions` are used to process
new errors reported by a Flycheck syntax checker.  Add to this hook to get
informed about each error reported in a Flycheck buffer.  In fact, Flycheck uses
this hook itself: The standard value :function:`flycheck-add-overlay` is
responsible for adding error highlighting to the buffer.  As a consequence, you
can **entirely opt out** from highlighting with a custom hook.

Error display
~~~~~~~~~~~~~

The function :hook:`flycheck-display-errors-function` is called to display an
error at point.  The `flycheck-pos-tip`_ extension uses this hook to show errors
in a GUI popup like conventional IDEs do.

.. _flycheck-color-mode-line: https://github.com/flycheck/flycheck-color-mode-line
.. _flycheck-pos-tip: https://github.com/flycheck/flycheck-pos-tip
