.. _flycheck-developers-guide:

=================
Developer's Guide
=================

So you want to extend Flycheck, but have no idea where to start?  This guide
will give you an overview of Flycheck internals, and take you through adding a
syntax checker to Flycheck.

An overview of Flycheck internals
=================================

The goal of Flycheck is to display errors from external checker programs
directly in the buffer you are editing.  Instead of you manually invoking
``make`` or the compiler for your favorite language, Flycheck takes care of it
for you, collects the errors and displays them right there in the buffer.

How Flycheck works is rather straightforward.  Whenever a syntax check is
started (see :ref:`flycheck-syntax-checks`), the following happens:

1. First, Flycheck runs the external program as an asynchronous process using
   ``start-process``.  While this process runs, Flycheck simply accumulates its
   output.
2. When the process exits, Flycheck parses its output in order to collect the
   errors.  The raw output is turned into a list of `flycheck-error` objects
   containing, among others, the filename, line, column, message and severity of
   the error.
3. Flycheck then filters the collected errors to keep only the relevant ones.
   For instance, errors directed at other files than the one you are editing are
   discarded.
4. Relevant errors are highlighted by Flycheck in the buffer, according to user
   preference.  By default, each error adds a mark in the fringe at the line it
   occurs, and underlines the symbol at the position of the error using
   *overlays*.
5. Finally, Flycheck rebuilds the error list buffer.

Flycheck follows this process for all the :ref:`many different syntax checkers
<flycheck-languages>` that are provided by default.

.. note::

   Specifically, the above describes the process of *command checkers*, i.e.,
   checkers that run external programs.  All the checkers defined in
   ``flycheck-checkers`` are command checkers, but command checkers are actually
   instances of *generic checkers*.  See :flyc:`flycheck-ocaml` for an example
   of how to use a generic checker.

.. seealso::

   :infonode:`(elisp)Asynchronous Processes`
      How to run and control asynchronous processes from inside Emacs.

   :infonode:`(elisp)Overlays`
      How to add temporary annotations to a buffer.

Adding a syntax checker to Flycheck
===================================

To add a syntax checker to Flycheck, you need to answer a few questions:

- How to invoke the checker?  What is the name of its program, and what
  arguments should Flycheck pass to it?
- How to parse the error messages from the checker output?
- What language (or languages) will the checker be used for?

For instance, if I were to manually run the Scala compiler ``scalac`` on the
following ``hello.scala`` file:

.. code-block:: scala

   object {
     println("Hello, world")
   }

Here is the output I would get:

.. code-block:: console

   $ scalac hello.scala
   hello.scala:1: error: identifier expected but '{' found.
   object {
          ^
   one error found


The compiler reports one syntax error from the file ``hello.scala``, on line 3,
with severity ``error``, and the rest of the line contains the error message.

So, if we want to instruct Flycheck to run ``scalac`` on our Scala files, we
need to tell Flycheck to:

- Invoke ``scalac FILE-NAME``
- Get errors from output lines of the form: ``file-name:line: error:message``

Writing the checker
-------------------

Once you have answered these questions, you merely have to translate the answers
to Emacs Lisp.  Here is the full definition of the ``scala`` checker you can
find in ``flycheck.el``:

.. code-block:: elisp

   (flycheck-define-checker scala
     "A Scala syntax checker using the Scala compiler.

   See URL `http://www.scala-lang.org/'."
     :command ("scalac" "-Ystop-after:parser" source)
     :error-patterns
       ((error line-start (file-name) ":" line ": error: " (message) line-end))
     :modes scala-mode
     :next-checkers ((warning . scala-scalastyle)))

The code is rather self-explanatory; but we'll go through it nonetheless.

First, we define a checker using `flycheck-define-checker`.  Its first argument,
``scala``, is the name of the checker, as a symbol.  The name is used to refer
to the checker in the documentation, so it should usually be the name of the
language to check, or the name of the program used to do the checking, or a
combination of both.  Here, ``scalac`` is the program, but the checker is named
``scala``.  There is another Scala checker using ``scalastyle``, with the name
``scala-scalastyle``.  See `flycheck-checkers` for the full list of checker
names defined in Flycheck.

After the name comes the docstring.  This is a documentation string answering
three questions: 1) What language is this checker for?  2) What is the program
used? 3) Where can users get this program?  Nothing more.  In particular, this
string does *not* include user documentation, which should rather go in the
manual (see :ref:`flycheck-languages`).

The rest of the arguments are keyword arguments; their order does not matter,
but they are usually given in the fashion above.

- ``:command`` describes what command to run, and what arguments to pass.  Here,
  we tell Flycheck to run ``scalac -Ystop-after:parser`` on ``source``.  In
  Flycheck, we usually want to get error feedback as fast as possible, hence we
  will pass any flag that will speed up the invocation of a compiler, even at
  the cost of missing out on some errors.  Here, we are telling ``scalac`` to
  stop after the parsing phase to ensure we are getting syntax errors quickly.

  The ``source`` argument is special: it instructs Flycheck to create a
  temporary file containing the content of the current buffer, and to pass that
  temporary file as argument to ``scalac``.  That way, ``scalac`` can be run on
  the content of the buffer, even when the buffer has not been saved.  There are
  other ways to pass the content of the buffer to the command, e.g., by piping
  it through standard input.  These special arguments are described in the
  docstring of `flycheck-substitute-argument`.

- ``:error-patterns`` describes how to parse the output, using the `rx` regular
  expression syntax.  Here, we expect ``scalac`` to return error messages of the
  form::

    file:line: error: message

  This is a common output format for compilers.  With the following
  ``:error-patterns`` value:

  .. code-block:: elisp

    ((error line-start (file-name) ":" line ": error: " (message) line-end))

  we tell Flycheck to extract three parts from each line in the output that
  matches the pattern: the ``file-name``, the ``line`` number, and the
  ``message`` content.  These three parts are then used by Flycheck to create a
  `flycheck-error` with the ``error`` severity.

- ``:modes`` is the list of Emacs major modes in which this checker can run.
  Here, we want the checker to run only in ``scala-mode`` buffers.

That's it!  This definition alone contains everything Flycheck needs to run
``scalac`` on a Scala buffer and parse its output in order to give error
feedback to the user.

.. note::

   ``rx.el`` is a built-in Emacs module for declarative regular expressions.
   Look for the documentation of the `rx` function inside Emacs for its usage.
   Flycheck extends `rx` with a few constructs like ``line``, ``file-name`` and
   ``message``.  You can find them the full list in the docstring for
   `flycheck-rx-to-string`.

Registering the checker
-----------------------

Usually, you'll want to register the checker so that it is eligible for
automatic selection.  For that, you just need to add the checker symbol to
`flycheck-checkers`.  The order of checkers does matter, as only one checker can
be enabled in a buffer at a time.  Usually you want to put the most useful
checker as the first checker for that mode.  For instance, here are the
JavaScript checkers provided by Flycheck:

.. code-block:: console

   javascript-eslint
   javascript-jshint
   javascript-gjslint
   javascript-jscs
   javascript-standard

If a buffer is in ``js-mode``, Flycheck will try first to enable
``javascript-eslint`` before any other JavaScript checker.

There are other factors governing checker selection in a buffer, namely whether
a checker is disabled by user configuration (see
:ref:`flycheck-disable-checkers`), and whether this checker *can* be enabled
(see the ``:enabled`` property in `flycheck-define-generic-checker`).

.. seealso::

   flycheck-get-checker-for-buffer
     This is the function that looks through `flycheck-checkers` to find a
     valid checker for the buffer.

A more complex example
----------------------

Here is a slightly more complex checker:

.. code-block:: elisp

   (flycheck-define-checker protobuf-protoc
     "A protobuf syntax checker using the protoc compiler.

   See URL `https://developers.google.com/protocol-buffers/'."
     :command ("protoc" "--error_format" "gcc"
               (eval (concat "--java_out=" (flycheck-temp-dir-system)))
               ;; Add the file directory of protobuf path to resolve import directives
               (eval (concat "--proto_path=" (file-name-directory (buffer-file-name))))
               source-inplace)
     :error-patterns
     ((info line-start (file-name) ":" line ":" column
            ": note: " (message) line-end)
      (error line-start (file-name) ":" line ":" column
             ": " (message) line-end)
      (error line-start
             (message "In file included from") " " (file-name) ":" line ":"
             column ":" line-end))
     :modes protobuf-mode
     :predicate (lambda () (buffer-file-name)))

The ``:command`` is longer, as the checker passes more flags to ``protoc``.
Note the use of ``eval`` for transforming Flycheck checker options into flags
for the command.  See the docstring for `flycheck-substitute-argument` for more
info, and look at other checkers for examples.

Note also that there are three patterns in ``:error-patterns``; the first one
will catch ``notes`` from the compiler and turn them into `flycheck-error`
objects with the ``info`` severity; the second is for errors from the file being
checked, and the third one is for errors from other files.

There is a new ``:predicate`` property, that is used to determine when the
checker can be called.  In addition to the ``:mode`` property which restricts
the checker to buffer in the ``protobuf-mode``, this checker should be called
only when there is a file associated to the buffer.  This is necessary since we
are passing the file associated to the buffer ``protobuf`` using
``source-inplace`` in ``:command``.

There are other useful properties, depending on your situation.  ``:enabled`` is
like ``:predicate``, but is run only once; it is used to make sure a checker has
everything it needs before being allowed to run in a buffer.  ``:verify`` is
helpful for giving feedback to users.  ``:error-parser`` replaces
``:error-patterns`` and is for parsing checker output from machine-readable
formats like XML or JSON.

.. seealso::

   flycheck-define-generic-checker
     For the full documentation of all the properties you can pass to
     `flycheck-define-checker`.  Look also in the docstring for
     `flycheck-define-command-checker` for additional properties.

.. note::

   Don't be afraid to look into the ``flycheck.el`` code.  The existing checkers
   serve as useful examples you can draw from, and most of core functions are
   well documented.

Sharing your checker
--------------------

Once you have written your own syntax checker, why not `submit a pull request
<https://github.com/flycheck/flycheck/pulls>`__ to integrate it into Flycheck?
If it's useful to you, it may be useful for someone else!  Please do check out
our :ref:`flycheck-contributors-guide` to learn how we deal with pull requests.
