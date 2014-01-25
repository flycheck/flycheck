==============
 Flycheck API
==============

This chapter provides a brief overview over the Flycheck API.

You may use this API to extend Flycheck, e.g. by implementing new error parsers
or more in-depth error analysis.  You will also find this API helpful if you
want to develop Flycheck itself.

The documentation provided in this chapter is intended as brief overview.  For a
definite reference on functions and variables, please consult the excellent
online help system of Emcas, via `describe-function` and `describe-variable`
respectively.

.. _error-api:

Error API
=========

Flycheck errors are represented by the CL structure
:el:cl-struct:`flycheck-error`.  @xref{Structures, , , cl}, for information
about CL structures.

.. el:cl-struct:: flycheck-error

   A Flycheck error with the following slots.  Each of these slots may be `nil`.

   .. el:cl-slot:: buffer

      The buffer object referring to the buffer this error belongs to.

      .. note::

         You do not need to set this attribute when creating errors in an error
         parser.  Flycheck automatically keeps track of the buffer itself.

   .. el:cl-slot:: checker

      The syntax checker that reported this error.

   .. el:cl-slot:: filename

      A string containing the filename the error refers to.

   .. el:cl-slot:: line

      An integer providing the line the error refers to.

   .. el:cl-slot:: column

      An integer providing the column the error refers to.

      If this attribute is `nil`, Flycheck will assume that the error refers to
      the whole line.

   .. el:cl-slot:: message

      The human-readable error message as string.

   .. el:cl-slot:: level

      The error level of the message, as symbol denoting an error level
      defined with :el:function:`flycheck-define-error-level`.

   There are two constructors to create new :el:cl-struct:`flycheck-error` objects:

   .. el:function:: flycheck-error-new-at line column &optional level message &key \
                    checker filename buffer

      Create a new Flycheck error at the given `line` and `column`.

      `line` and `column` refer to the :el:cl-slot:`line` and
      :el:cl-slot:`column` of the new error.  The optional `level` and `message`
      arguments fill the :el:cl-slot:`level` and :el:cl-slot:`message` slots
      respectively.

      `checker`, `filename` and `buffer` are keyword arguments, for
      :el:cl-slot:`checker`, :el:cl-slot:`filename` and :el:cl-slot:`buffer`
      respectively.  `buffer` defaults to the current buffer, the other two
      default to `nil`.

      .. warning::

         Due to a limitation of Common Lisp functions in Emacs Lisp, you must
         specify **all** optional arguments, that is, **both** `level` **and**
         `message`, to pass any keyword arguments.

   .. el:function:: flycheck-error-new &rest attributes

      Create a new :el:cl-struct:`flycheck-error` with the given `attributes`.

      `attributes` is a property list, where each property specifies the value
      for the corresponding slot of :el:cl-struct:`flycheck-error`, for
      instance:

      .. code-block:: cl

         (flycheck-error-new :line 10 :column 5 :message "Foo" :level 'warning)

   The following functions and macros work on errors:

   .. el:macro:: flycheck-error-with-buffer error &rest forms

      Evaluate `forms` with the :el:cl-slot:`buffer` of the given `error` as the
      current buffer.

      If the buffer is not live, `forms` are **not** evaluated.

   .. el:function:: flycheck-error-line-region error

      Get the region marking the whole :el:cl-slot:`line` of the given `error`, as
      a cons cell :samp:`({beg} . {end})`.

   .. el:function:: flycheck-error-column-region error

      Get the region marking the :el:cl-slot:`column` of the given `error`, as
      cons cell :samp:`({beg} . {end})`.

      If the :el:cl-slot:`column` of `error` is `nil`, return `nil` instead.

   .. el:function:: flycheck-error-sexp-region error

      Get the region marking the expression around the :el:cl-slot:`column` of
      the `error`, as cons cell :samp:`({beg} . {end})`.

      If the :el:cl-slot:`column` of `error` is `nil`, or if there is no
      expression around this column, return `nil` instead.

      .. warning::

         Flycheck relies on the major mode to parse the expression around the
         column.  As such, the major mode must implement support for this feature.

         Some major modes, especially from 3rd party packages, do not support
         this feature at all, or only very poorly.  Others (for instance
         `python-mode`) exhibit serious performance drops in some cases.

         Use this function with care, and at best only in major modes which are
         known to have good support for this feature.

   .. el:function:: flycheck-error-pos error

      Get the exact buffer position of `error`.

      Essentially this is the position of the :el:cl-slot:`column` of `error`,
      if it is not `nil`, or the position of the first non-whitespace character
      on the :el:cl-slot:`line` of `error` otherwise.
      @end defun

   .. el:function:: flycheck-error-format error

      Format `error` as a human-readable string.

The following functions and variables may be used to analyze the errors of a
syntax check.

.. el:variable:: flycheck-current-errors

   This buffer-local variable stores the errors of the last syntax check,
   sorted by line and column number.

.. el:function:: flycheck-count-errors errors

   Count the number of errors and warnings in `errors`.

   Return a cons cell :samp:`({no-errors} . {no-warnings})`.

.. el:function:: flycheck-has-errors-p errors &optional level

   Determine if there are any `errors` with the given `level`.

   If `level` is omitted, determine whether `errors` is not nil.  Otherwise
   determine whether there are any errors whose level is equal to the given
   `level`.


.. _error-parser-api:

Error parser API
================

These functions can be used to implement custom error parsers:

.. el:function:: flycheck-parse-xml-string s

   Parse a string `s` containing XML and return the parsed document tree.
