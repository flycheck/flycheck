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

Flycheck errors are represented by the CL structure :cl-struct:`flycheck-error`.
See :infonode:`(cl)Structures` for more information about CL structures.

.. cl-struct:: flycheck-error

   A Flycheck error with the following slots.  Each of these slots may be `nil`.

   .. cl-slot:: buffer

      The buffer object referring to the buffer this error belongs to.

      .. note::

         You do not need to set this attribute when creating errors in an error
         parser.  Flycheck automatically keeps track of the buffer itself.

   .. cl-slot:: checker

      The syntax checker that reported this error.

   .. cl-slot:: filename

      A string containing the filename the error refers to.

   .. cl-slot:: line

      An integer providing the line the error refers to.

   .. cl-slot:: column

      An integer providing the column the error refers to.

      If this attribute is `nil`, Flycheck will assume that the error refers to
      the whole line.

   .. cl-slot:: message

      The human-readable error message as string.

   .. cl-slot:: level

      The error level of the message, as symbol denoting an error level defined
      with :function:`flycheck-define-error-level`.

   There are two constructors to create new :cl-struct:`flycheck-error` objects:

   .. function:: flycheck-error-new-at line column &optional level message &key \
                    checker filename buffer

      Create a new Flycheck error at the given :var:`line` and :var:`column`.

      :var:`line` and :var:`column` refer to the :cl-slot:`line` and
      :cl-slot:`column` of the new error.  The optional :var:`level` and
      :var:`message` arguments fill the :cl-slot:`level` and cl-slot:`message`
      slots respectively.

      :var:`checker`, :var:`filename` and :var:`buffer` are keyword arguments,
      for :cl-slot:`checker`, :cl-slot:`filename` and :cl-slot:`buffer`
      respectively.  :var:`buffer` defaults to the current buffer, the other two
      default to `nil`.

      .. warning::

         Due to a limitation of Common Lisp functions in Emacs Lisp, you must
         specify **all** optional arguments, that is, **both** :var:`level`
         **and** :var:`message`, to pass any keyword arguments.

   .. function:: flycheck-error-new &rest attributes

      Create a new :cl-struct:`flycheck-error` with the given :var:`attributes`.

      :var:`attributes` is a property list, where each property specifies the
      value for the corresponding slot of :cl-struct:`flycheck-error`, for
      instance:

      .. code-block:: cl

         (flycheck-error-new :line 10 :column 5 :message "Foo" :level 'warning)

   The following functions and macros work on errors:

   .. macro:: flycheck-error-with-buffer error &rest forms

      Evaluate :var:`forms` with the :cl-slot:`buffer` of the given :var:`error`
      as the current buffer.

      If the buffer is not live, :var:`forms` are **not** evaluated.

   .. function:: flycheck-error-line-region error

      Get the region marking the whole :cl-slot:`line` of the given
      :var:`error`, as a cons cell :varcode:`({beg} . {end})`.

   .. function:: flycheck-error-column-region error

      Get the region marking the :cl-slot:`column` of the given :var:`error`, as
      cons cell :varcode:`({beg} . {end})`.

      If the :cl-slot:`column` of :var:`error` is `nil`, return `nil` instead.

   .. function:: flycheck-error-sexp-region error

      Get the region marking the expression around the :cl-slot:`column` of the
      :var:`error`, as cons cell :varcode:`({beg} . {end})`.

      If the :cl-slot:`column` of :var:`error` is `nil`, or if there is no
      expression around this column, return `nil` instead.

      .. warning::

         Flycheck relies on the major mode to parse the expression around the
         column.  As such, the major mode must implement support for this feature.

         Some major modes, especially from 3rd party packages, do not support
         this feature at all, or only very poorly.  Others (for instance
         `python-mode`) exhibit serious performance drops in some cases.

         Use this function with care, and at best only in major modes which are
         known to have good support for this feature.

   .. function:: flycheck-error-pos error

      Get the exact buffer position of :var:`error`.

      Essentially this is the position of the :cl-slot:`column` of :var:`error`,
      if it is not `nil`, or the position of the first non-whitespace character
      on the :cl-slot:`line` of :var:`error` otherwise.

   .. function:: flycheck-error-format error

      Format :var:`error` as a human-readable string.

The following functions and variables may be used to analyze the errors of a
syntax check.

.. variable:: flycheck-current-errors

   This buffer-local variable stores the errors of the last syntax check,
   sorted by line and column number.

.. function:: flycheck-count-errors errors

   Count the number of errors and warnings in :var:`errors`.

   Return an alist mapping error level symbols to error counts.  Each item is a
   cons cell :varcode:`({level} . {count})`, where :varcode:`{level}` is the
   level symbol and :varcode:`{count}` is the number of errors of of
   :varcode:`{level}`.

.. function:: flycheck-has-errors-p errors &optional level

   Determine if there are any :var:`errors` with the given :var:`level`.

   If :var:`level` is omitted, determine whether :var:`errors` is not nil.
   Otherwise determine whether there are any errors whose level is equal to the
   given :var:`level`.

.. _error-parser-api:

Error parser API
================

These functions can be used to implement custom error parsers:

.. function:: flycheck-parse-xml-string s

   Parse a string :var:`s` containing XML and return the parsed document tree.
