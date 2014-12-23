.. _flycheck-ert:

==================
 Flycheck ERT API
==================

.. require:: flycheck-ert

This chapter documents the Flycheck ERT API, which is used throughout Flycheck's
own test suite, and available for use in test suites for your own Flycheck
extensions.

.. contents:: Contents
   :local:

Compatibility
=============

``flycheck-ert`` is compatible with all ERT versions included in Emacs from 24.1
onwards.

.. note:: Test skipping

   ERT in Emacs 24.3 and earlier does not support skipped tests.
   For these versions, ``flycheck-ert`` provides limited support for skipped
   tests by marking them as expected to fail.

   For this feature to work, you **must** call `flycheck-ert-initialize` after
   defining all test cases.


Loading and initializing
========================

The ERT API is provided by the ``flycheck-ert`` library, which is part of the
Flycheck package.  If the Flycheck package is installed (see
:ref:`installation`), you can load the ERT API with:

.. code-block:: cl

   (require 'flycheck-ert)

This will also load the ``ert`` and ``flycheck`` libraries.

Flycheck ERT supports test resources (see :ref:`test-resources`).  For this
feature, you need to initialize Flycheck ERT with a resource directory by
calling :function:`flycheck-ert-initialize`:

.. function:: flycheck-ert-initialize
   :auto:

Utilities
=========

Creating temporary buffers
--------------------------

.. macro:: flycheck-ert-with-temp-buffer
   :auto:

.. macro:: flycheck-ert-with-file-buffer
   :auto:

Scoping resource access
-----------------------

.. macro:: flycheck-ert-with-help-buffer
   :auto:

.. macro:: flycheck-ert-with-env
   :auto:

.. macro:: flycheck-ert-with-global-mode
   :auto:

.. _test-resources:

Accessing test resources
------------------------

The following functions and macros load resources from the directory given to
:function:`flycheck-ert-initialize`.

.. function:: flycheck-ert-resource-filename
   :auto:

.. macro:: flycheck-ert-with-resource-buffer
   :auto:

.. function:: flycheck-ert-locate-config-file
   :auto:

Obtaining information about the environment
-------------------------------------------

.. constant:: flycheck-ert-user-error-type
   :auto:

.. function:: flycheck-ert-travis-ci-p
   :auto:

.. function:: flycheck-ert-check-gpg
   :auto:

.. function:: flycheck-ert-extract-version-command
   :auto:

Defining test cases
===================

In addition to the standard :macro:`ert-deftest` from ERT, this library provides
macros for specialized test case definitions:

.. macro:: flycheck-ert-def-checker-test
   :auto:

Checking results of test cases
------------------------------

Flycheck ERT provides some functions to check the results of test cases, which
are handy in `:expected-result` forms.

.. function:: flycheck-ert-syntax-check-timed-out-p
   :auto:

Invoking syntax checkers in test cases
--------------------------------------

.. function:: flycheck-ert-buffer-sync
   :auto:

.. function:: flycheck-ert-ensure-clear
   :auto:

Writing assertions
------------------

.. function:: flycheck-ert-should-overlay
   :auto:

.. function:: flycheck-ert-should-errors
   :auto:

.. function:: flycheck-ert-should-syntax-check
   :auto:

.. function:: flycheck-ert-at-nth-error
   :auto:
