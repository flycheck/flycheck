.. _flycheck-quickstart:

============
 Quickstart
============

This page gives a quick introduction into Flycheck and an overview of its most
important features.  Before you start here please make sure that Flycheck is
:ref:`installed <flycheck-installation>`.

Enable Flycheck
===============

Now add the following code to your :term:`init file` to permanently enable
syntax checking with Flycheck:

.. code-block:: elisp

   (add-hook 'after-init-hook #'global-flycheck-mode)

Install syntax checker programs
===============================

Now you need to install syntax checking programs for the languages you'd like to
use Flycheck with.  The :ref:`list of supported languages <flycheck-languages>`
tells you which languages Flycheck supports and what programs it uses.

For instance, you can install Pylint_ for Python and ESLint_ for Javascript:

.. code-block:: shell

   $ pip install pylint
   $ npm install eslint

.. _Pylint: https://pylint.org
.. _ESLint: https://eslint.org

Check syntax in a buffer
========================

Now you are ready to use Flycheck in a Python or Javascript buffer.  Visit a
Python or Javascript file and check whether your Flycheck setup is complete with
`C-c ! v`.

If everything is green, Flycheck will now start to check the buffer on the fly
while you are editing.  Whenever you make a mistake that eslint or Pylint can
catch, Flycheck will highlight the corresponding place in the buffer with an
error underline whose color reflects the severity of the issue.  Additionally,
Flycheck will put a symbol into the fringe for affected lines and show the total
number of errors and warnings in the buffer in the mode line.

Navigate and list errors
========================

With `C-c ! n` and `C-c ! p` you can now jump back and forth between erroneous
places.  If you keep on such a place for a little while Flycheck will show the
corresponding error message in the echo area.  Likewise, if you hover such a
place with the mouse cursor Flycheck will show the error message in a tooltip.

Press `C-c ! l` to pop up a list of all errors in the current buffer.  This list
automatically updates itself when you fix errors or introduce new ones, and
follows the currently selected buffer.  If the error list is selected you can
type :kbd:`n` and :kbd:`p` to move up and down between errors and jump to their
corresponding location in the buffer.

More features
=============

All Flycheck commands are available in the Emacs Menu at :menuselection:`Tools
---> Syntax checking`:

.. figure:: /images/flycheck-menu.png

   The menu of Flycheck, showing all available Flycheck commands

The same menu also pops up when you click on the mode line lighter:

.. figure:: /images/flycheck-mode-line-menu.png

   The mode line menu of Flycheck

What else Flycheck can do
=========================

The basics above are only the start.  A few highlights worth knowing about:

- **Inline error messages.**  Flycheck can show each message right next to the
  code it refers to, in the spirit of VS Code's Error Lens - turn on
  ``flycheck-annotate-mode`` (see :doc:`error-reports`).
- **Fixes.**  Some checkers report a machine-applicable fix; apply the one at
  point with :kbd:`C-c ! f`, or every fix in the buffer with :kbd:`C-c ! F`
  (see :doc:`error-interaction`).
- **Related locations.**  When an error points at other places (an earlier
  definition, a borrow), jump to them with :kbd:`C-c ! j` (see
  :doc:`error-interaction`).
- **Language servers.**  Flycheck can report a language server's diagnostics -
  either through Eglot (``flycheck-eglot-mode``) or by talking to a diagnostics
  linter directly, with no full LSP client (``flycheck-lsp-mode``).  See
  :doc:`syntax-checks`.
