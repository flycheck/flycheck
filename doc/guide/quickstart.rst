============
 Quickstart
============

Enable Flycheck
===============

Enable Flycheck globally by adding the following to your :file:`init.el`, and
restart Emacs:

.. code-block:: cl

   (add-hook 'after-init-hook #'global-flycheck-mode)

Install some syntax checker tools
=================================

.. highlight:: console

Install some syntax checking tools for the programming or markup language you
are using.  Take a look at the :ref:`list of supported languages
<supported-languages>` to check what languages and tools are supported.

For Python::

   $ pip install --user pylint

Or for Ruby::

   $ gem install rubocop ruby-lint

Or for Haskell::

   $ brew install haskell-platform
   $ cabal install hlint

The :ref:`language list <supported-languages>` also lists the available options
for each syntax checker.  Take a look at these to tune the syntax checkers to
your needs, and read :ref:`how to use these options <syntax-checker-options>`.

Check syntax in a buffer
========================

Flycheck will now check syntax using these tools, when you visit a buffer in any
of these languages.  Syntax checking happens **automatically** when you save the
buffer or make any changes.  Flycheck highlights errors and warnings in the
buffer, indicates them in the fringe, and reports their numbers in the mode
line.

You can also manually check a buffer with :kbd:`C-c ! c`
(:function:`flycheck-buffer`).

Navigate and list errors
========================

Use :kbd:`C-c ! n` (:function:`flycheck-next-error`) and :kbd:`C-c ! p`
(:function:`flycheck-previous-error`) to navigate between error locations.  If
you keep the point at an error location, Flycheck will show the error message in
the echo area after a short delay.  You can also hover error locations with the
mouse and see the error message in a tooltip.

To get an overview of all errors and warnings in the current buffer, type
:kbd:`C-c ! l` (:function:`flycheck-list-errors`) to pop up a list of all errors
in your current buffer.  The error list updates automatically when you fix
errors or introduce new ones, or when you switch to another buffer.

Explore the menu
================

All Flycheck commands are available in the Emacs menu at :menuselection:`Tools
--> Syntax Checking`.  Explore this menu to see what features Flycheck has to
offer.

.. only:: not texinfo

   .. figure:: /images/flycheck-menu.png
      :align: center
      :width: 678
      :height: 513
      :scale: 75%

The menu can also be opened by clicking on the mode line lighter of Flycheck.

.. only:: not texinfo

   .. figure:: /images/flycheck-mode-line-menu.png
      :align: center
      :width: 444
      :height: 361
      :scale: 75%

Further reading
===============

- :doc:`usage` describes all commands and options in detail.
- :doc:`languages` has a list of all supported languages, and the corresponding
  syntax checker tools and their options.
