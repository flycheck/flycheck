==========================================
 Flycheck — Syntax checking for GNU Emacs
==========================================

**Flycheck** is a modern on-the-fly syntax checking extension for GNU Emacs,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.

It uses various syntax checking and linting tools to :ref:`automatically check
the contents of buffers <flycheck-checking>` while you type, and reports
warnings and errors directly in the buffer, or in an optional error list:

.. image:: images/flycheck-annotated.png

Out of the box Flycheck supports over :ref:`40 different programming languages
<flycheck-languages>` with more than 80 different syntax checking tools, and
comes with a :ref:`simple interface <flycheck-definitions>` to define new syntax
checkers.

Many :ref:`3rd party extensions <flycheck-extensions>` provide new syntax
checkers and other features like alternative error displays or mode line
indicators.

Very quick start
================

Flycheck needs GNU Emacs 24.1 or newer, and works best on Unix systems.
**Windows users**, please be aware that Flycheck does not support Windows
officially, although it should mostly work fine on Windows.  See :ref:`Windows
support <flycheck-windows-support>` and watch out for `known Windows
issues`_!

Install some :ref:`syntax checker tools <flycheck-languages>` and type the
following in your ``*scratch*`` buffer and run ``M-x eval-buffer``:

.. code-block:: cl

   (require 'package)
   (add-to-list 'package-archives
                '("melpa" . "http://stable.melpa.org/packages/") t)
   (package-initialize)

   (package-install 'flycheck)

   (global-flycheck-mode)

For a more gentle introduction read the :ref:`Installation
<flycheck-installation>` instructions and go through :ref:`Quickstart
<flycheck-quickstart>` guide.

.. _`known windows issues`: https://github.com/flycheck/flycheck/labels/B-Windows%20only

The User Guide
==============

The User Guide provides installation and usage help for Flycheck.  It starts
with installation instructions and a quick start tutorial and then focuses on an
in-depth references of all parts of Flycheck.

The Community Guide
===================

The Community Guide provides information about Flycheck’s ecosystem and
community.

.. toctree::
   :maxdepth: 1

   community/conduct
   community/extensions
   community/support
   community/people

The Contributor Guide
=====================

The Contributor Guide explains how to contribute to Flycheck.

.. toctree::
   :maxdepth: 2

   contributor/contributing.rst
