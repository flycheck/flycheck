==========================================
 Flycheck — Syntax checking for GNU Emacs
==========================================

**Flycheck** is a modern on-the-fly syntax checking extension for GNU Emacs,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.  For a detailed comparison to Flymake see :ref:`flycheck-versus-flymake`.

It uses various syntax checking and linting tools to :ref:`automatically check
the contents of buffers <flycheck-syntax-checks>` while you type, and reports
warnings and errors directly in the buffer, or in an optional :ref:`error list
<flycheck-error-list>`:

.. image:: images/flycheck-annotated.png

Out of the box Flycheck supports over :ref:`40 different programming languages
<flycheck-languages>` with more than 80 different syntax checking tools, and
comes with a :ref:`simple interface <flycheck-definitions>` to define new syntax
checkers.

Many :ref:`3rd party extensions <flycheck-extensions>` provide new syntax
checkers and other features like alternative error displays or mode line
indicators.

Try out
=======

Flycheck needs GNU Emacs 24.3 or newer, and works best on Unix systems.
**Windows users**, please be aware that Flycheck does not support Windows
officially, although it should mostly work fine on Windows.  See :ref:`Windows
support <flycheck-windows-support>` and watch out for `known Windows
issues`_!

To try Flycheck in your Emacs session install some :ref:`syntax checker tools
<flycheck-languages>` and type the following in your ``*scratch*`` buffer and
run ``M-x eval-buffer``:

.. code-block:: cl

   (require 'package)
   (add-to-list 'package-archives
                '("melpa" . "http://stable.melpa.org/packages/") t)
   (package-initialize)

   (package-install 'flycheck)

   (global-flycheck-mode)

For a permanent installation of Flycheck follow the :ref:`Installation
<flycheck-installation>` instructions.  For a gentle introduction into Flycheck
features go through :ref:`Quickstart <flycheck-quickstart>` guide.

.. _`known windows issues`: https://github.com/flycheck/flycheck/labels/B-Windows%20only

The User Guide
==============

The User Guide provides installation and usage help for Flycheck.  It starts
with installation instructions and a quick start tutorial and then focuses on an
in-depth references of all parts of Flycheck.

We are currently in the process of converting the old Texinfo manual to Sphinx.
Meanwhile you can read a simple HTML version of the old manual at
:download:`flycheck.html <legacy/flycheck.html>`.

.. todo:: Port the Texinfo manual

.. toctree::

   user/installation
   user/quickstart
   user/syntax-checks
   user/syntax-checkers
   user/error-reports
   user/flycheck-versus-flymake

The Developer Guide
===================

The Developer Guide shows how to write syntax checkers for Flycheck and how to
extend Flycheck.

.. todo:: Port the extending section from the Texinfo manual

The Community Guide
===================

The Community Guide provides information about Flycheck’s ecosystem and
community.

.. toctree::

   community/conduct
   community/extensions
   community/get-help
   community/people

The Contributor Guide
=====================

The Contributor Guide explains how to contribute to Flycheck.

.. toctree::

   contributor/contributing
   contributor/maintaining

Indices and Tables
==================

* :ref:`flycheck-languages`
* :doc:`glossary`
* :doc:`changes`
* :ref:`genindex`
* :ref:`search`

.. toctree::
   :hidden:

   languages
   glossary
   changes

Licensing
=========

Flycheck is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Flycheck is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

See :ref:`flycheck-gpl` for a copy of the GNU General Public License.

Permission is granted to copy, distribute and/or modify the Flycheck
documentation under the terms of the GNU Free Documentation License, Version 1.3
or any later version published by the Free Software Foundation; with no
Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.  See
:ref:`flycheck-fdl` for a copy of the GNU Free Documentation License.

Alternatively, you may copy, distribute and/or modify the Flycheck documentation
under the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  See :ref:`flycheck-cc-by-sa` for a copy of the license.

Permission is granted to copy, distribute and/or modify the Flycheck logo under
the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  See :ref:`flycheck-cc-by-sa` for a copy of the license.

.. toctree::
   :hidden:
   :maxdepth: 2

   licenses

.. FIXME: Remove when the old manual is ported completed

TODO
====

.. todolist::
