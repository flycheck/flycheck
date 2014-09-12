=========================================
 Flycheck — Modern Emacs syntax checking
=========================================

.. image:: /images/logo.png
   :align: center

.. highlight:: cl

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.

It uses various syntax checking and linting tools to automatically check the
contents of buffers while you type, and reports warnings and errors directly in
the buffer, or in an optional error list:

.. figure:: /images/screenshot.png
   :align: center
   :width: 731
   :height: 519
   :scale: 75%

   Flycheck in an Emacs Lisp buffer, with error highlights, indicators, and the
   error list.  The color theme is `Solarized Light`_, together with the `Source
   Code Pro`_ font.

.. contents:: Table of Contents
   :local:

.. _Solarized Light: https://github.com/bbatsov/solarized-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro

User guide
==========

This part of the documentation explains how to use Flycheck.  It starts with a
little introduction into Flycheck and a comparison with the built-in Flymake
package, and then guides you through the installation and gives you a quick
start.  Eventually it provides comprehensive usage instructions covering all
commands and options of Flycheck, and concludes with a list of supported
languages and syntax checking tools.

.. toctree::
   :maxdepth: 2

   guide/introduction
   guide/flycheck-versus-flymake
   guide/installation
   guide/quickstart
   guide/usage
   guide/languages
   guide/glossary
   guide/releases/index

Developer guide
===============

This part of the documentation shows how to extend Flycheck, and provides an API
reference.

.. toctree::
   :maxdepth: 2

   dev/extending
   dev/api

Contributor guide
=================

This part of the documentation targets contributors to Flycheck.  It provides
general contribution guidelines, by which we review pull requests and other
contributions, and instructions for specific tasks Flycheck contributors need to
perform and problems they face.

.. toctree::
   :maxdepth: 2

   contrib/guidelines
   contrib/testing
   contrib/docs
   contrib/maintenance

Index
=====

The index provides a sorted list of all symbols and concepts explained
throughout Flycheck's documentation:

- :ref:`genindex`

Credits
=======

.. include:: ../CREDITS.rst
   :start-line: 4

Licensing
=========

Flycheck is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Flycheck is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

See :doc:`gpl` or http://www.gnu.org/licenses/ for a copy of the GNU General
Public License.

Permission is granted to copy, distribute and/or modify this documentation under
the terms of the GNU Free Documentation License, Version 1.3 or any later
version published by the Free Software Foundation; with no Invariant Sections,
no Front-Cover Texts, and no Back-Cover Texts.  A copy of the license is
included in the section entitled :doc:`fdl`.

Alternatively, you may copy, distribute and/or modify this documentation under
the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  A copy of the license can be obtained at
https://creativecommons.org/licenses/by-sa/4.0/legalcode.

Permission is granted to copy, distribute and/or modify the Flycheck logo under
the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  A copy of the license can be obtained at
https://creativecommons.org/licenses/by-sa/4.0/legalcode.

.. toctree::
   :maxdepth: 1

   gpl
   fdl
