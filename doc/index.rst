=========================================
 Flycheck — Modern Emacs syntax checking
=========================================

.. highlight:: cl

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24.

It uses various syntax checking and linting tools to check the contents of
buffers, and reports warnings and errors directly in the buffer, or in an
optional error list.  The following screenshot shows Flycheck at work in an
Emacs Lisp buffer, with the `Solarized Light`_ theme and the `Source Code Pro`_
font:

.. figure:: /images/screenshot.png
   :align: center
   :width: 731
   :height: 519
   :scale: 75%

It is a replacement for the older Flymake extension, which is part of Emacs,
with more supported languages, more features, and better extensibility:

- :ref:`features`
- :doc:`Changes in this version <manual/changes>`
- :doc:`manual/languages`
- :ref:`3rd-party-extensions`
- :doc:`flycheck-versus-flymake`

.. _Solarized Light: https://github.com/bbatsov/solarized-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro

Installation
============

Flycheck needs GNU Emacs 24, and works best on Unix systems (i.e. Linux or OS
X).  Windows or other flavors of Emacs are **not** supported.

Additionally, Flycheck needs **external** checker programs for almost all
supported languages.  See :doc:`manual/languages` for a list of supported
languages and the corresponding checker programs, and use
:command:`flycheck-describe-checker` to get help about specific checkers inside
Emacs.

Install the ELPA package from MELPA_ or Marmalade_ with :kbd:`M-x
package-install RET flycheck`.

In your Cask_ file::

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

See :ref:`installation` for detailed installation instructions.

.. _Cask: https://github.com/cask/cask
.. _MELPA: http://melpa.milkbox.net/#/flycheck
.. _Marmalade: http://marmalade-repo.org/packages/flycheck

Quick start
===========

Enable Flycheck globally by adding the following to your :file:`init.el`, and
restart Emacs::

   (add-hook 'after-init-hook #'global-flycheck-mode)

.. highlight:: console

Install some syntax checking tools for the programming or markup language you
are using.  For Python::

   $ pip install --user pylint

Or for Ruby::

   $ gem install rubocop ruby-lint

Or for Haskell::

   $ brew install haskell-platform
   $ cabal install hlint

Now browse the :ref:`syntax checker options <syntax-checker-configuration>` to
configure your syntax checkers.  Typically you don't need to change any options,
though.  Flycheck will mostly work automatically.

Flycheck will now check syntax using these tools, when you visit a buffer in any
of these languages.  Syntax checking happens automatically when you save the
buffer or make any changes.  Flycheck highlights errors and warnings in the
buffer, indicates them in the fringe, and reports their numbers in the mode
line.

Use :kbd:`C-c ! n` (:function:`flycheck-next-error`) and :kbd:`C-c ! p`
(:function:`flycheck-previous-error`) to navigate between error locations.  If
you keep the point at an error location, Flycheck will show the error message in
the echo area after a short delay.  You can also hover error locations with the
mouse and see the error message in a tooltip.

To get an overview of all errors and warnings in the current buffer, type
:kbd:`C-c ! l` (:function:`flycheck-list-errors`) to pop up a list of all errors
in your current buffer.  The error list updates automatically when you fix
errors or introduce new ones, or when you switch to another buffer.

For more details, read the :ref:`usage` instructions in the manual.

Support
=======

- :doc:`manual/index`
- :github:`Issue tracker <issues>`

Contribute
==========

- :github:`Github <>`
- :doc:`manual/contributing`

Credits
=======

- :github:`Contributors <graphs/contributors>`
- :doc:`manual/credits`

Contents
========

.. toctree::
   :maxdepth: 2

   flycheck-versus-flymake
   news/index
   manual/index
   copying

Licensing
=========

Flycheck is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Flycheck is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

See :doc:`copying` or http://www.gnu.org/licenses/ for a copy of the GNU General
Public License.

This documentation is free documentation: you can copy, distribute and/or modify it
under the terms of the GNU Free Documentation License, Version 1.3 or any later
version published by the Free Software Foundation; with no Invariant Sections,
no Front-Cover Texts, and no Back-Cover Texts.  A copy of the license is
included in the section :doc:`manual/fdl`.

Alternatively, you may copy, distribute and/or modify this documentation under
the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  A copy of the license can be obtained at
https://creativecommons.org/licenses/by-sa/4.0/legalcode.
