==========
 Flycheck
==========

.. default-role:: code

.. |travis| image:: https://travis-ci.org/flycheck/flycheck.svg?branch=master
            :target: https://travis-ci.org/flycheck/flycheck

.. |license| image:: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
             :target: https://github.com/flycheck/flycheck/blob/master/COPYING

|license| |travis|

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24.

It uses various syntax checking and linting tools to check the contents of
buffers, and reports warnings and errors directly in the buffer, or in an
optional error list.  The following screenshot shows Flycheck at work in an
Emacs Lisp buffer, with with the `Solarized Light`_ color theme and the `Source
Code Pro`_ font:

.. figure:: https://github.com/flycheck/flycheck/raw/master/doc/images/screenshot.png
   :align: center
   :width: 731
   :height: 519
   :scale: 75%

It is a replacement for the older Flymake extension, which is part of Emacs,
with more supported languages, more features, and better extensibility:

- Features_
- Changelog_
- `Supported Languages`_
- `Flycheck versus Flymake`_
- `3rd party extensions`_

.. _Solarized Light: https://github.com/bbatsov/solarized-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro
.. _Features: http://flycheck.readthedocs.org/en/latest/manual/introduction.html#features
.. _Changelog: http://flycheck.readthedocs.org/en/latest/manual/changes.html
.. _Flycheck versus Flymake: http://flycheck.readthedocs.org/en/latest/flycheck-versus-flymake.html
.. _Supported Languages: http://flycheck.readthedocs.org/en/latest/manual/languages.html
.. _3rd party extensions: http://flycheck.readthedocs.org/en/latest/manual/introduction.html#rd-party-extensions

Installation
============

Flycheck needs GNU Emacs 24, and works best on Unix systems (i.e. Linux or OS
X).  Windows or other flavors of Emacs are **not** supported.

For almost all supported languages, Flycheck also needs additional
**external** programs.  See `Supported Languages`_ for a list of supported
languages and the corresponding checker programs, and use `C-c ! ?` to get help
about specific checkers inside Emacs.

Install the ELPA package from MELPA_ or Marmalade_ with `M-x package-install RET
flycheck`.

In your Cask_ file:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

See Installation_ for detailed installation instructions.

.. _MELPA: http://melpa.milkbox.net/#/flycheck
.. _Marmalade: http://marmalade-repo.org/packages/flycheck
.. _Cask: https://github.com/cask/cask
.. _Supported Languages: http://flycheck.readthedocs.org/en/latest/manual/languages.html
.. _Installation: http://flycheck.readthedocs.org/en/latest/manual/installation.html

Quick start
===========

Once installed, enable Flycheck globally with the following line in your
`init.el`:

.. code-block:: cl

   (add-hook 'after-init-hook #'global-flycheck-mode)

Enable Flycheck globally by adding the following to your `init.el`, and
restart Emacs:

.. code-block:: cl

   (add-hook 'after-init-hook #'global-flycheck-mode)

Install some syntax checking tools for the programming or markup language you
are using.  For Python:

.. code-block:: console

   $ pip install --user pylint

Or for Ruby:

.. code-block:: console

   $ gem install rubocop ruby-lint

Or for Haskell:

.. code-block:: console

   $ brew install haskell-platform
   $ cabal install hlint

Now browse the `syntax checker options`_ to configure your syntax checkers.
Typically you don't need to change any options, though.  Flycheck will mostly
work automatically.

Flycheck will now check syntax using these tools, when you visit a buffer in any
of these languages.  Syntax checking happens automatically when you save the
buffer or make any changes.  Flycheck highlights errors and warnings in the
buffer, indicates them in the fringe, and reports their numbers in the mode
line.

Use `C-c ! n` and `C-c ! p` to navigate between error locations.  If you keep
the point at an error location, Flycheck will show the error message in the echo
area after a short delay.  You can also hover error locations with the mouse and
see the error message in a tooltip.

To get an overview of all errors and warnings in the current buffer, type `C-c !
l` to pop up a list of all errors in your current buffer.  The error list
updates automatically when you fix errors or introduce new ones, or when you
switch to another buffer.

For more details, read the `Usage`_ instructions in the manual.

.. _Syntax checker options: http://flycheck.readthedocs.org/en/latest/manual/usage.html#syntax-checker-configuration
.. _Usage: http://flycheck.readthedocs.org/en/latest/manual/usage.html

Support
=======

- `User Manual`_
- `Issue tracker`_

.. _User Manual: http://flycheck.readthedocs.org/en/latest/manual/index.html
.. _Issue tracker: https://github.com/flycheck/flycheck/issues

Contribute
==========

- Github_
- `Contribution guidelines`_

.. _Github: https://github.com/flycheck/flycheck
.. _Contribution guidelines: https://github.com/flycheck/flycheck/blob/master/CONTRIBUTING.rst

Credits
=======

- Contributors_
- Credits_

.. _Contributors: https://github.com/flycheck/flycheck/graphs/contributors
.. _Credits: http://flycheck.readthedocs.org/en/latest/manual/credits.html

License
=======

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See COPYING_ for details.

The Flycheck documenation is free documentation: you can copy, distribute and/or
modify it under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with no Invariant
Sections, no Front-Cover Texts, and no Back-Cover Texts.  A copy of the license
is available at https://github.com/flycheck/flycheck/blob/master/doc/fdl.txt.

Alternatively, you may copy, distribute and/or modify the Flycheck documentation
under the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  A copy of the license can be obtained at
https://creativecommons.org/licenses/by-sa/4.0/legalcode.

.. _COPYING: https://github.com/flycheck/flycheck/blob/master/COPYING
