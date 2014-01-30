==========
 Flycheck
==========

.. default-role:: code

.. figure:: https://github.com/flycheck/flycheck/raw/master/doc/images/screenshot.png
   :align: center

   The screenshot shows Flycheck on Emacs 24.3.50 with the awesome Solarized_
   Light color theme and the great `Source Code Pro`_ font.

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24

- Support for over 30 languages
- Fully automatic syntax checking in the background
- Nice error indication and highlighting
- Optional error list popup
- Many customization options
- A comprehensive manual
- A dead simple function to create new syntax checkers
- A “doesn't get in your way” guarantee

- `Flycheck versus Flymake`_
- `Supported Languages`_

.. _Solarized: https://github.com/bbatsov/solarized-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro
.. _Flycheck versus Flymake: http://flycheck.readthedocs.org/en/latest/flycheck-versus-flymake.html
.. _Supported Languages: http://flycheck.readthedocs.org/en/latest/manual/languages.html

3rd party extensions
====================

The following extensions provide additional cool features for Flycheck:

- flycheck-cask_ makes Flycheck use local packages in Cask_ projects.
- flycheck-color-mode-line_ colors the mode line according to the Flycheck
  status.
- flycheck-d-unittest_ a Flycheck checker to run unit tests for D programs on
  the fly.
- flycheck-haskell_ improves Haskell support in Flycheck, by configuring
  Flycheck according to the current Cabal project, and using Cabal sandbox
  packages.
- flycheck-hdevtools_ provides a Flycheck syntax checker for hdevtools_.
- flycheck-mercury_ provides a Flycheck syntax checker for the `Mercury
  language`_.

.. _flycheck-cask: https://github.com/flycheck/flycheck-cask
.. _Cask: https://github.com/cask/cask
.. _flycheck-color-mode-line: https://github.com/flycheck/flycheck-color-mode-line
.. _flycheck-d-unittest: https://github.com/flycheck/flycheck-d-unittest
.. _flycheck-hdevtools: https://github.com/flycheck/flycheck-hdevtools
.. _hdevtools: https://github.com/bitc/hdevtools
.. _flycheck-haskell: https://github.com/flycheck/flycheck-haskell
.. _flycheck-mercury: https://github.com/flycheck/flycheck-mercury
.. _Mercury language: http://mercurylang.org/

Installation
============

Flycheck needs Emacs 24, and works best on Unix systems (i.e. Linux or OS X).

Install the ELPA package from MELPA_ or Marmalade_ with `M-x package-install RET
flycheck`.

In your Cask_ file:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

Most syntax checkers need external utilities.  See `Supported languages`_ for
supported languages and utilities, or use `C-c ! ?` to see information about a
specific syntax checker.

See Installation_ for detailed installation instructions.

.. _MELPA: http://melpa.milkbox.net/#/flycheck
.. _Marmalade: http://marmalade-repo.org/packages/flycheck
.. _Cask: https://github.com/cask/cask
.. _Supported Languages: http://flycheck.readthedocs.org/en/latest/manual/languages.html
.. _Installation: http://flycheck.readthedocs.org/en/latest/manual/introduction.html#installation

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

.. _COPYING: https://github.com/flycheck/flycheck/blob/master/COPYING
