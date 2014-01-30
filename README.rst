==========
 Flycheck
==========

.. default-role:: code

.. figure:: https://github.com/flycheck/flycheck/raw/master/doc/images/screenshot.png
   :align: center

   The screenshot shows Flycheck on Emacs 24.3.50 with the awesome Solarized_
   Light color theme and the great `Source Code Pro`_ font.

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24

- Features_
- `Flycheck versus Flymake`_
- `Supported Languages`_
- `3rd party extensions`_

.. _Solarized: https://github.com/bbatsov/solarized-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro
.. _Features: http://flycheck.readthedocs.org/en/latest/manual/introduction.html#features
.. _Flycheck versus Flymake: http://flycheck.readthedocs.org/en/latest/flycheck-versus-flymake.html
.. _Supported Languages: http://flycheck.readthedocs.org/en/latest/manual/languages.html
.. _3rd party extensions: http://flycheck.readthedocs.org/en/latest/manual/introduction.html#rd-party-extensions

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
