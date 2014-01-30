=========================================
 Flycheck — Modern Emacs syntax checking
=========================================

.. figure:: /images/screenshot.png
   :scale: 75%
   :align: center

   Flycheck with the `Solarized Light`_ theme and the `Source Code Pro`_ font

.. _Solarized Light: https://github.com/bbatsov/solarized-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro

Features
========

- :ref:`features`
- :doc:`flycheck-versus-flymake`
- :doc:`manual/languages`
- :ref:`3rd-party-extensions`

Installation
============

Flycheck needs Emacs 24, and works best on Unix systems (i.e. Linux or OS X).

Install the ELPA package from MELPA_ or Marmalade_ with :kbd:`M-x
package-install RET flycheck`.

In your Cask_ file:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

Most syntax checkers need external utilities.  See :doc:`manual/languages` for
supported languages and utilities, or use :kbd:`C-c ! ?` to see information
about a specific syntax checker.

See :ref:`installation` for detailed installation instructions.

.. _Cask: https://github.com/cask/cask
.. _MELPA: http://melpa.milkbox.net/#/flycheck
.. _Marmalade: http://marmalade-repo.org/packages/flycheck

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
