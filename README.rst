.. |travis| image:: https://travis-ci.org/flycheck/flycheck.svg?branch=master
            :target: https://travis-ci.org/flycheck/flycheck
            :alt: Test status

.. |license| image:: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
             :target: https://github.com/flycheck/flycheck/blob/master/COPYING
             :alt: License GPL 3

.. |docs| image:: https://readthedocs.org/projects/flycheck/badge/?version=latest
          :target: https://readthedocs.org/projects/flycheck/?badge=latest
          :alt: Documentation Status

.. |melpa| image:: http://melpa.org/packages/flycheck-badge.svg
           :target: http://melpa.org/#/flycheck
           :alt: MELPA Status

.. |melpa-stable| image:: http://stable.melpa.org/packages/flycheck-badge.svg
                  :target: http://stable.melpa.org/#/flycheck
                  :alt: MELPA Stable Status

.. |waffle| image:: https://badge.waffle.io/flycheck/flycheck.svg?label=ready&title=Ready
            :target: https://waffle.io/flycheck/flycheck
            :alt: Ready issues

.. |logo| image:: https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/logo.png
          :target: http://www.flycheck.org
          :alt: Flycheck logo

========
 |logo|
========

|license| |melpa| |melpa-stable| |waffle| |docs| |travis|

.. default-role:: code

**Flycheck** is a modern on-the-fly syntax checking extension for GNU Emacs 24,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.

It uses various syntax checking and linting tools to check the contents of
buffers, and reports warnings and errors directly in the buffer, or in an
optional error list.  The following screenshow shows Flycheck at work in a
Haskell buffer, using GHC and Hlint for syntax checking:

.. figure:: https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/screencast.gif
   :align: center

The color theme in this screencast is `Solarized Light`_, with the the
`Source Code Pro`_ font.

.. _Solarized Light: https://github.com/bbatsov/solarized-emacs
.. _Source Code Pro: https://github.com/adobe/source-code-pro

Features
========

- Supports over 30 programming and markup languages with more than 60 different
  syntax checking tools
- Fully automatic, fail-safe, on-the-fly syntax checking in background
- Nice error indication and highlighting
- Optional error list popup
- Many customization options
- A comprehensive manual
- A simple interface to define new syntax checkers
- A “doesn't get in your way” guarantee
- Many `3rd party extensions`_

.. _3rd party extensions: http://flycheck.org/en/latest/guide/introduction.html#rd-party-extensions

Installation
============

Flycheck needs GNU Emacs 24 on a Unix system, and additionally **external**
syntax checking programs for the languages you use.  Flycheck does not
officially support Windows, but tries to maintain Windows compatibility and
should generally work fine on Windows, too.  See Prerequisites_ and `Supported
Languages`_ for more information.

Install Flycheck from MELPA_ or `MELPA Stable`_ with::

   M-x package-install RET flycheck

In your Cask_ file:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

See `Package Installation`_ for more information.

.. _MELPA: http://melpa.org
.. _MELPA Stable: http://stable.melpa.org
.. _Cask: https://github.com/cask/cask
.. _Prerequisites: http://www.flycheck.org/en/latest/guide/installation.html#prerequisites
.. _Supported Languages: http://www.flycheck.org/en/latest/guide/languages.html
.. _Package installation: http://www.flycheck.org/en/latest/guide/installation.html#package-installation

Documentation
=============

Documentation is available at http://www.flycheck.org:

- `Quick start`_ (tutorial)
- `User manual`_ (reference information)

.. _Quick start: http://www.flycheck.org/en/latest/guide/quickstart.html
.. _User manual: http://www.flycheck.org/en/latest/guide/usage.html

Support & Contribution
======================

- `Issue tracker`_ and `Waffle Board`_
- Github_
- `Contributor Guide`_

.. _Issue tracker: https://github.com/flycheck/flycheck/issues
.. _Waffle Board: https://waffle.io/flycheck/flycheck
.. _Github: https://github.com/flycheck/flycheck
.. _Contributor Guide: http://www.flycheck.org/en/latest/index.html#contributor-guide

Credits
=======

- Contributors_
- Credits_

.. _Contributors: https://github.com/flycheck/flycheck/graphs/contributors
.. _Credits: http://www.flycheck.org/en/latest/credits.html

License
=======

Flycheck is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Flycheck is distributed in the hope that it will be useful, but WITHOUT ANY
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

Permission is granted to copy, distribute and/or modify the Flycheck logo in
``/flycheck.svg`` under the terms of the Creative Commons Attribution-ShareAlike
4.0 International Public License.  A copy of the license can be obtained at
https://creativecommons.org/licenses/by-sa/4.0/legalcode.

.. _COPYING: https://github.com/flycheck/flycheck/blob/master/COPYING
