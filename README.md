[![License GPL 3](https://img.shields.io/badge/license-GPL_3-blue.svg)][COPYING]
[![MELPA stable version](http://stable.melpa.org/packages/flycheck-badge.svg)](http://stable.melpa.org/#/flycheck)
[![MELPA version](http://melpa.org/packages/flycheck-badge.svg)](http://melpa.org/#/flycheck)
[![Manual](https://img.shields.io/badge/manual-latest-green.svg)][manual]
[![Waffle Board](https://img.shields.io/badge/Board-ready-75AED9.svg)](https://waffle.io/flycheck/flycheck)

# [![Flycheck Logo](https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/logo.png)](http://www.flycheck.org)

**Flycheck** is a modern on-the-fly syntax checking extension for GNU Emacs 24,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.

It uses various syntax checking and linting tools to check the contents of
buffers, and reports warnings and errors directly in the buffer or in an
optional error list:

![](https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/flycheck-annotated.png)

[COPYING]: https://github.com/flycheck/flycheck/blob/master/COPYING
[manual]: http://www.flycheck.org/manual/latest/index.html

Features
--------

- Supports over 30 programming and markup languages with more than 70 different
  syntax checking tools
- Fully automatic, fail-safe, on-the-fly syntax checking in background
- Nice error indication and highlighting
- Optional error list popup
- Many customization options
- A [comprehensive manual][manual]
- A simple interface to define new syntax checkers
- A “doesn't get in your way” guarantee
- Many [3rd party extensions](http://flycheck.org/extensions.html)

Installation
------------

Flycheck needs GNU Emacs 24 on a Unix system, and additionally **external**
syntax checking programs for the languages you use.  See [Supported Languages][]
for more information.

Flycheck does not officially support Windows, but tries to maintain Windows
compatibility and should generally work fine on Windows, too.

Install Flycheck from [MELPA](http://melpa.org) or
[MELPA Stable](http://stable.melpa.org) with:

    M-x package-install RET flycheck

In your [Cask](https://github.com/cask/cask) file:

    (source gnu)
    (source melpa)

    (depends-on "flycheck")

Then add the following to your init file:

    (add-hook 'after-init-hook #'global-flycheck-mode)

If you don’t know where your init file is, look at the value of the variable
`user-init-file` with `C-h v user-init-file`.

See [Installation][] and [Quick start][] for more information.

[Supported Languages]: http://www.flycheck.org/manual/latest/Supported-languages.html#Supported-languages
[Installation]: http://www.flycheck.org/manual/latest/Installation.html#Installation
[Quick start]: http://www.flycheck.org/manual/latest/Quickstart.html#Quickstart

Documentation
=============

Documentation is available at http://www.flycheck.org:

- [Quick start][] (tutorial)
- [User manual](http://www.flycheck.org/manual/latest/Usage.html#Usage)
  (reference information)

Support & Contribution
======================

- [Stack Exchange](https://emacs.stackexchange.com/questions/tagged/flycheck)
- [Issue Tracker](https://github.com/flycheck/flycheck/issues)
- [Waffle Board](http://waffle.io/flycheck/flycheck)
- [Github](https://github.com/flycheck/flycheck)
- [Contribution guidelines](https://github.com/flycheck/flycheck/blob/master/CONTRIBUTING.md)

Credits
=======

- [Contributors](https://github.com/flycheck/flycheck/graphs/contributors)
- [Credits](https://github.com/flycheck/flycheck/blob/master/CREDITS.md)

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
this program.  If not, see <http://www.gnu.org/licenses/>.

See [COPYING][] for details.

The Flycheck documenation is free documentation: you can copy, distribute and/or
modify it under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with no Invariant
Sections, no Front-Cover Texts, and no Back-Cover Texts.  A copy of the license
is available at <https://github.com/flycheck/flycheck/blob/master/doc/fdl.txt>.

Alternatively, you may copy, distribute and/or modify the Flycheck documentation
under the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  A copy of the license can be obtained at
<https://creativecommons.org/licenses/by-sa/4.0/legalcode>.

Permission is granted to copy, distribute and/or modify the Flycheck logo in
`/flycheck.svg` under the terms of the Creative Commons Attribution-ShareAlike
4.0 International Public License.  A copy of the license can be obtained at
<https://creativecommons.org/licenses/by-sa/4.0/legalcode>.
