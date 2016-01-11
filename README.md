[![License GPL 3](https://img.shields.io/badge/license-GPL_3-blue.svg)][COPYING]
[![Join the chat](https://badges.gitter.im/flycheck/flycheck.svg)](https://gitter.im/flycheck/flycheck)
[![Manual](https://img.shields.io/badge/manual-latest-green.svg)][manual]
[![MELPA stable version](http://stable.melpa.org/packages/flycheck-badge.svg)](http://stable.melpa.org/#/flycheck)
[![MELPA version](http://melpa.org/packages/flycheck-badge.svg)](http://melpa.org/#/flycheck)
[![Build Status](https://travis-ci.org/flycheck/flycheck.svg?branch=master)](https://travis-ci.org/flycheck/flycheck)

# [![Flycheck][logo]](http://www.flycheck.org) #

**Flycheck** is a modern on-the-fly syntax checking extension for GNU Emacs 24,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.

It uses various syntax checking and linting tools to check the contents of
buffers, and reports warnings and errors directly in the buffer or in an
optional error list (see [Flycheck manual][manual] for more information):

![](https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/flycheck-annotated.png)

Out of the box Flycheck supports over 40 different programming languages with
more than 80 different syntax checking tools, and comes with a simple interface
to define new syntax checkers.

Many [3rd party extensions](http://flycheck.org/extensions.html) provide
new syntax checkers and other features like alternative error displays or mode
line indicators.

[COPYING]: https://github.com/flycheck/flycheck/blob/master/COPYING
[manual]: http://www.flycheck.org/manual/latest/index.html
[logo]: https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/logo.png

## Installation ##

Flycheck needs GNU Emacs 24 on a Unix system, and additionally **external**
syntax checking programs for the languages you use.  See [Supported Languages][]
for more information.

**Flycheck does not officially support Windows**, but tries to maintain Windows
compatibility and should generally work fine on Windows, too.  However, we can
neither answer questions about Windows nor fix bugs that only occur on Windows
without the help of active Windows users.  Please watch out for
[known Windows issues][windows]!

Install Flycheck from [MELPA](http://melpa.org) (development snapshots) or
[MELPA Stable](http://stable.melpa.org) (releases) with:

    M-x package-install RET flycheck

Then add the following to your init file:

```lisp
(add-hook 'after-init-hook #'global-flycheck-mode)
```

If you don’t know where your init file is, look at the value of the variable
`user-init-file` with `C-h v user-init-file`.

See [Installation][] for more information, and [Quick start][] for a quick tour
through Flycheck’s setup and major features.

[Supported Languages]: http://www.flycheck.org/manual/latest/Supported-languages.html#Supported-languages
[Installation]: http://www.flycheck.org/manual/latest/Installation.html#Installation
[Quick start]: http://www.flycheck.org/manual/latest/Quickstart.html#Quickstart
[windows]: https://github.com/flycheck/flycheck/labels/B-Windows%20only

## Support & Contribution ##

Please ask questions about Flycheck on [Stack Exchange][sx] or in our
[Gitter chat][gitter].  We try to answer all questions as fast and as precise as
possible.

To report problems or bugs, please use our [issue tracker][].  Our
[contribution guidelines][contrib] help you to create good bug reports; please
take a look.

We welcome patches and pull requests that fix bugs or provide new features.
Please read our [Contribution guidelines][contrib] for help and guidelines
before submitting pull requests.  When making larger changes to Flycheck or
implementing new features we recommend that you first open a separate issue or
ask in our [Gitter channel][gitter] to discuss you intended changes.

All contributors and all participants in our communication channels are expected
to follow our [Code of Conduct][coc].

[sx]: https://emacs.stackexchange.com/questions/tagged/flycheck
[gitter]: https://gitter.im/flycheck/flycheck
[Issue Tracker]: https://github.com/flycheck/flycheck/issues
[contrib]: https://github.com/flycheck/flycheck/blob/master/CONTRIBUTING.md
[Waffle Board]: http://waffle.io/flycheck/flycheck
[coc]: https://github.com/flycheck/flycheck/blob/master/CONDUCT.md

## License ##

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
