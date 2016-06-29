# [![Flycheck][logo]](http://www.flycheck.org) #

[![License GPL 3](https://img.shields.io/github/license/flycheck/flycheck.svg)][COPYING]
[![Join the chat](https://img.shields.io/gitter/room/flycheck/flycheck.svg)](https://gitter.im/flycheck/flycheck)
[![MELPA stable version](http://stable.melpa.org/packages/flycheck-badge.svg)](http://stable.melpa.org/#/flycheck)
[![MELPA version](http://melpa.org/packages/flycheck-badge.svg)](http://melpa.org/#/flycheck)
[![Build Status](https://img.shields.io/travis/flycheck/flycheck/master.svg)](https://travis-ci.org/flycheck/flycheck)

**Flycheck** is a modern on-the-fly syntax checking extension for GNU Emacs 24,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.

It uses various syntax checking and linting tools to
[automatically check the contents of buffers][checking] while you type, and
reports warnings and errors directly in the buffer or in an optional error list
(see [Flycheck manual][manual] for more information):

![](https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/flycheck-annotated.png)

Out of the box Flycheck supports over
[40 different programming languages][languages] with more than 80 different
syntax checking tools, and comes with a [simple interface][definitions] to
define new syntax checkers.

Many [3rd party extensions][extensions] provide new syntax checkers and other
features like alternative error displays or mode line indicators.

[COPYING]: https://github.com/flycheck/flycheck/blob/master/COPYING
[manual]: http://www.flycheck.org/en/latest/index.html#the-user-guide
[logo]: https://raw.githubusercontent.com/flycheck/flycheck/master/doc/_static/logo.png
[checking]: http://www.flycheck.org/en/latest/_downloads/flycheck.html#Checking-buffers
[languages]:http://www.flycheck.org/en/latest/languages.html
[definitions]: http://www.flycheck.org/en/latest/_downloads/flycheck.html#Defining-syntax-checkers
[extensions]: http://www.flycheck.org/en/latest/community/extensions.html

## Try out ##

Flycheck needs GNU Emacs 24.3 or newer, and works best on Unix systems.
**Windows users**, please be aware that Flycheck does not support Windows
officially, although it should mostly work fine on Windows.  See
[Windows support][] and watch out for [known Windows issues][windows issues]!

Install some [syntax checker tools][languages] and type the following in your
`*scratch*` buffer and run `M-x eval-buffer`:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(package-install 'flycheck)

(global-flycheck-mode)
```

For a more gentle introduction read the [Installation][] instructions and go
through [Quickstart][] guide.

[Installation]: http://www.flycheck.org/en/latest/user/installation.html
[Quickstart]: http://www.flycheck.org/en/latest/user/quickstart.html
[windows issues]: https://github.com/flycheck/flycheck/labels/B-Windows%20only
[windows support]: http://www.flycheck.org/en/latest/user/installation.html#windows-support

## Support & Contribution ##

Please ask questions about Flycheck on [Stack Exchange][sx] or in our
[Gitter chat][gitter].  We try to answer all questions as fast and as precise as
possible.

To report problems or bugs, please use our [issue tracker][].  Our
[Contributor’s Guide][contrib] helps you to create good bug reports; please take
a look.

We welcome patches and pull requests that fix bugs or provide new features.
Please read our [Contributor’s Guide][contrib] for help and guidance before
submitting pull requests.  When making larger changes to Flycheck or
implementing new features we recommend that you first open a separate issue or
ask in our [Gitter channel][gitter] to discuss you intended changes.

All contributors and all participants in our communication channels are expected
to follow our [Code of Conduct][coc].

[sx]: https://emacs.stackexchange.com/questions/tagged/flycheck
[gitter]: https://gitter.im/flycheck/flycheck
[Issue Tracker]: https://github.com/flycheck/flycheck/issues
[contrib]: http://www.flycheck.org/en/latest/contributor/contributing.html
[coc]: http://www.flycheck.org/en/latest/community/conduct.html

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

You may copy, distribute and/or modify the Flycheck documentation under the
terms of the Creative Commons Attribution-ShareAlike 4.0 International Public
License.  A copy of the license can be obtained at
<https://creativecommons.org/licenses/by-sa/4.0/legalcode>.

Permission is granted to copy, distribute and/or modify the Flycheck logo in
`/flycheck.svg` under the terms of the Creative Commons Attribution-ShareAlike
4.0 International Public License.  A copy of the license can be obtained at
<https://creativecommons.org/licenses/by-sa/4.0/legalcode>.
