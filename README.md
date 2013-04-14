Flycheck [![Build Status](https://travis-ci.org/lunaryorn/flycheck.png?branch=master)](https://travis-ci.org/lunaryorn/flycheck)
========

Flycheck provides on-the-fly syntax checking for GNU Emacs 24.  Essentially it's
“flymake done right” with

- major-mode based checkers (instead of file name patterns),
- simple declarative checker definitions (instead of init functions),
- ready-to-use syntax checkers for a bunch of languages (instead of broken
  checkers using non-existing tools),
- and a clean, concise and understandable implementation (instead of a almost 2k
  line mess of spaghetti code).

And this is how it looks like, using the awesome [solarized-light][] color theme:

![Screenshot of Flycheck in action](https://github.com/lunaryorn/flycheck/raw/master/doc/screenshot.png)


Features
--------

- Automatic syntax check after saving or insertion of new lines
- Error navigation with `next-error` and `previous-error`
- Built-in syntax checkers for:
  - CoffeeScript
  - CSS
  - Emacs Lisp
  - Go
  - Haml
  - HTML
  - Javascript
  - JSON
  - Lua
  - Perl
  - PHP
  - Python
  - Ruby
  - Rust
  - RST (ReStructuredText)
  - Sass
  - SCSS
  - Shell scripts (Bash, Dash and Zsh)
  - TeX/LaTeX
  - XML
- Easy customization (see `flycheck-checkers`)
- Easy declarative API to define new syntax checkers


Installation
------------

Install the ELPA package from [MELPA][] or [Marmalade][] with `M-x
package-install RET flycheck`.  All dependencies are automatically installed.

Flycheck is written and tested against GNU Emacs 24.2 and newer.  It should work
on Emacs 24.1, too.  GNU Emacs 23 and before, and other flavors of Emacs
(e.g. XEmacs) are *not* supported.

Most checkers have dependencies against external tools that perform the
checking.  Use `C-c ! ?` to see what a checker needs, e.g. `C-c ! ?
python-pylint`.


Usage
-----

Once installed, enable Flycheck mode with `M-x flycheck-mode`.

To enable Flycheck mode in all buffers, in which it can be used, add the
following to your `init.el` file:

```scheme
(add-hook 'after-init-hook #'global-flycheck-mode)
```

In Flycheck mode the buffer will automatically be checked on the fly, if a
suitable syntax checker exists.  Use `C-c ! c` to start a syntax check manually.

A suitable syntax checker is automatically selected from the list of registered
checkers in `flycheck-checkers`.  Use  `C-c ! s` to manually select a specific
syntax checker, or configure the syntax checker per file by setting
`flycheck-checker` as file local variable::

```python
# Local Variables:
# flycheck-checker: python-pylint
# End:
```

Some syntax checkers read configuration files.  Use `M-x customize-group RET
flycheck-config-files` to customize these.  Refer to the section *Configuration*
in the Flycheck info manual.

Errors and warnings from the syntax checker are reported in the mode line,
highlighted in the buffer and indicated with icons in the fringe.  Customize
`flycheck-highlighting-mode` to change the highlighting of errors.

**Note:** The default highlighting faces provided GNU Emacs are ill-suited to
highlight errors.  They are too easily overlooked.  Make sure to customize these
faces to add a striking background color or an underlying, or choose a color
theme with reasonable Flycheck faces, for instance the excellent light or dark
[solarized][] themes.

Use `C-c ! n` and `C-c ! p` to navigate between errors.  If the point is on an
error, the error message is shown in the echo area or in a popup buffer after a
short delay.  You may also hover the mouse over a highlighted error to get a
tooltip with the error message.

Read the complete manual with `C-c ! i` or `M-x flycheck-info`.


Credits
-------

The following people contributed to flycheck:

- [Bozhidar Batsov][bbatsov] provided valuable feedback and refinements, and
  brought Flycheck to a larger user base by adding it to his awesome [Prelude][]
  project.
- [Damon Haley][dhaley] helped to shape and test the PHP CodeSniffer checker.
- [Jimmy Yuen Ho Wong][wyuenho] added the HTML syntax checker and the jshint
  Javascript checker, and did valuable testing and bug fixing.
- [Krzysztof Witkowski][kwitek] implemented `eval` support in Flycheck commands.
- [Marian Schubert][maio] added the Perl syntax checker.
- [Martin Grenfell][scrooloose] created the awesome Vim library [syntastic][]
  which inspired this project and many of its checkers.
- [Peter Vasil][ptrv] contributed syntax checkers for XML, Lua and Go (using `go
  build` and `go test`), added unit tests and did valuable testing.
- [Robert Zaremba][robert-zaremba] added a Go syntax checker using `gofmt`.
- [steckerhalter][] provided the PHP CodeSniffer checker.
- [Steve Purcell][purcell] implemented many checkers, contributed important
  ideas to the design of the checker API and engaged in worthwhile discussion to
  shape this project.
- [Yannick Roehlly][yannick1974] added support for PEP8 naming errors to the
  Flake8 syntax checker.
- [Victor Deryagin][vderyagin] added the Rust syntax checker.


License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [COPYING][] for details.


[solarized-light]: https://github.com/bbatsov/solarized-emacs
[melpa]: http://melpa.milkbox.net
[marmalade]: http://marmalade-repo.org/
[download]: https://github.com/lunaryorn/flycheck/tags
[dash.el]: https://github.com/magnars/dash.el
[s.el]: https://github.com/magnars/s.el
[solarized]: https://github.com/bbatsov/solarized-emacs
[python]: http://python.org
[checkstyle]: http://checkstyle.sourceforge.net/
[bbatsov]: https://github.com/bbatsov
[prelude]: https://github.com/bbatsov/prelude
[dhaley]: https://github.com/dhaley
[syntastic]: https://github.com/scrooloose/syntastic
[scrooloose]: https://github.com/scrooloose
[purcell]: https://github.com/purcell
[wyuenho]: https://github.com/wyuenho
[kwitek]: https://github.com/kwitek
[maio]: https://github.com/maio
[ptrv]: https://github.com/ptrv
[robert-zaremba]: https://github.com/robert-zaremba
[steckerhalter]: https://github.com/steckerhalter
[yannick1974]: https://github.com/yannick1974
[copying]: https://github.com/lunaryorn/flycheck/blob/master/COPYING
[vderyagin]: https://github.com/vderyagin
