Flycheck [![Build Status](https://travis-ci.org/lunaryorn/flycheck.png?branch=master)](https://travis-ci.org/lunaryorn/flycheck)
========

Flycheck (aka “Flymake done right”) is a modern on-the-fly syntax checking
extension for GNU Emacs 24 with:

- ready-to-use syntax checkers for a bunch of languages,
- easy customization,
- a comprehensive manual,
- a dead simple, declarative API to create new syntax checkers,
- major-mode based selection of syntax checkers,
- multiple syntax checkers per buffer,
- optional error list popup,
- a “doesn’t get in your way” guarantee,
- and a clean, concise and understandable implementation with decent test
  coverage.

![Screenshot of Flycheck in action](https://github.com/lunaryorn/flycheck/raw/master/doc/screenshot.png)

The screenshot was taken on Emacs 24.3 using the awesome [solarized-light][]
color theme and the great [Anonymous Pro][anon-pro] font.


Features
--------

- Automatic on-the-fly syntax checking while editing
- Fully automatic selection of the best syntax checker
- Optional manual selection of a syntax checker with `flycheck-select-checker`
  at `C-c ! s`
- Built-in syntax checkers for:
  - CoffeeScript
  - CSS
  - Elixir
  - Emacs Lisp
  - Erlang
  - Go
  - Haml
  - HTML
  - Javascript
  - JSON
  - Lua
  - Perl
  - PHP
  - Puppet
  - Python
  - Ruby
  - Rust
  - RST (ReStructuredText)
  - Sass
  - Scala
  - SCSS
  - Shell scripts (POSIX Shell, Bash and Zsh)
  - TeX/LaTeX
  - XML
- Nice error indication and highlighting
- Easy customization
- Syntax checker configuration with project-specific configuration files and
  options
- Error navigation with `next-error` and `previous-error`
- Error list with `flycheck-list-errors` at `C-c ! l`
- Declarative API to define new syntax checkers
- Error parsers for structured markup formats (e.g. Checkdoc XML)

### 3rd party extensions

The following extensions provide additional cool features for Flycheck:

- [flycheck-color-mode-line.el][] colors the mode line according to the Flycheck
  status.


Installation
------------

Install the ELPA package from [MELPA][] or [Marmalade][] with `M-x
package-install RET flycheck`.  All dependencies are automatically installed.

Flycheck supports GNU Emacs 24.2 and newer on Linux, OS X and any other decent
flavor of Unix.  It should work with GNU Emacs 24.1, too, but it is not tested
against this version of Emacs.

Flycheck does not support Windows, but tries to be compatible with it.  You may
try to use Flycheck on Windows.  It should mostly work, but expect problems and
issues.  Pull requests which improve Windows compatibility are welcome.

Flycheck also does not support GNU Emacs 23 and other flavors of Emacs
(e.g. XEmacs, Aquamacs, etc.).  Don't try, it will *not* work.

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

- [Bozhidar Batsov][bbatsov] provided valuable feedback and refinements, brought
  Flycheck to a larger user base by adding it to his awesome [Prelude][]
  project, and added a Ruby syntax checker using `rubocop`.
- [Damon Haley][dhaley] helped to shape and test the PHP CodeSniffer checker.
- [Jimmy Yuen Ho Wong][wyuenho] added the HTML syntax checker and the jshint
  Javascript checker, and did valuable testing and bug fixing.
- [Krzysztof Witkowski][kwitek] implemented `eval` support in Flycheck commands.
- [Magnar Sveen][magnars] developed the awesome [dash.el][] and [s.el][]
  libraries, that drive considerable parts of Flycheck's internals.
- [Marian Schubert][maio] added the Perl syntax checker.
- [Mark Hellewell][markhellewell] added the Puppet syntax and style checkers.
- [Martin Grenfell][scrooloose] created the awesome Vim library [syntastic][]
  which inspired this project and many of its checkers.
- [Matthias Dahl][BinaryKhaos] improved the performance of Flycheck's temp file
  handling.
- [Peter Vasil][ptrv] contributed syntax checkers for XML, Lua and Go (using `go
  build` and `go test`), added unit tests and did valuable testing.
- [Robert Zaremba][robert-zaremba] added a Go syntax checker using `gofmt`.
- [steckerhalter][] provided the PHP CodeSniffer checker.
- [Steve Purcell][purcell] implemented many checkers, contributed important
  ideas to the design of the checker API and engaged in worthwhile discussion to
  shape this project.
- [Sylvain Benner][syl20bnr] added syntax checkers for Elixir and Erlang, and
  wrote the cool [flycheck-color-mode-line.el][] extension.
- [Sylvain Rousseau][thisirs] added a syntax checker for POSIX shell script
  using `bash`, and improved error parsing in the Bash script syntax checker.
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
[anon-pro]: http://www.marksimonson.com/fonts/view/anonymous-pro
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
[magnars]: https://github.com/magnars
[dash.el]: https://github.com/magnars/dash.el
[s.el]: https://github.com/magnars/s.el
[thisirs]: https://github.com/thisirs
[syl20bnr]: https://github.com/syl20bnr
[flycheck-color-mode-line.el]: https://github.com/syl20bnr/flycheck-color-mode-line
[markhellewell]: https://github.com/markhellewell
[BinaryKhaos]: https://github.com/Binarykhaos
