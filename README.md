Flycheck [![Build Status](https://travis-ci.org/flycheck/flycheck.png?branch=master)](https://travis-ci.org/flycheck/flycheck)
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

![Screenshot of Flycheck in action](https://github.com/flycheck/flycheck/raw/master/doc/screenshot.png)

The screenshot shows Flycheck on Emacs 24.3.50 with the awesome [Zenburn][]
color theme and the great
[Source Code Pro](https://github.com/adobe/source-code-pro).


Features
--------

- Automatic on-the-fly syntax checking while editing
- Fully automatic selection of the best syntax checker
- Optional manual selection of a syntax checker with `flycheck-select-checker`
  at `C-c ! s`
- Built-in syntax checkers for:
  - C/C++
  - CoffeeScript
  - CSS
  - D
  - Elixir
  - Emacs Lisp
  - Erlang
  - Go
  - Haml
  - Handlebars
  - Haskell
  - HTML
  - Javascript
  - JSON
  - LESS
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
  - Slim
  - Shell scripts (POSIX Shell, Bash and Zsh)
  - TeX/LaTeX
  - XML
  - YAML
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

- [flycheck-cask](https://github.com/flycheck/flycheck-cask) makes Flycheck use
  Cask packages in Cask projects.
- [flycheck-color-mode-line][] colors the mode line according to the Flycheck
  status.
- [flycheck-d-unittest][] adds a Flycheck checker to run unit tests for D
  programs on the fly.


Installation
------------

Install the ELPA package from [MELPA](http://melpa.milkbox.net/#/flycheck) or
[Marmalade](http://marmalade-repo.org/packages/flycheck).

In your [`Cask` file](https://github.com/cask/cask):

```lisp
(source gnu)
(source melpa)

(depends-on "flycheck")
```


Flycheck supports Emacs 24.  It is tested with Emacs 24.3, and Emacs snapshot
builds.  It should work with GNU Emacs 24.1 and 24.2 as well, but it is not
tested against these versions.  If it does not work with these versions, please
report an issue.

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
[solarized][] themes, or the fancy [Zenburn][] theme.

Use `C-c ! n` and `C-c ! p` to navigate between errors.  If the point is on an
error, the error message is shown in the echo area or in a popup buffer after a
short delay.  You may also hover the mouse over a highlighted error to get a
tooltip with the error message.

Read the complete manual with `C-c ! i` or `M-x flycheck-info`.


Credits
-------

The following people contributed to flycheck:

- [Bozhidar Batsov](https://github.com/bbatsov) provided valuable feedback and
  refinements, brought Flycheck to a larger user base by adding it to his
  awesome [Prelude](https://github.com/bbatsov/prelude) project, and added a
  Ruby syntax checker using `rubocop`.
- [Damon Haley](https://github.com/dhaley) helped to shape and test the PHP
  CodeSniffer checker.
- [David Holm](https://github.com/dholm) added C/C++ syntax and style checkers
  using `clang` and `cppcheck` respectively.
- [Gereon Frey](https://github.com/gfrey) fixed the `go-build` syntax checker
  and improve its tests.
- [Jimmy Yuen Ho Wong](https://github.com/wyuenho) added the HTML syntax checker
  and the jshint Javascript checker, and did valuable testing and bug fixing.
- [Krzysztof Witkowski](https://github.com/kwitek) implemented `eval` support in
  Flycheck commands.
- [Magnar Sveen](https://github.com/magnars) developed the awesome
  [dash.el](https://github.com/magnars/dash.el) and
  [s.el](https://github.com/magnars/s.el) libraries, that drive considerable
  parts of Flycheck's internals.
- [Marian Schubert](https://github.com/maio) added the Perl syntax checker.
- [Mark Hellewell](https://github.com/markhellewell) added the Puppet syntax and
  style checkers.
- [Martin Grenfell](https://github.com/scrooloose) created the awesome Vim
  library [syntastic](https://github.com/scrooloose/syntastic) which inspired
  this project and many of its checkers.
- [Matthias Dahl](https://github.com/Binarykhaos) improved the performance of
  Flycheck's temp file handling.
- [Peter Vasil](https://github.com/ptrv) contributed syntax checkers for XML,
  Lua and Go (using `go build` and `go test`), added unit tests and did valuable
  testing.
- [Robert Dallas Gray](https://github.com/rdallasgray) made error display
  customizable with `flycheck-display-errors-function`.
- [Robert Zaremba](https://github.com/robert-zaremba) added a Go syntax checker
  using `gofmt`.
- [steckerhalter](https://github.com/steckerhalter) provided the PHP CodeSniffer
  checker.
- [Steve Purcell](https://github.com/purcell) implemented many checkers,
  contributed important ideas to the design of the checker API and engaged in
  worthwhile discussion to shape this project.
- [Sylvain Benner](https://github.com/syl20bnr) added syntax checkers for Elixir
  and Erlang, and wrote the cool [flycheck-color-mode-line][] extension.
- [Sylvain Rousseau](https://github.com/thisirs) added a syntax checker for
  POSIX shell script using `bash`, and improved error parsing in the Bash script
  syntax checker.
- [tom tan](https://github.com/tom-tan) added a syntax checker for the D
  programming language using `dmd`, and wrote the cool [flycheck-d-unittest][]
  extension.
- [Yannick Roehlly](https://github.com/yannick1974) added support for PEP8
  naming errors to the Flake8 syntax checker.
- [Yasuyuki Oka](https://github.com/yasuyk) added syntax checkers for YAML
  (using Ruby's standard YAML parser), Javascript (using Google's Closure
  linter), Slim and PHP (using PHP Mess Detector)
- [Victor Deryagin](https://github.com/vderyagin) added the Rust syntax checker.


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

See [COPYING](https://github.com/flycheck/flycheck/blob/master/COPYING) for
details.

[zenburn]: https://github.com/bbatsov/zenburn-emacs
[solarized]: https://github.com/bbatsov/solarized-emacs
[flycheck-color-mode-line]: https://github.com/flycheck/flycheck-color-mode-line
[flycheck-d-unittest]: https://github.com/flycheck/flycheck-d-unittest
