Flycheck
========

Flycheck provides on-the-fly syntax checking for GNU Emacs.  Essentially it's
“flymake done right” with

- major-mode based checkers (instead of file name patterns),
- simple declarative checker definitions (instead of init functions)
- ready-to-use syntax checkers for a bunch of languages (instead of broken
  checkers using non-existing tools)
- a clean, concise and understandable implementation (instead of a almost 2k
  line mess of spaghetti code).

And this is how it looks like (the color theme is [solarized-light][]):

![Screenshot of Flycheck in action](https://github.com/lunaryorn/flycheck/raw/master/screenshot.png)


Features
--------

- Automatic syntax check after saving or insertion of new lines
- Error navigation with `next-error` and `previous-error`
- Built-in syntax checkers for:
  - CoffeeScript
  - CSS
  - Emacs Lisp
  - HAML
  - HTML
  - Javascript
  - JSON
  - Lua
  - Perl
  - PHP
  - Python
  - Ruby
  - SASS
  - Shell scripts (Bash, Dash and Zsh)
  - TeX/LaTeX
  - XML
- Easy customization (see `flycheck-checkers`)
- Easy declarative API to define new syntax checkers


Installation
------------

Install the ELPA package from [MELPA][] (bleeding edge snapshots) or
[Marmalade][] (stable releases) with `M-x package-install RET flycheck`.  All
dependencies are automatically installed.

Or [download][] the latest release and install `flycheck.el` with `M-x
package-install-file`.  In this case you also need to install the following
dependencies manually:

- [dash.el][]
- [s.el][]

The library is written and tested against GNU Emacs 24 and may or may not work
in earlier versions of GNU Emacs.

Most checkers have dependencies against external tools that perform the
checking.  See [Checkers](#checkers) for details.


Usage
-----

Enable `flycheck-mode` in your `init.el` file.

```scheme
;; Enable flymake for all files
(add-hook 'find-file-hook 'flycheck-mode-on)
;; Enable flymake for Python only
(add-hook 'python-mode-hook 'flycheck-mode-on)
```

Or do `M-x flycheck-mode` manually after visiting a file.

With `flycheck-mode` enabled the buffer is automatically checked on-the-fly.
You can also manually check the buffer with `M-x flycheck-buffer`.

### Syntax checker selection

By default a suitable checker is automatically selected from `flycheck-checkers`
(see [Customization](#customization)).  You can select a specific checker for
the current buffer with `M-x flycheck-select-checker` or by configuring the
file-local variable `flycheck-checker`:

```python
# Local Variables:
# flycheck-checker: flycheck-checker-python-pylint
# End:
```

Now **only** the [pylint][] checker will be used for the file.  If the checker
from `flycheck-checker` or `flycheck-select-checker` cannot be used for the
current buffer (e.g. the major mode does not match, the checker does not exist,
etc.) an error is signaled.

### Checker configuration

Some checkers can be configured from configuration files.  Such checkers have an
associated variable providing the name of the configuration file, for instance
`flycheck-jslintrc` for `flycheck-checker-javascript-jslint`.  These variables
obey the following rules:

If it contains a plain file name without any slash, e.g. `.jslintrc`,
this file is searched in the buffer's directory, any ancestors thereof, and the
home directory eventually.  If the buffer has no backing file, only the home
directory is searched.

If it contains a path, e.g. `./.jshintrc` or `$HOME/.jshintrc`, the
path is expanded against the buffer's directory (using `expand-file-name`).

If the configuration file is found and exists, it is passed to the checker
invocation.  Otherwise it is simply ignored.

You can get a list of all these variables with `M-x customize-group RET
flycheck-config-files`.  Here you can also change their default values.

These variables may also be used as file-local variables, e.g.

```scheme
// Local Variables:
// flycheck-jshintrc: "../.jshintrc"
// End:
```

This file is now always checked using `.jshintrc` from the parent directory.


### Error reporting

Errors and warnings from the checker are reported in the mode line (see
[Mode line](#mode-line)) and highlighted in the buffer with
`flycheck-error-face` and `flycheck-warning-face` respectively.  By default only
the error column is highlighted if an error refer to a specific column only.
Customize `flycheck-ignore-columns` (see [Customization](#customization)) to
always highlight the whole line.

**Note:** The default faces provided by Emacs are ill-suited, because they are
relatively pale and do not specify a background color or underline.  Hence they
are easily overlooked and – even worse – **cannot highlight spaces**.  For best
error highlighting, **customize** the Flycheck faces and add a background color
or underline, or – even better – choose a color theme that has reasonable
Flycheck faces, for instance the light or dark [solarized][] theme.

Additionally indicators are placed in the fringe to highlight the lines in which
errors occurred.  **The fringe must be at least 8 pixels wide** for the
indicators to be displayed properly.

To view an error message, either hover the mouse over the highlighted error, or
move the cursor to the highlighted error and wait a short moment.  To clear all
error information use `M-x flycheck-clear`.

### Mode line

Flycheck indicates its state in the mode line:

- `FlyC`: No errors in the current buffer.
- `FlyC*`: The syntax check is currently running.
- `FlyC:3/4`: There are three errors and four warnings in the current buffer.
- `FlyC!`: The syntax check failed.  Inspect the `*Messages*` buffer for
  details.
- `FlyC?`: The syntax check had a dubious result.  The definition of the checker
  might be flawed.  Inspect the `*Messages*` buffer for details.


Checkers
--------

You need to install external utilities for the following checkers:

- CoffeeScript: Install [coffeelint][].
- CSS: Install [csslint][].
- HAML: Install [HAML][].
- HTML: Install [Tidy][].
- Javascript: Install [jsl][] or [jshint][].
- JSON: Install [jsonlint][].
- Lua: Install [Lua][].
- Perl: Install [Perl][].
- PHP: Install the [PHP command line][php].
- Python: Install [flake8][], [pyflakes][] or [pylint][].
- Ruby: Install Ruby.
- SASS: Install [SASS][].
- Shell scripts: Install Bash or Zsh depending on the type of shell file you
  want to check.
- TeX/LaTeX: Install [chktex][] or [lacheck][].  Most TeX distributions,
  including TeXLive and MacTeX, already do this for you.
- XML: Install [xmlstarlet][].


Customization
-------------

`M-x customize-group RET flycheck`

### Checker selection

- `M-x customize-variable RET flycheck-checkers`: A list of all checkers. The
  checkers are tried in the order of appearance in this list.  The first checker
  that supports the current mode and whose executable exists is used.  To change
  the preference of checkers, change their order in this list or remove entries
  from this list.

Some modes have multiple checkers.  For instance `python-mode` has three
checkers using `flake8`, `pylint` or `pyflakes`.  When doing syntax checking in
`python-mode`, the checkers are tried in this order and the first whose
executable is found is used.

To change the order of preference or enforce a single checker, just modify the
order of their appearance in `flycheck-checkers` or delete checkers you do not
want to use.  For instance, to always use `pyflakes` in `python-mode`, just
remove `flycheck-checker-python-flake8` and `flycheck-checker-python-pylint`
from `flycheck-checkers` via customization.

Alternatively use `flycheck-select-checker` to select a specific checker for a
buffer.  For instance, to use `pyflakes` as checker in the current buffer, use
`M-x flycheck-select-checker RET python-checker-python-pyflakes`.


### Checker configuration

`M-x customize-group RET flycheck-config-files` customizes the names and paths
of configuration files for syntax checkers.


### Appearance

- `M-x customize-variable RET flycheck-ignore-columns`: Customize whether
  Flycheck takes column numbers into account when highlighting errors.
- `M-x customize-face RET flycheck-error-face`: Customize the face for error
  highlights.  Inherits from `flymake-errline` by default.
- `M-x customize-face RET flycheck-warning-face`: Customize the face for error
  highlights.  Inherits from `flymake-warnline` by default.


### Hooking

- `M-x customize-variable RET flycheck-mode-hook`: Customize functions to run
  after `flycheck-mode` is enabled.
- `M-x customize-variable RET flycheck-after-syntax-check-hook`: Customize
  functions to run after each syntax check.


Extending
---------

Syntax checkers are special symbols declared with `flycheck-declare-checker`.
This function takes a symbol and some keyword arguments that describe the
checker:

- `:command` (*mandatory*): A list containing the *executable* of the syntax
  checking tool (in the `car` of the list) and its *arguments* (in the
  `cdr`). Before enabling a checker **the executable is checked for existence**
  with `executable-find`.  If this check fails the checker is **not** used.  In
  *arguments* the following special symbols and tags are replaced:

  - `source`: The source file to check.  A temporary file with the contents of
    the buffer to check, created in the **system temporary directory**.
  - `source-inplace`: The source file to check.  A temporary file with the
    contents of the buffer to check, but created in the **same directory** as
    the original file.  If the buffer to be checked has no `buffer-file-name`,
    this is the same as `source`.
  - `(config OPTION-NAME VARIABLE-NAME)`: Find and pass a config file to the
    checker.  `OPTION-NAME` is a string containing the name of the option that
    understood by the checker.  `VARIABLE-NAME` is a symbol referring to a
    variable from which to take the configuration file.  Use
    `flycheck-def-config-file-var` to define this variable (see `C-h f
    flycheck-def-config-file-var` for more information).

- `:error-patterns` (*mandatory*): A list of error patterns to parse the output
  of `:command`.  Each pattern has the form `(REGEXP LEVEL)`:

  - `REGEXP` is a regular expression that matches a single error or warning.  It
    may match a **multi-line** string.  The expression may provide the following
    match groups:

     - Group **1**: The file name
     - Group **2**: The line number
     - Group **3**: The column number
     - Group **4**: The error text

     Each of these groups is optional, however error messages without line
     numbers will be ignored.  Use *explicitly numbered groups*
     (i.e. `\(?1:foo\)`).

  - `LEVEL` is either `warning` or `error` and indicates the **severity of this
    error**.

  **All** patterns are applied in the order of declaration to the whole** output
  **of the checker.
- `:modes` (*optional*): A single major mode symbol or a list thereof.  If given
  the checker will only be used in any of these modes.
- `:predicate` (*optional*): A form that if present is evaluated to determine
  whether the checker is to be used.  The checker is only used if the form
  evaluates to non-nil.

**At least one** of `:modes` and `:predicate` must **be present**.  If **both**
are present, **both** must match for the checker to be used.

See `C-h f flycheck-declare-checker` for details.

Checkers are registered via `flycheck-checkers`, which is a list of symbols.
Each symbol in this list must be a checker declared with
`flycheck-declare-checker`.


### Examples

#### A simple example

Let's see this in action by explaining the definition of a [Python][] checker
included in flycheck.  This checker uses the [pylint][] utility to perform the
actual syntax check.

First we declare the checker properties:

```scheme
(flycheck-declare-checker flycheck-checker-python-pylint
  :command '("epylint" source-inplace)
  :error-patterns
  '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): Warning (W.*): \\(?4:.*\\)$" warning)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): Error (E.*): \\(?4:.*\\)$" error)
    ("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\[F\\] \\(?4:.*\\)$" error))
  :modes 'python-mode)
```

We specify the command to execute in this mode in `:command`.  It is a
straight-forward list with the executable name as first element and some
subsequent arguments.  The symbol `source-inplace` is automatically replaced
with the name of the file to check.  We use `source-inplace` to check the file
at its original location so that `pylint` can take the Python package
structure of the source tree into account.  Note that a checker is not enabled
if its executable does not exist (as by `executable-find`).

Next we give a list of error patterns to extract error location and message from
the `epylint` output.  An error pattern is a list containing a regular
expression that matches the error and extracts error information in match
groups, and an error level (either `warning` or `error`).  As you can see
`epylint` emits both errors and warnings.

Eventually we declare that the checker is to be used in `python-mode`.

Now we only need to register this error checker for use with

```scheme
(add-to-list 'flycheck-checkers 'flycheck-checker-python-pylint)
```

Assuming that `flycheck-mode` is enabled (see [Usage](#usage)), Python source
code will now be syntax-checked on the fly in `python-mode`.

#### Predicates for checkers

Some checkers have more complicated conditions for whether they are to be used
or not.  For instance, syntax checking in `sh-mode` needs to use different
shells depending on the value of `sh-shell`.  Hence in the checkers for this
mode we also give a `:predicate` that determines whether the right shell is
active:

```scheme
(flycheck-declare-checker flycheck-checker-zsh
  :command '("zsh" "-n" "-d" "-f" source)
  :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
  :modes 'sh-mode
  :predicate '(eq sh-shell 'zsh))
```

First note that unlike with `pylint` we use `source` instead of `source-inplace`
to check the file from a proper temporary copy because Zsh syntax checking does
not need information from the original source tree.

We declare that the checker is to be used in `sh-mode` **and** if a Zsh shell
script is being edited.  The `predicate` is simply an Emacs Lisp form that is
evaluated whenever flycheck tries to use the checker for the current buffer. If
both `:modes` and `:predicate` are given, **both** must match for the checker to
be used.

#### Configuration files for checkers

Some checkers accept a configuration file as argument that controls how the
checker finds and reports errors.  Flycheck provides built-in support to pass
configuration files to syntax checkers.  An example is the `jshint` checker for
JavaScript:

```scheme
(flycheck-def-config-file-var flycheck-jshintrc
    flycheck-checker-javascript-jshint ".jshintrc")

(flycheck-declare-checker flycheck-checker-javascript-jshint
  :command '("jshint" (config "--config" flycheck-jshintrc) source)
  :error-patterns
  '(("^\\(?1:.*\\): line \\(?2:[0-9]+\\), col \\(?3:[0-9]+\\), \\(?4:.+\\)$"
     error))
  :modes 'js-mode)
```

First we declare a variable that provides the name or path of the configuration
file.  This variable gets an appropriate docstring, added to the customization
interface and marked as safe local variable for strings.

In the checker declaration we use this variable in a special `config` element in
the arguments of the checker.  This element is a triple, and its second item is
the option by which to pass the configuration file to the checker.  If the
configuration file is found it is passed to the checker, e.g. `jshint --config
/path/to/.jshintrc /the/file/to/check`.  Otherwise the whole element is simply
omitted.


Further help
------------

- `C-h f flycheck-mode`
- `C-h f flycheck-buffer`
- `C-h f flycheck-select-checker`
- `C-h v flycheck-checkers`
- `C-h v flycheck-checker`
- `C-h f flycheck-declare-checker`
- `C-h f flycheck-def-config-file-var`


Credits
-------

The following people contributed to flycheck:

- [Jimmy Yuen Ho Wong][wyuenho] added the HTML syntax checker and the jshint
  Javascript checker, and did valuable testing and bug fixing.
- [Marian Schubert][maio] added the Perl syntax checker.
- [Martin Grenfell][scrooloose] created the awesome Vim library [syntastic][]
  which inspired this project and many of its checkers.
- [Peter Vasil][ptrv] created the XML and Lua syntax checkers.
- [Steve Purcell][purcell] implemented many checkers, contributed important
  ideas to the design of the checker API and engaged in worthwhile discussion to
  shape this project.


License
-------

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
Street, Fifth Floor, Boston, MA 02110-1301, USA.

See [COPYING][] for details.


[solarized-light]: https://github.com/bbatsov/solarized-emacs
[melpa]: http://melpa.milkbox.net
[marmalade]: http://marmalade-repo.org/
[download]: https://github.com/lunaryorn/flycheck/tags
[dash.el]: https://github.com/magnars/dash.el
[s.el]: https://github.com/magnars/s.el
[solarized]: https://github.com/bbatsov/solarized-emacs
[python]: http://python.org
[syntastic]: https://github.com/scrooloose/syntastic
[scrooloose]: https://github.com/scrooloose
[purcell]: https://github.com/purcell
[wyuenho]: https://github.com/wyuenho
[maio]: https://github.com/maio
[ptrv]: https://github.com/ptrv
[copying]: https://github.com/lunaryorn/flycheck/blob/master/COPYING

[coffeelint]: http://www.coffeelint.org/
[csslint]: https://github.com/stubbornella/csslint
[haml]: http://haml.info
[tidy]: https://github.com/w3c/tidy-html5
[jsl]: http://www.javascriptlint.com/
[jshint]: http://www.jshint.com
[jsonlint]: https://github.com/zaach/jsonlint
[lua]: http://www.lua.org/
[perl]: http://www.perl.org/
[php]: http://php.net/manual/en/features.commandline.php
[flake8]: http://pypi.python.org/pypi/flake8
[pyflakes]: http://pypi.python.org/pypi/pyflakes
[pylint]: http://pypi.python.org/pypi/pylint
[sass]: http://sass-lang.com
[chktex]: http://baruch.ev-en.org/proj/chktex/
[lacheck]: http://www.ctan.org/pkg/lacheck
[xmlstarlet]: http://xmlstar.sourceforge.net/
