Flycheck
========

Flycheck is “flymake done right” with

- an improved customization interface based on major modes (instead of file name
  patterns),
- a much nicer and easier, declarative syntax for checker definitions (instead
  of init functions)
- a bunch of ready-to-use syntax checkers for various languages (instead of
  broken checkers using non-existing tools)
- and a clean, concise and understandable implementation (instead of a almost 2k
  line mess of spaghetti code).


Motivation
----------

Get rid of flymake which is utterly broken.


Features
--------

- Automatic syntax check after saving or insertion of new lines
- Built-in syntax checkers for:
  - CoffeeScript
  - CSS
  - Emacs Lisp
  - HAML
  - HTML
  - Javascript
  - JSON
  - Perl
  - PHP
  - Python
  - Ruby
  - SASS
  - Shell scripts (Bash, Dash and Zsh)
  - TeX/LaTeX
  - XML
  - Lua
- Easy customization (see `flycheck-checkers`)
- Easy declarative API to define new syntax checkers

Compared to flymake these features are currently **missing**:

- Jump to next/previous error
- Show error message under cursor (as provided by flymake-cursor)


Installation
------------

Install the ELPA package from [MELPA][] (bleeding edge snapshots) or
[Marmalade][] (stable releases) with `M-x package-install RET flycheck`.

Or [download][] the latest release and install `flycheck.el` with `M-x
package-install-file`.

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

Errors and warnings from the checker are reported in the mode line (see
[Mode line](#mode-line)) and highlighted in the buffer.  To clear all error
information use `M-x flycheck-clear`.

By default a suitable checker is automatically selected from `flycheck-checkers`
(see [Customization](#customization)).  You can select a specific checker for
the current buffer with `M-x flycheck-select-checker` or by configuring the
file-local variable `flycheck-checker`:

```scheme
;; Local Variables:
;; flycheck-checker: flycheck-checker-python-pylint
;; End:
```

Now **only** the [pylint][] checker will be used for the file.  If the checker
from `flycheck-checker` or `flycheck-select-checker` cannot be used for the
current buffer (e.g. the major mode does not match, the checker does not exist,
etc.) an error is signaled.


Mode line
---------

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
- Javascript: Install [jshint][] or [jslint][].  The former will
  automatically use a `.jshintrc` file in the buffer's directory, any ancestor
  thereof or your `$HOME` directory.  You may override the path to this file
  globally or per buffer with the variable `flycheck-jshintrc`.
- JSON: Install [jsonlint][].
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
- Lua: Install [lua][].


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

To chance the order of preference or enforce a single checker, just modify the
order of their appearance in `flycheck-checkers` or delete checkers you do not
want to use.  For instance, to always use `pyflakes` in `python-mode`, just
remove `flycheck-checker-python-flake8` and `flycheck-checker-python-pylint`
from `flycheck-checkers` via customization.

Alternatively use `flycheck-select-checker` to select a specific checker for a
buffer.  For instance, to use `pyflakes` as checker in the current buffer, use
`M-x flycheck-select-checker RET python-checker-python-pyflakes`.


### Appearance

- `M-x customize-variable RET flycheck-error-indicator`: Customize the indicator
  at the beginning of lines containing *errors*.  Defaults to `"⚠"`, set to
  `nil` to disable indicators for error messages..
- `M-x customize-variable RET flycheck-warning-indicator`: Customize the
  indicator at the beginning of lines containing *warnings*.  Defaults to `nil`,
  i.e. the indicator is disabled.
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

In flycheck a syntax checker is a [property list][] with the following keys (the
*checker properties*):

- `:command` (*mandatory*): A list containing the executable of the syntax
  checking tool (in the `car` of the list) and its arguments (in the
  `cdr`). Before enabling a checker **the executable is checked for existence**
  with `executable-find`.  If this check fails the checker is **not** used.  In
  arguments the special symbol `source` is replaced with a **temporary copy of
  the source file**, created in the system temporary directory.  Use
  `source-inplace` instead to force the copy being created in the **same
  directory as the original source file**.  **Prefer** `source` over
  `source-inplace` if possible.
- `:error-patterns` (*mandatory*): A list of error patterns to parse the output
  of `:command`.  Each pattern has the form `(REGEXP FILE-IDX LINE-IDX COL-IDX
  ERR-TEXT-IDX LEVEL)`:

  - `REGEXP` is a regular expression that matches a single
    error or warning.  It may match a **multi-line** string.
  - `FILE-IDX`, `LINE-IDX`, `COL-IDX` and `ERR-TEXT-IDX` are **indexes of match
    groups** that provide the file name, the line number, the column number and
    the error message respectively.  Each of these may be nil to indicate that
    the message does not provide the corresponding information.
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

Checkers are registered via `flycheck-checkers`, which is a list of symbols.
Each symbol in this list must either be a **variable bound to a checker property
list**, or be a **function returning one**.  In the former case, the variables
value is **retrieved anew on each syntax check**.  In the latter case the
function is **invoked on each syntax check with no arguments**.


### Example

Let's see this in action by explaining the definition of a [Python][] checker
included in flycheck.  This checker uses the [pylint][] utility to perform the
actual syntax check.

First we declare the checker properties:

```scheme
(defvar flycheck-checker-python-pylint
  '(:command
    ("epylint" source-inplace)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\): Warning (W.*): \\(.*\\)$" 1 2 nil 3 warning)
     ("^\\(.*\\):\\([0-9]+\\): Error (E.*): \\(.*\\)$" 1 2 nil 3 error)
     ("^\\(.*\\):\\([0-9]+\\): \\[F\\] \\(.*\\)$" 1 2 nil 3 error))
    :modes python-mode))
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
expression that matches the error, indexes of groups that match the file name,
the line number, the column number and the error message respectively, and an
error level (either `warning` or `error`).  As you can see `epylint` emits both
errors and warnings.

Eventually we declare that the checker is to be used in `python-mode`.

Now we only need to register this error checker for use with

```scheme
(add-to-list 'flycheck-checkers 'flycheck-checker-python-pylint)
```

Assuming that `flycheck-mode` is enabled (see [Usage](#usage)), Python source
code will now be syntax-checked on the fly in `python-mode`.

Some checkers have more complicated conditions for whether they are to be used
or not.  For instance, syntax checking in `sh-mode` needs to use different
shells depending on the value of `sh-shell`.  Hence in the checkers for this
mode we also give a `:predicate` that determines whether the right shell is
active:

```scheme
(defvar flycheck-checker-zsh
  '(:command
    ("zsh" "-n" "-d" "-f" source)
    :error-patterns
    (("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3 error))
    :modes sh-mode
    :predicate (eq sh-shell 'zsh)))
```

First note that unlike with `pylint` we use `source` instead of `source-inplace`
to check the file from a proper temporary copy because Zsh syntax checking does
not need information from the original source tree.

We declare that the checker is to be used in `sh-mode` **and** if a Zsh shell
script is being edited.  The `predicate` is simply an Emacs Lisp form that is
evaluated whenever flycheck tries to use the checker for the current buffer. If
both `:modes` and `:predicate` are given, **both** must match for the checker to
be used.


Further help
------------

- `C-h f flycheck-mode`
- `C-h f flycheck-buffer`
- `C-h f flycheck-select-checker`
- `C-h v flycheck-checkers`
- `C-h v flycheck-checker`


Credits
-------

The following people contributed to flycheck:

- [Jimmy Yuen Ho Wong][wyuenho] added the HTML syntax checker and the jshint
  Javascript checker, and did valuable testing and bug fixing.
- [Marian Schubert][maio] added the Perl syntax checker.
- [Martin Grenfell][scrooloose] created the awesome Vim library [syntastic][]
  which inspired this project and many of its checkers.
- [Peter Vasil][ptrv] created the XML and Lua syntax checker.
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


[melpa]: http://melpa.milkbox.net
[marmalade]: http://marmalade-repo.org/
[download]: https://github.com/lunaryorn/flycheck/tags
[property list]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html#Property-Lists]
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
[jshint]: http://www.jshint.com
[jslint]: http://www.jslint.com/
[jsonlint]: https://github.com/zaach/jsonlint
[perl]: http://www.perl.org/
[php]: http://php.net/manual/en/features.commandline.php
[flake8]: http://pypi.python.org/pypi/flake8
[pyflakes]: http://pypi.python.org/pypi/pyflakes
[pylint]: http://pypi.python.org/pypi/pylint
[sass]: http://sass-lang.com
[chktex]: http://baruch.ev-en.org/proj/chktex/
[lacheck]: http://www.ctan.org/pkg/lacheck
[xmlstarlet]: http://xmlstar.sourceforge.net/
[lua]: http://www.lua.org/