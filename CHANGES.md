master
======

- New syntax checkers:

  - Go using `gofmt`

- Improvements:

  - Support McCabe warnings from `flake8`
  - Support warnings from `flake8` 2


0.7.1 (Feb 23, 2013)
====================

- Bug fixes:

  - Do not signal errors from `flycheck-mode` (#87)
  - Correctly fall back to `$HOME` when searching config files
  - Correctly ascend to parent directory when searching config files

- API changes:

  - Rename `config` cell to `config-file`
  - Add support for single-argument configuration files
  - Add support for evaluating arbitrary forms in syntax checker commands (#86)


0.7 (Feb 14, 2013)
==================

- New features:

  - Navigate to source from syntax checker help
  - Add online Info manual (#60)

- Improvements:

  - Use pipes instead of TTYs to read output from syntax checkers
  - Defer syntax checks for invisible buffers (#80)
  - Immediately display error messages after error navigation (#62)

- Bug fixes

  - Never select deleted buffers
  - Do not let the debugger interfere with necessary cleanup
  - Do not attempt to parse empty XML trees (#78)
  - Fix infinite recursion on Windows (#81, #82)


0.6.1 (Jan 30, 2013)
====================

- Fix package dependencies


0.6 (Jan 29, 2013)
==================

- New syntax checkers:

  - Emacs Lisp with Checkdoc (#53, #73)
  - PHP with PHP CodeSniffer (#71)

- Removed syntax checkers:

  - Javascript with `jsl`

- New features:

  - Error navigation with `next-error` and `previous-error` (#26)
  - Fringe icons instead of error indicators (#33, #57)
  - Menu entry for Flycheck (#59)
  - Customizable error highlighting, taking into account the column number (#35)
  - Configuration files for syntax checkers
  - Allow to compile a buffer with a syntax checker (#58)
  - Use multiple syntax checkers during a syntax check (#31)

- Improvements:

  - Match error patterns in order of declaration (#55)

- Bug fixes:

  - Inherit highlighting faces from built-in faces (#24, #43)
  - Correct error patterns of the HTML syntax checker (#36)
  - Detect syntax errors in `flake8` syntax checker for Python (#42)
  - Fix various regressions after introducing unit tests
  - Work around broken package installation with syntax checking enabled (#45)
  - Work around issues with checking Tramp buffers (#54)
  - Preserve whitespace in error messages (#65)

- API changes:

  - Replace syntax checker variables with syntax checker declarations (#41, #52)
  - Add support for error parsing with functions instead of patterns (#38)
  - Add an error parser for Checkstyle-like XML output (#38)


0.5 (Dec 28, 2012)
==================

- New syntax checkers:

  - SASS (#15)
  - Perl (#21)
  - XML
  - Lua (#30)

- New features:

  - Support manual, buffer-local selection of syntax checker (#25)
  - Add customizable error indicators (#15, #28)

- Improvements:

  - Remember the last automatically selected syntax checker (#24)

- Bug fixes:

  - Fix syntax checking of buffers without file names (#19)

- API changes:

  - Replace Flymake API with a custom syntax checking implementation (#15, #27)


0.4 (Nov 21, 2012)
==================

- Rename the project to Flycheck (#5)
- New syntax checkers:

  - HAML (#9)
  - CSS (#9)
  - Javascript (#9, #16) with `jsl` and `jshint`
  - JSON (#12)
  - LaTeX with `lacheck`

- Bug fixes:

  - Fix type error when checking compressed Emacs Lisp (#10)


0.3 (Oct 20, 2012)
==================

- Add a distinct mode syntax checking (#4)


0.2 (Oct 15, 2012)
==================

- New syntax checkers:

  - PHP

- API changes:

  - Simplify syntax checker declarations (#2)


0.1 (Oct 11, 2012)
==================

Initial release as flymake-checkers

- New syntax checkers:

  - TeX/LaTeX
  - Shell scripts
  - Python
  - Ruby
  - CoffeeScript
  - Emacs Lisp
