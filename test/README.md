Testing Flycheck
----------------

This directory contains the large unit test suite of Flycheck.  The test suite
tests all syntax checkers with (almost) all options and large parts of
Flycheck’s internal API and interactive commands.

Test suite layout
-----------------

The library `flycheck-ert.el` extends ERT with utilities, test case definitions
and assertion predicates for testing Flycheck code and syntax checkers.  This
library is part of all Flycheck packages, so you may use it in your own Flycheck
extensions as well.

This directory contains the test suite itself:

- `flycheck-test.el`: The actual unit test suite, that is, the test cases plus
  some local utility functions and macros.
- `resources/`: Resource files used by the test suite, including example code in
  various programming languages to test syntax checkers.
- `run.el`: A simple test runner for non-interactive use, which reads ERT
  selectors from the command line and runs all matching tests.

Test running
------------

Before running tests, you need to install the Emacs Lisp dependencies of the
test suite (you need [Cask][] for this):

```console
$ make deps
```

Then install the syntax checkers that you'd like to test, for example:

```console
$ brew install go
$ pip install --user pylint
$ npm install --global coffee coffee-lint jshint
$ cabal install hlint shellcheck
```

If you’d like to keep your machine clean, you may use the
[virtual test environment](#virtual-test-environment) instead.

Now you can run the test suite.  Test cases are automatically skipped if the
corresponding tools are not available:

```console
$ make test
```

You may also run a specific subset of test cases by passing an Emacs Lisp
expression with ERT test selectors.  The following example runs all tests for C
and C++, except those whose name matches `gcc`:

```console
$ make ERTSELECTOR='(and (or (language c) (language c++)) (not "gcc"))' test
```

Keep in mind that you need to quote this expression for Emacs Lisp *and* for
your shell.

In addition to the [standard ERT selectors][ertselector], the Flycheck test
suite defines a couple of additional selectors:

- `(language LANGUAGE)` matches all tests for the given programming `LANGUAGE`.
- `(checker CHECKER)` matches all tests for the given syntax `CHECKER`.
- `(new-checker-for LANGUAGE)` matches all tests that need to be run if a new
  checker for `LANGUAGE` was added.

You can also use a different Emacs to run the tests with:

```console
$ make EMACS=emacs-snapshot test
```

[Cask]: http://cask.github.io
[ertselector]: http://www.gnu.org/software/emacs/manual/html_node/ert/Test-Selectors.html#Test-Selectors

Virtual test environment
------------------------

Flycheck provides a [virtual machine][flycheck-vm] which contains all Emacs
versions and all syntax checkers supported by Flycheck.

[flycheck-vm]: https://github.com/flycheck/flycheck-vm

Travis CI
=========

The basic test suite—without all syntax checker tests—continuously runs on
[Travis CI][] after every push, with the latest Emacs release and a nightly
Emacs snapshot.

Travis CI is configured from `.travis.yml` in the top-level source directory,
and uses mostly the same playbooks for provisioning.

Travis CI is the **reference environment** for Flycheck's test suite.  All tests
**must pass** on Travis CI.

[Travis CI]: https://travis-ci.org/flycheck/flycheck
