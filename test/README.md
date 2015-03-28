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

Flycheck provides a virtual machine based on [VirtualBox][] and [Vagrant][]
which a complete environment to run the entire test suite.  If you need to run
the test suite frequently, it is recommended that you use this environment.

The virtual machine contains

- The latest stable Emacs release
- A nightly build of Emacs trunk
- `make` and [Cask][]
- All syntax checkers supported by Flycheck

To use this VM, first install the following tools on your system:

- [VirtualBox][]
- [Vagrant][]
- [Ansible][]

On Linux, these packages are typically available from the package manager of
your distribution.  On OS X, use the binaries provided for Vagrant and
VirtualBox, and install Ansible from [Homebrew][] with `brew install ansible`.

To start the VM run the following command in the root directory of Flycheck:

```console
$ vagrant up
```

If run the first time, this command will download, setup and provision the
virtual machine.  This includes downloading and compiling a lot of packages.
Depending on your network connection, disk speed and CPU power, this can take a
lot of time.

After the VM is started, you can connect to it:

```console
$ vagrant ssh
```

This will give you a bash prompt on the virtual machine, on which you can run
the tests as explained in the [previous section](#test-running).

To switch between Emacs versions, pass `EMACS` to `make test`:

```console
$ make EMACS=emacs24 test
$ make EMACS=emacs-snapshot test
```

The latter is the default.

When finished, you can shutdown the VM with:

```console
$ vagrant halt
```

Occasionally you should provision the test environment again, to update to the
latest Emacs nightly build, and to follow updates of syntax checker tools.  To
do so, use the `--provision` flag when starting the VM:

```console
$ vagrant up --provision
```

You can also provision a running machine with::

```console
$ vagrant provision
```

The VM is provisioned from Ansible playbooks in the `playbooks/` sub-directory
of the top-level source directory.

[VirtualBox]: https://www.virtualbox.org/
[Vagrant]: https://www.vagrantup.com/
[Ansible]: http://www.ansible.com/home
[Homebrew]: http://brew.sh/


Travis CI
=========

The entire test suite continuously runs on [Travis CI][] after every push, with
the latest Emacs release and a nightly Emacs snapshot.

Travis CI is configured from `.travis.yml` in the top-level source directory,
and uses mostly the same playbooks for provisioning.

Travis CI is the **reference environment** for Flycheck's test suite.  All tests
**must pass** on Travis CI.

[Travis CI]: https://travis-ci.org/flycheck/flycheck
