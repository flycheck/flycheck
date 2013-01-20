Flycheck testsuite
==================

This directory contains the test suite of Flycheck, using the [ERT][] test
library.

Running tests
-------------

Run the commands in this section from the **top-level source directory**!

### Local test execution

```
$ make test
```

### Virtual test environment

Install [VirtualBox][] and [Vagrant][] and run:

```
# Test against stable Emacs 24.2
$ make virtual-test
# Test against Emacs 24.3 snapshot
$ make EMACS=emacs-snapshot virtual-test
```

The virtual machine behind these tests is configured by `Vagrantfile` and
provisioned by the shell script `vagrant/provision.sh`.


Writing tests
-------------

Add your tests to the appropriate file, or create a new one.  Follow these
guidelines:

- Read the documentation of [ERT][].
- Use test helpers from `testhelpers.el` as much as possible.
- Use `flycheck-with-resource-buffer` to load files from a test file's
  directory.
- If you add tests for a new checker, do not forget to extend
  `/vagrant/provision.sh` to install the necessary tools to test your checker.


[ert]: http://www.gnu.org/software/emacs/manual/html_node/ert/index.html
[virtualbox]: https://www.virtualbox.org/
[vagrant]: http://www.vagrantup.com/
