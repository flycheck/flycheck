==================
 Testing Flycheck
==================

.. highlight:: console

Flycheck comes with a large unit test suite, which tests all syntax checkers and
large parts of Flychecks internal API and interactive commands.

This document explains how to use this test suite.

Test suite layout
=================

All test code is contained in the ``test/`` sub-directory, which has the following
contents:

- ``flycheck-test.el``: The actual unit test suite, that is, the test cases and
  the utility functions.
- ``resources/``: Resource files used by the test suite, including example code
  in various programming languages to test syntax checkers.
- ``run-tests.el``: A simple test runner for non-interactive use, which reads
  ERT selectors from the command line and runs all matching tests.

.. _test-running:

Test running
============

First install Emacs Lisp dependencies using Cask_::

   $ make deps

Then install the syntax checkers that you'd like to test::

   $ brew install go
   $ pip install --user pylint
   $ npm install --global coffee coffee-lint jshint
   $ cabal install hlint shellcheck

.. note::

   If you'd like to keep your environment clean, use the
   :ref:`virtual-test-environment` instead.

Run the entire test suite::

   $ make test

.. note::

   Tests which can't be run because of missing tools are gracefully skipped.

You may also run a specific subset of test cases by passing an ERT selector
expression.  The following example runs all tests for C and C++, except those
whose name matches ``gcc``::

   $ make ERTSELECTOR='(and (or (tag language-c) (tag language-c++)) (not "gcc"))' test

See :infonode:`(ert)Test Selectors` for the syntax of test selectors.

You can also use a different Emacs to run the tests with::

   $ make EMACS=emacs-snapshot test

.. note::

   Keep in mind that you must quote the expression for your shell *and* for
   Emacs Lisp.

.. _cask: http://cask.readthedocs.org

.. _virtual-test-environment:

Virtual test environment
========================

To run all tests successfully, you need a large amount of external tools.  To
keep your environment clean, and test against a reference environment, Flycheck
provides a virtual machine with a complete test environment, based on VirtualBox
and Vagrant.

The virtual machine contains

- The latest stable Emacs release
- A nightly build of Emacs trunk
- ``make`` and Cask_
- All syntax checkers supported by Flycheck

To use this VM, first install the following tools on your system:

- VirtualBox_
- Vagrant_
- Ansible_

On Linux, these packages are typically available from the package manager of
your distribution.  On OS X, use the binaries provided for Vagrant and
VirtualBox, and install Ansible from `Homebrew`_ with ``brew install ansible``.

To start the VM run the following command in the root directory of Flycheck::

   $ vagrant up

.. warning::

   On the first time, this command will setup and provision the VM, and
   **download and install over 1GB** of images and packages along the way.
   Depending on your network connection and disk speed, this can take an hour or
   more.

After the VM is started, you can connect to it::

   $ vagrant ssh

This will give you a bash prompt within the VM, on which you can run the tests
as explained in :ref:`test-running`.

To switch between Emacs versions, pass ``EMACS`` to ``make test``::

   $ make EMACS=emacs24 test
   $ make EMACS=emacs-snapshot test

The latter is the default.

When finished, you can shutdown the VM with::

   $ vagrant halt

Occasionally you should provision the test environment again, to update to the
latest Emacs nightly build, and to follow updates of syntax checker tools.  To
do so, use the `--provision` flag when starting the VM::

   $ vagrant up --provision

You can also provision a running machine with::

   $ vagrant provision

The VM is provisioned from Ansible playbooks in the ``playbooks/`` sub-directory
of the top-level source directory.

.. _VirtualBox: https://www.virtualbox.org/
.. _Vagrant: http://www.vagrantup.com/
.. _Ansible: http://www.ansible.com/home
.. _Homebrew: http://brew.sh/

Travis CI
=========

The entire test suite continuously runs on `Travis CI`_ after every push, with
the latest Emacs release and a nightly Emacs snapshot.

Travis CI is configured from ``.travis.yml`` in the top-level source directory,
and uses mostly the same playbooks for provisioning.

.. note::

   Travis CI is the **reference environment** for Flycheck's test suite.  All
   tests **must pass** on Travis CI.

   In case Travis CI diverges from the :ref:`VM <virtual-test-environment>`,
   Travis CI is authoritative.
