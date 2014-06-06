=======================
 Writing documentation
=======================

This document explains how to edit and building the documentation of Flycheck.

.. _doc-build-env:

Build environment setup
=======================

Flycheck's documentation is written using Sphinx_ and sphinxcontrib-emacs_.  To
build the documentation locally, you need to install these tools first.  This
section guides you through the process of installing them.

First make sure, that you have Python 2.7 and virtualenv_ available.  To install
virtualenv, use:

.. code-block:: console

   $ pip install --user virtualenv

.. note::

   You probably need to install pip_ first. It is available in the package
   repositories of most Linux distributions, as ``python-pip`` or similar. If
   ``pip`` is not available for your Linux distribution, or if you are using OS
   X, please follow the instructions to `install pip`_.

Then add ``~/Library/Python/2.7/bin`` (on OS X) or ``~/.local/bin`` (on other
Unix variants) to ``$PATH``.

Now create a virtual environment and install the requirements of the
documentation:

.. code-block:: console

   $ mkdir -p ~/.virtualenvs
   $ virtualenv -p python2.7 ~/.virtualenvs/cask
   $ pip install -r doc/requirements.txt

Now you are set up to build the documentation.

.. _Sphinx: http://sphinx-doc.org
.. _sphinxcontrib-emacs: http://sphinxcontrib-emacs.readthedocs.org/en/latest/
.. _virtualenv: http://virtualenv.readthedocs.org/en/latest/
.. _pip: https://pip.pypa.io/
.. _install pip: https://pip.pypa.io/en/latest/installing.html

.. _building-docs:

Building HTML and Texinfo
=========================

First, switch to the virtual environment you created in the
:ref:`doc-build-env`, and make sure that the requirements are up to date:

.. code-block:: console

   $ source ~/.virtualenvs/cask/bin/activate
   $ pip install -r doc/requirements.txt

Now build the documentation:

.. code-block:: console

   $ make html
   $ make texinfo

The HTML documentation is build to ``build/doc/html/``, where you can preview
it.  The Texinfo build goes to ``doc/flycheck.texi``.

.. warning::

   Unlike the HTML output, the Texinfo output is *committed* to the repository,
   since it is needed by MELPA_ to build an Info manual for inclusion in the
   Flycheck packages.


   .. _MELPA: http://melpa.milkbox.net/

Verifying references
====================

Sphinx emits warnings for internal references and cross-references to Emacs Lisp
symbols that could not be resolved.  Your documentation should build without any
such warnings.

You can additionally verify all external references with:

.. code-block:: console

   $ source ~/.virtualenvs/cask/bin/activate
   $ make linkcheck
