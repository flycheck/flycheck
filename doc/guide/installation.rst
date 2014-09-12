.. _installation:

==============
 Installation
==============

Prerequisites
=============

Emacs
-----

Flycheck supports Emacs 24 on Unix systems.  It is tested with Emacs 24.3, and
Emacs snapshot builds.  It should work with GNU Emacs 24.1 and 24.2 as well, but
it is not tested against these versions.  If it does not work with these
versions, please report an issue.

.. warning::

   Flycheck does not support Windows, but tries to be compatible with it.  You
   may try to use Flycheck on Windows.  It should mostly work, but expect
   problems and issues.  Pull requests which improve Windows compatibility are
   welcome.

   Flycheck also does not support GNU Emacs 23 and other flavors of Emacs
   (e.g. XEmacs, Aquamacs, etc.).  Don't try, it will *not* work.

Syntax checking tools
---------------------

For almost all supported languages, Flycheck also needs additional **external**
checker programs.  See :doc:`languages` for a list of supported languages and
the corresponding checkers programs, and use
:command:`flycheck-describe-checker` to get help about specific checkers inside
Emacs.

For instance, for Python you need either Flake8_ or Pylint_, which can be
installed with `pip install flake8` and `pip install pylint` respectively.

Generally you can install the required external checker programs with the
standard package manager of the corresponding programming languages
(e.g. Rubygems for Ruby, NPM for Javascript).  Many checker programs are also
available in the package repositories of popular Linux distributions, or as
Formula for the OS X package manager Homebrew_.

.. _Flake8: https://flake8.readthedocs.org/
.. _Pylint: http://www.pylint.org/
.. _homebrew: http://brew.sh/

Package installation
====================

Manual
------

Install the ELPA package from MELPA_ or `MELPA Stable`_ using :kbd:`M-x
package-install RET flycheck`.

.. warning::

   Flycheck is available from Marmalade_ as well, but this repository is **not
   recommended**.  Due to frequent outages the package is not guaranteed to be
   updated regularly.

Neither of these repositories is included in GNU Emacs by default.  You need to
enable these repositories explicitly.  For instance, to add the MELPA
repository, add the following code to :file:`init.el`:

.. code-block:: cl

   (require 'package)
   (add-to-list 'package-archives
                '("melpa" . "http://melpa.milkbox.net/packages/") t)
   (package-initialize)

Cask
----

If you use Cask_, just add the following to your :file:`Cask` file, and run
`cask install`:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

Setup
=====

Use :kbd:`M-x flycheck-mode` to enable Flycheck in the current buffer, and
:kbd:`M-x global-flycheck-mode` to enable Flycheck for the entire current Emacs
session.

To permanently enable Flycheck, add the following to your :file:`init.el`:

.. code-block:: cl

    (add-hook 'after-init-hook #'global-flycheck-mode)

.. _MELPA: http://melpa.milkbox.net
.. _MELPA Stable: http://melpa-stable.milkbox.net
.. _Marmalade: http://marmalade-repo.org/
