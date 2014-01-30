==============
 Introduction
==============

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24.

.. _features:

Features
========

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24.

- Support for |#flycheck-languages| languages with |#flycheck-checkers| syntax
  checkers, see :doc:`languages`
- :ref:`Fully automatic syntax checking in the background <syntax-checking>`
- :ref:`Nice error indication and highlighting <error-reporting>`
- Optional error list popup
- :ref:`Many customization options <syntax-checker-configuration>`
- :doc:`A comprehensive manual <index>`
- :ref:`A dead simple API to create new syntax checkers
  <defining-new-syntax-checkers>`
- A “doesn't get in your way” guarantee

.. _3rd-party-extensions:

3rd party extensions
====================

The following extensions provide additional cool features for Flycheck:

- flycheck-cask_ makes Flycheck use Cask packages in Cask_ projects.
- flycheck-color-mode-line_ colors the mode line according to the Flycheck
  status.
- flycheck-d-unittest_ adds a Flycheck checker to run unit tests for D programs
  on the fly.
- flycheck-hdevtools_ adds a Flycheck syntax checker for Haskell based on
  hdevtools_.
- flycheck-haskell_ improves Haskell support in Flycheck, by configuring
  Flycheck according to the current Cabal project, and using Cabal sandbox
  packages.
- flycheck-mercury_ adds a Flycheck syntax checker for the `Mercury Language`_.

.. _flycheck-cask: https://github.com/flycheck/flycheck-cask
.. _Cask: https://github.com/cask/cask
.. _flycheck-color-mode-line: https://github.com/flycheck/flycheck-color-mode-line
.. _flycheck-d-unittest: https://github.com/flycheck/flycheck-d-unittest
.. _flycheck-hdevtools: https://github.com/flycheck/flycheck-hdevtools
.. _hdevtools: https://github.com/bitc/hdevtools/
.. _flycheck-haskell: https://github.com/flycheck/flycheck-haskell
.. _flycheck-mercury: https://github.com/flycheck/flycheck-mercury
.. _Mercury language: http://mercurylang.org/

.. _installation:

Installation
============

Install the ELPA package from MELPA_ or Marmalade_ using :kbd:`M-x
package-install RET flycheck`.  The former is the *recommended* repository.

Neither of these repositories is included in GNU Emacs by default.  You need to
enable these repositories explicitly.  For instance, to add the MELPA
repository, add the following code to :file:`init.el`:

.. code-block:: cl

   (require 'package)
   (add-to-list 'package-archives
                '("melpa" . "http://melpa.milkbox.net/packages/") t)
   (package-initialize)

If you use Cask_, add the following to your :file:`Cask` file:

.. code-block:: cl

   (source gnu)
   (source melpa)

   (depends-on "flycheck")

Flycheck supports Emacs 24.  It is tested with Emacs 24.3, and Emacs snapshot
builds.  It should work with GNU Emacs 24.1 and 24.2 as well, but it is not
tested against these versions.  If it does not work with these versions, please
report an issue.

Flycheck does not support Windows, but tries to be compatible with it.  You may
try to use Flycheck on Windows.  It should mostly work, but expect problems and
issues.  Pull requests which improve Windows compatibility are welcome.

Flycheck also does not support GNU Emacs 23 and other flavors of Emacs
(e.g. XEmacs, Aquamacs, etc.).  Don't try, it will *not* work.

Most checkers depend on external tools to perform the actual syntax checking.
Use :command:`flycheck-describe-checker` to get help about a syntax checker and
its dependencies.

.. _MELPA: http://melpa.milkbox.net
.. _Marmalade: http://marmalade-repo.org/
