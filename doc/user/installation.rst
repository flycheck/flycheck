.. _flycheck-installation:

==============
 Installation
==============

Prerequisites
=============

Flycheck needs GNU Emacs 24.3 and works best on Unix-like systems like Linux or
OS X.  It does not support older releases of GNU Emacs or other flavours of
Emacs (e.g. XEmacs, Aquamacs, etc.).

.. _flycheck-windows-support:

Windows support
---------------

**Flycheck does not explicitly support Windows**, but tries to maintain Windows
compatibility and should generally work fine on Windows, too.  However, we can
neither answer questions about Windows nor fix bugs that only occur on Windows
without the help of active Windows users.  Please watch out for `known Windows
issues`_.

.. _known Windows issues: https://github.com/flycheck/flycheck/labels/B-Windows%20only

Syntax checking tools
---------------------

Flycheck does not check buffers itself but relies on *external* programs to
check buffers.  These programs must be installed separately.  Please take a look
at the :ref:`list of supported languages <flycheck-languages>` to find out what
tools are required for a particular language.

Many of these programs are available in the package repositories of Linux
distributions or in Homebrew_ for OS X.  Others can be installed with standard
package managers such as Rubygems, NPM, Cabal, etc.

.. _Homebrew: http://brew.sh

Package installation
====================

We recommend to install Flycheck with Emacs' built-in package manager.  Flycheck
is available in the popular MELPA_ archive which provides up to date snapshots
of Flycheck's development state.  The sibling repository `MELPA Stable`_ serves
tagged releases of Flycheck instead.  We advise to use MELPA if you are fine
with weekly or even daily updates.  If you would prefer longer time between
releases use MELPA Stable instead.

Unfortunately neither of these repositories are available in Emacs by default.
You must explicitly add them to `package-archives`, by adding the following to
your :term:`init file`:

.. code-block:: elisp

   (require 'package)
   (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/") t)
   (package-initialize)

This adds MELPA; for MELPA Stable replace ``https://melpa.org`` with
``https://stable.melpa.org``.  If you do not know where your init file is
inspect the value of `user-init-file` with :kbd:`C-h v user-init-file`.

Once the repository is set up you can install Flycheck from Emacs' package menu
at :kbd:`M-x list-packages`, or directly with :kbd:`M-x package-install RET
flycheck`.

use-package
-----------

You may want to take a look at `use-package`_ which provides simple syntax to
declare and configure packages in your init file.  In addition to the Github
README the article `My Emacs configuration with use-package`_ has more
information about use-package.  Specifically it allows to automatically install
missing packages from package archive when Emacs starts.

Add the following form to your init file to setup Flycheck with `use-package`:

.. code-block:: elisp

   (use-package flycheck
     :ensure t
     :init (global-flycheck-mode))

Then press :kbd:`C-M-x` with point somewhere in this form to install and enable
Flycheck for the current Emacs session.

.. _MELPA: https://melpa.org
.. _MELPA Stable: https://stable.melpa.org
.. _Getting Started: https://melpa.org/#/getting-started
.. _use-package: https://github.com/jwiegley/use-package
.. _My Emacs configuration with use-package: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
