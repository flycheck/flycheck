.. _flycheck-extensions:

========================
 Recommended extensions
========================

The Emacs community has produced a number of extensions to Flycheck.  This page
lists all that we know of and can safely recommend to our users.  *Official*
extensions are maintained by the :ref:`Flycheck maintainers
<flycheck-maintainers>`.

If you do know extensions not in this list, or would like to see your own
extension here, please feel free to `add it`_.

.. _add it: https://github.com/flycheck/flycheck/edit/master/doc/community/extensions.rst

User interface
==============

* :flyc:`flycheck-color-mode-line` (*official*) colors the mode line according
  to the Flycheck status.
* :flyc:`flycheck-pos-tip` (*official*) shows Flycheck error messages in a
  graphical popup.
* :gh:`liblit/flycheck-status-emoji` adds cute emoji (e.g. 😱 for errors) to
  Flycheck’s mode line status.

Language integration
====================

* :flyc:`flycheck-cask` (*official*) makes Flycheck use Cask packages for Emacs
  Lisp syntax checking in Cask_ projects.
* :flyc:`flycheck-rust` (*official*) configures Flycheck according to the Cargo
  settings and layouts of the current Rust project.
* :flyc:`flycheck-haskell` (*official*) configures Flycheck from the Cabal
  settings and sandbox in Haskell projects.
* :flyc:`Wilfred/flycheck-pkg-config` configures Flycheck to use settings from
  `pkg-config`_ when checking C/C++.

.. _Cask: https://github.com/cask/cask
.. _pkg-config: https://www.freedesktop.org/wiki/Software/pkg-config/

Additional languages and syntax checkers
========================================

* :gh:`Gnouc/flycheck-checkbashisms` adds a shell script syntax checker using
  :program:`checkbashisms` which is part of `Debian devscripts`_ and checks for
  common Bash constructs in POSIX shell scripts.
* :gh:`clojure-emacs/squiggly-clojure` provides syntax checking for Clojure.
* :flyc:`flycheck-d-unittest` (*official*) adds a Flycheck checker to run unit
  tests for D programs on the fly.
* :flyc:`flycheck-google-cpplint` (*official*) adds a syntax checker for
  Google's C++ style checker.
* :gh:`cmarqu/flycheck-hdl-irun` adds a syntax checker for hardware description
  languages (HDLs) supported by `Cadence IES/irun`_.
* :gh:`Sarcasm/flycheck-irony` adds a Flycheck syntax checker for C, C++ and
  Objective C using :gh:`Irony Mode <Sarcasm/irony-mode>`.
* :gh:`purcell/flycheck-ledger` adds a syntax checker for the Ledger_ accounting
  tool.
* :flyc:`flycheck-mercury` (*official*) adds a Flycheck syntax checker for the
  `Mercury language`_.
* :flyc:`flycheck-ocaml` (*official*) adds a syntax checker for OCaml.
* :gh:`purcell/flycheck-package` checks emacs lisp package source code for
  common problems.
* :gh:`Wilfred/flycheck-pyflakes` adds a Python syntax checker using Pyflakes.

.. _Debian devscripts: https://anonscm.debian.org/cgit/collab-maint/devscripts.git
.. _Ledger: http://ledger-cli.org/
.. _Mercury language: http://mercurylang.org/
.. _Cadence IES/irun: http://www.cadence.com/products/fv/enterprise_simulator/pages/default.aspx
.. _Pyflakes: https://github.com/pyflakes/pyflakes
