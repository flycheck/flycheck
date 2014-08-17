==============
 Introduction
==============

Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs 24,
intended as replacement for the older Flymake extension which is part of GNU
Emacs.

It uses various syntax checking and linting tools to automatically check the
contents of buffers while you type, and reports warnings and errors directly in
the buffer, or in an optional error list.

.. _features:

Features
========

- |#flycheck-languages| languages with |#flycheck-checkers| syntax checkers, see
  :ref:`supported-languages`
- :ref:`Fully automatic, fail-safe, on-the-fly syntax checking in background
  <syntax-checking>`
- :ref:`Nice error indication and highlighting <error-reporting>`
- Optional error list popup
- :ref:`Many customization options <syntax-checker-configuration>`
- :ref:`A comprehensive manual <usage>`
- :ref:`A simple API to define new syntax checkers
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
- flycheck-google-cpplint_ adds a syntax checker for Google's C++ style checker.
- flycheck-haskell_ improves Haskell support in Flycheck, by configuring
  Flycheck according to the current Cabal project, and using Cabal sandbox
  packages.
- flycheck-hdevtools_ adds a Flycheck syntax checker for Haskell based on
  hdevtools_.
- flycheck-ledger_ adds a syntax checker for the Ledger_ accounting tool.
- flycheck-mercury_ adds a Flycheck syntax checker for the `Mercury Language`_.
- flycheck-pos-tip_ shows Flycheck error messages in a popup.
- flycheck-pyflakes_ adds a Python syntax checker using Pyflakes.
- flycheck-rust_ improves Rust support in Flycheck, by configuring Flycheck
  according to Cargo settings and layouts.

.. _flycheck-cask: https://github.com/flycheck/flycheck-cask
.. _Cask: https://github.com/cask/cask
.. _flycheck-color-mode-line: https://github.com/flycheck/flycheck-color-mode-line
.. _flycheck-d-unittest: https://github.com/flycheck/flycheck-d-unittest
.. _flycheck-google-cpplint: https://github.com/flycheck/flycheck-google-cpplint
.. _flycheck-haskell: https://github.com/flycheck/flycheck-haskell
.. _flycheck-hdevtools: https://github.com/flycheck/flycheck-hdevtools
.. _hdevtools: https://github.com/bitc/hdevtools/
.. _flycheck-ledger: https://github.com/purcell/flycheck-ledger
.. _Ledger: http://ledger-cli.org/
.. _flycheck-mercury: https://github.com/flycheck/flycheck-mercury
.. _Mercury language: http://mercurylang.org/
.. _flycheck-pos-tip: https://github.com/flycheck/flycheck-pos-tip
.. _flycheck-pyflakes: https://github.com/Wilfred/flycheck-pyflakes
.. _flycheck-rust: https://github.com/flycheck/flycheck-rust
