===============
 Flycheck 0.18
===============

:date: Mar 24, 2014
:version: 0.18

Today, we release Flycheck 0.18.

It's quite some time since the last release, but there are not many big changes,
as Flycheck becomes more and more mature, and slowly approaches a 1.0 release.

.. contents:: Table of Contents
   :local:

Breaking changes
================

There are some small breaking changes.  Most notably, the naming conventions of
various shell script syntax checkers were changed.  All of them now use the
prefix `sh-`, and the former `sh-` checkers now have the prefix `sh-posix-`.
This affects their executable variables as well.

Furthermore, the version requirements for some syntax checker tools changed:

- :flyc-checker:`rust`  needs upcoming Rust 0.10 now, because Flycheck followed
  backwards-incompatible upstream changes in the `rustc` options.
- :flyc-checker:`rst-sphinx` needs Sphinx 1.2 now, because Flycheck uses a
  special writer now, which is not present in previous releases.

Syntax checkers
===============

Flycheck can now check BSD Makefile with BSD Make as well.  The
:flyc-checker:`make` syntax checker does not use GNU Make specific options
anymore, and will work with any POSIX compatible variant of Make.

Go syntax checking got a major update with new syntax checkers based on Golint_
(:flyc-checker:`go-golint`) and `go tool vet` (:flyc-checker:`go-vet`).  Go now
has one of the most complete and powerful syntax checker chains in Flycheck.

Support for Perl and for shell scripts was greatly improved by introducing two
new syntax checkers:

- :flyc-checker:`perl-perlcritic` checks Perl with the powerful `Perl-Critic`_
  tool.
- :flyc-checker:`sh-shellcheck` checks Shell scripts with the awesome
  `Shellcheck`_ tool, which finds all sorts of idiomatic and semantics mistakes
  in shell scripts of various shell languages, and will greatly improve your
  shell scripting.

D users can now tell :flyc-checker:`d-dmd` about include directories with the
new option :option:`flycheck-dmd-include-path`.  Likewise, Rust users can use
:option:`flycheck-rust-library-path` now.

.. _golint: https://github.com/golang/lint
.. _Perl-Critic: https://metacpan.org/pod/Perl::Critic
.. _shellcheck: https://github.com/koalaman/shellcheck

Bug fixes
=========

As always, a good deal of bugs and issues was fixed:

- :flyc-checker:`puppet-lint` includes the name of the corresponding check in
  the error message now.
- :flyc-checker:`rst` handles ``.. include::`` directives with relative file
  names now.
- :flyc-checker:`rst-sphinx` will no longer choke if the document being checked
  contains custom nodes without ``text``  writers.
- :flyc-checker:`rust` will not longer emit pointless warnings about missing
  ``main`` functions in library creates.
- Various error parsing problems were corrected in :flyc-checker:`c/c++-clang`,
  :flyc-checker:`go-build` and :flyc-checker:`go-test`.

Get it
======

See :ref:`installation`.
