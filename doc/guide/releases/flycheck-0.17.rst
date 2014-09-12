===============
 Flycheck 0.17
===============

:date: Feb 1, 2014
:version: 0.17

.. only:: not format_texinfo

   .. figure:: /images/flycheck-0_17.png
      :align: center

      Flycheck 0.17 with `Solarized Light`_ and `Source Code Pro`_

Flycheck 0.17 was released today.  Though it's just about two weeks since the
last release, there are quite some changes.

.. _Source Code Pro: https://github.com/adobe/source-code-pro
.. _Solarized Light: https://github.com/bbatsov/solarized-emacs

.. contents:: Table of Contents
   :local:

Breaking changes
================

The default value of :option:`flycheck-completion-system` was changed to `nil`,
i.e. Flycheck now uses plain `completing-read` as default completion system.  To
restore the previous default (i.e. IDO), customize it with :kbd:`M-x
customize-variable RET flycheck-completion-system` or add the following to your
:file:`init.el`:

.. code-block:: cl

   (eval-after-load 'flycheck '(setq flycheck-completion-system 'ido))

Also, :function:`flycheck-count-errors` was changed to count errors of all
levels.  It returns an alist mapping error levels to the amount of errors of
that level.

New online manual
=================

The most notable change is the new online documentation at
http://flycheck.readthedocs.org, which replaces the old online manual at
http://flycheck.github.io.  The latter now redirects to the former.

The new manual is written with Sphinx_ instead of Texinfo_.  The underlying
markup is ReStructuredText_, which is simpler, less cluttered, and more powerful
than the TeX dialect of Texinfo_.  It is also more popular, as nowadays Sphinx
has become a standard tool among Python developers, and is also increasingly
used with other languages as well.  For instance, the `LLVM documentation`_ is
written in Sphinx.

Besides providing a better HTML documentation, we hope that the new manual
lowers the barrier to external contributors and increases the amount of
contributions to the documentation.

The documentation is now automatically re-build after each push to the Github
repository, so that http://flycheck.readthedocs.org/en/latest now always hosts
the up to date documentation.  Besides, there are separate sites for each
versioned release of Flycheck.  For instance,
http://flycheck.readthedocs.org/en/0.17 now hosts the documentation of Flycheck
0.17 now.

Beyond the manual itself, the new documentation now also includes the release
notes, which were previously hosted at http://www.lunaryorn.com, and the
:ref:`comparison with Flymake <flycheck-versus-flymake>`, which was previously
hosted at the Github Wiki of Flycheck.  The Github Wiki is disabled now, to make
all documentation available from a single place.

Unfortunately, the Info manual included in the Flycheck ELPA packages is
somewhat inferior now, because there is some idiomatic gap between Sphinx and
Texinfo.  It's still usable, though, and we hope to overcome the most striking
shortcomings over the time.

.. _Sphinx: http://sphinx-doc.org
.. _Texinfo: http://www.gnu.org/software/texinfo/
.. _ReStructuredText: http://docutils.sourceforge.net/rst.html
.. _LLVM documentation: http://llvm.org/docs/

Syntax checkers
===============

Flycheck can now check GNU Makefiles (see [GH-321]) with the new `make-gmake`
syntax checker.

The :flyc-checker:`rust` syntax checker now uses `--no-trans` to check for more
than just syntax errors.  For instance, it will report unused variables now.

As part of the transition to the new manual, Flycheck now provides a syntax
checker for Sphinx documents (:flyc-checker:`rst-sphinx`).  The
:flyc-checker:`rst` syntax checker does not check documents within a Sphinx
project anymore, do avoid false positives from Sphinx-specific markup.

Extending syntax checkers
=========================

Other bug fixes and improvements
================================

- Flycheck does not longer attach syntax checker processes to the buffer
  anymore, improving compatibility with Tabbar Mode ([GH-298]).
- The :flyc-checker:`emacs-lisp` and :flyc-checker:`emacs-lisp-checkdoc` syntax
  checkers do not visit the file being checked anymore, to avoid unintended side
  effects from unsafe local variables, etc. ([GH-319])
- When a buffer changes while being checked, Flycheck immediately re-checks the
  buffer after the check finished, to avoid outdated errors ([GH-301]).
