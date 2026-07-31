.. _flycheck-faq:

============================
 Frequently asked questions
============================

Short answers to the things people ask most, with pointers to the chapter that
covers each properly.  If something is going wrong rather than merely puzzling,
start with :ref:`flycheck-troubleshooting`.

Getting started
===============

Why does nothing happen when I open a file?
-------------------------------------------

Look at the mode line.  ``FlyC-`` means no syntax checker applies to the
buffer, usually because the tool is not installed.  Flycheck also says so once
per major mode.  Press :kbd:`C-c ! v` for the full picture: every checker that
could apply, and why each one is or is not usable.  See
:ref:`flycheck-mode-line-troubleshooting`.

Do I have to install the syntax checkers myself?
------------------------------------------------

Yes.  Flycheck runs the linters and compilers you already have; it does not
bundle or install any of them.  :ref:`flycheck-languages` lists what each
language's checkers need.

Which checker runs when several support my language?
----------------------------------------------------

The first installed one in `flycheck-checkers`.  Checkers that are not chained
to each other do not both run, so the order decides.  :kbd:`C-c ! v` names the
one that will run and lists the ones that could.  To choose a different one for
a buffer, use :kbd:`C-c ! s` (`flycheck-select-checker`); to change it
everywhere, reorder `flycheck-checkers`.

Can I run two linters over the same buffer?
--------------------------------------------

Yes, by chaining them:

.. code-block:: elisp

   (flycheck-add-next-checker 'python-ruff 'python-mypy)

Both sets of errors land in the same list.  See
:ref:`flycheck-checker-chains`, which also covers running one only when the
other found nothing serious.

Configuration
=============

How do I stop a checker from running?
--------------------------------------

Add it to `flycheck-disabled-checkers`, which is buffer-local and works as a
file- or directory-local variable.  For one buffer, :kbd:`C-c ! x` disables the
checker at point; :kbd:`C-u C-c ! x` re-enables one.

Why did a checker disable itself?
----------------------------------

Because its tool said it could not run at all, rather than reporting findings:
a missing dependency, an unreadable configuration, a linter with no config
file.  Flycheck names the reason and :kbd:`C-u C-c ! x` brings the checker
back.  See :ref:`flycheck-common-issues`.

How do I pass extra arguments to a checker?
--------------------------------------------

Most checkers have a ``…-args`` option taking a list of strings, plus options
for the settings people reach for most.  :kbd:`C-c ! ?`
(`flycheck-describe-checker`) lists what a given checker accepts, and
:ref:`flycheck-languages` documents them all.

How do I point a checker at a specific executable?
---------------------------------------------------

Set its ``flycheck-…-executable`` option, which every checker has.  See
:ref:`flycheck-checker-executables`.

LSP
===

I use Eglot.  Why does Flycheck show nothing?
----------------------------------------------

Eglot reports through Flymake and provides no Flycheck backend of its own.
Bridge them with ``global-flycheck-eglot-mode``.  See
:ref:`flycheck-syntax-checks`.

Can I use Eglot for my language server and Flycheck for a linter?
------------------------------------------------------------------

Yes, and both can report at once.  That is the usual arrangement for something
like Tailwind's or ESLint's server alongside the one you edit with; see
:ref:`flycheck-lsp-alongside-eglot`.

Why is Emacs slow in a buffer backed by a language server?
------------------------------------------------------------

Some servers publish diagnostics continuously while they index or build, and
each push that changes anything re-runs the check that publishes it.  :kbd:`C-c
! v` reports how many pushes arrived and how many changed something.  See
:ref:`flycheck-troubleshooting`.

Display
=======

Can I see errors inline, next to the code?
-------------------------------------------

Yes: `flycheck-annotate-mode`, or ``global-flycheck-annotate-mode``
everywhere.  Several styles ship, from a compact message after the line to the
full text underneath.  See :doc:`error-reports`.

Why do some errors have a ``[fix]`` marker?
--------------------------------------------

Their checker offered a machine-applicable fix, which :kbd:`C-c ! f` applies.
A ``[fix?]`` marker means the fix has to be fetched before Flycheck knows
whether there is one, as with a language server's code actions; pressing
:kbd:`C-c ! f` asks.  See :doc:`error-interaction`.

Flycheck and Flymake
====================

Should I use Flycheck or Flymake?
----------------------------------

Both are good.  :doc:`flycheck-versus-flymake` compares them feature by
feature, without pretending to be neutral about the conclusion.

Can I run both at once?
------------------------

You can, but you will see every diagnostic twice.  ``flycheck-eglot-mode``
turns Flymake off in the buffers it manages for that reason.
