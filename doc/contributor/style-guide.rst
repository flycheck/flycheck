.. _flycheck-style-guide:

=============
 Style Guide
=============

This document describes our code style.  It tells you what to look for when
making changes to Flycheck, or when reviewing pull requests.

Features
========

Flycheck’s scope and focus is providing the infrastructure and foundations for
on-the-fly syntax checking.  Flycheck provides the basics but deep integration
with particular programming languages is best left to :ref:`separate packages
<flycheck-extensions>`.

Whether a feature is within the scope of Flycheck is the :ref:`maintainer’s
<flycheck-maintainers>` judgement call.  Generally we reserve the right to
reject any pull request for being out of scope.

* Avoid a *disproportionate amount of code* for a single syntax checker or
  language.  Look at the built-in checkers for judgement.  A syntax checker that
  requires a lot more code than any built-in checker is likely to be rejected.

* Avoid *deep integration* with a particular UI or completion framework.  Emacs’
  standard is our standard: We will reject code that is tied to Helm or Counsel.

* Likewise do not deviate from Emacs’ default behaviour too much.  Stick to
  Emacs’ standard for key bindings, interactive functions, etc.

Backward compatibility
======================

Checkers and languages evolve over time, and their error format often change as
a consequence.  It is not a goal of Flycheck to work with every version of every
checker ever supported.  However, the latest Flycheck version *should always
work* with the contemporary version of a checker.

As a rule of thumb, if maintaining backward compatibility is trivial (i.e., does
not incur code maintenance costs), then we should do it.  For example, a
slightly more complex parsing regexp is OK, but doing version detection to add a
flag would most likely be too much.

Keep in mind that users may not have the choice of updating to the latest
version of a checker (e.g., ``gcc`` on Debian-based distributions).  On the
other hand, npm or Python packages are usually trivial to update.  Making an
extra effort to maintain backward compatibility for these hard-to-update
checkers is reasonable.

The integration tests that are run on our CI should always reflect the latest
supported version.

Style
=====

.. important::

   ``make check compile`` must pass on Emacs 25 or newer.  This command checks
   for some formatting issues and compilation errors.

   Run ``make format`` with Emacs 25 to automatically reformat the Emacs Lisp
   source files.

* Generally try to fit into the style of the code you see.

* Indent with the default indentation rules.

* Follow the :infonode:`(elisp)Programming Tips` for Emacs Lisp.

* Whitespace:

  * 80 characters per line.
  * Avoid tabs and trailing spaces.

* Naming:

  * Prefix all variables and functions with the name of the containing library,
    i.e. ``flycheck-`` for everything that is in :file:`flycheck.el`.

  * End boolean predicates with ``-p``, i.e. ``flycheck-valid-checker-p``.

* Avoid macros, and use them for syntax only.

* Adhere to the :infonode:`(elisp)Key Binding Conventions`.  Particularly do not
  define keys in Emacs’ reserved keymaps or in the :samp:`C-c {LETTER}` space
  for user bindings.

Libraries
=========

* Do **not** advise built-in or 3rd party functions and commands.

* Do **not** redefine built-in or 3rd party functions, unless for compatibility,
  but then copy the newer definition verbatim.

* Do **not** use ``with-eval-after-load`` and similar functions.

* Dependencies:

  * Use built-in Emacs libraries freely.
  * Introduce external dependencies with care.  Prefer built-in
    libraries. ``dash.el`` is fine, though.
  * Avoid dependencies on language-specific libraries.

* Avoid ``cl-lib``:

  * Prefer ``seq`` over ``dash`` over ``cl-lib``.  Use list functions from
    ``cl-lib`` only as the very last resort.
  * Prefer ``let-alist`` and ``pcase`` over ``cl-destructuring-bind``.

Tests
=====

* Add comprehensive buttercup specs for new functions and commands to
  :file:`test/specs/`.  Check whether the specs fit into an existing spec file,
  or add a new file instead.  In doubt, use a new file.

* For new syntax checkers add at least one syntax checker integration test to
  :file:`test/flycheck-test.el`.  Make sure that the test passes with
  :samp:`make LANGUAGE={language} integ`.

Documentation
=============

* Add docstrings to all functions and variables.

* Follow the :infonode:`(elisp)Documentation Tips`.

* Take care to update our manual:

  * Document new interactive commands and user options in the :ref:`user guide
    <flycheck-user-guide>`.
  * Document new syntax checkers and new options for existing syntax checkers in
    the :ref:`list of languages <flycheck-languages>`.
  * Document new or changed version requirements for syntax checkers in the
    :ref:`list of languages <flycheck-languages>`.
  * Document changes to our build system and tooling in the :ref:`contributor’s
    guide <flycheck-contributors-guide>` or the :ref:`maintainer’s guide
    <flycheck-maintainers-guide>`.

Commits
=======

* Make each commit self-contained.

* Squash trivial fixes into previous commits so that no commit in and by itself
  violates this style guide.

* Write commit messages that adhere to the style illustrated below.

* In doubt prefer long messages over short messages.  Take the time to write a
  good message that explains the intention of the change and illustrates
  noteworthy aspects of the implementation.

* If the commit fixes a bug try to reproduce a brief description of the bug in
  the message and make sure to mention the corresponding GitHub issue
  (e.g. ``Fixes GH-42``).

Commit message style
--------------------

This model commit message illustrates our style::

   Fix a foo bug

   The first line is the summary, 50 characters or less.  Write in the
   imperative and in present tense: “Fix bug”, not “fixed bug” or “fixes
   bug”.  Explain the intend of the change not the actual contents which the
   diff already provides

   After the summary more paragraphs with detailed explanations may follow,
   wrapped at 72 characters.  Separate multiple paragraphs by blank lines.

   You may use simple formatting like *emphasis* or _underline_, but keep
   it to a minimum.  Commit messages are not in Markdown :)

   Commit messages may reference issues by number, like this: See GH-42.
   Please use `GH-` to prefix issue numbers.  You may also close issues
   like this: Fixes GH-42 and closes GH-42.

`Git Commit`_ and Magit_ provide Emacs mode for Git commit messages, which helps
you to comply to these guidelines.

.. seealso::

   `A Note About Git Commit Messages`_
      Further information about good commit messages, including some motivation
      for our rules for commit messages.

.. _Git Commit: https://github.com/magit/magit/
.. _Magit: https://github.com/magit/magit/
.. _A Note About Git Commit Messages: https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
