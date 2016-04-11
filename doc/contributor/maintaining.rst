====================
 Maintainer’s Guide
====================

.. _flycheck-tag-signing:

Commit and tag signing
======================

.. todo:: Signing policy for commits and tags, plus a short intro

Versioning and releases
=======================

We use a single continuously increasing version number for Flycheck.  Breaking
changes may occur at any point.

Please feel free to make a release whenever you think it’s appropriate.
It’s generally a good idea to release when

- you fixed an important bug that affects many users,
- there are a couple of new syntax checkers available,
- there’s a major new feature in ``master``,
- etc.

In doubt just make a release.  We aim to release early and frequently.  If
anything breaks anything we can just publish another release afterwards.

Release process
---------------

First, check that

1. you are on ``master``,
2. your working directory is clean, i.e. has no uncommitted changes or untracked
   files,
3. all commits are pushed,
4. and Travis CI passes for the latest commit on ``master``.

If all is good a new release is a simple as

.. code-block:: console

   $ make -C maint release

This runs the release script in :file:`maint/release.el`.  If any of the above
requirements isn't met the release script will signal an error and abort.

The release script bumps the version number, commits and tags a new release, and
pushes it to Github.  Note that the tag is *signed*; you must configure Git for
:ref:`signing commits and tags <flycheck-tag-signing>` before you make a release
the first time.  After pushing the new release to Github, the script bumps the
version number again, to the next snapshot, and commits the changes again.

When the script has completed, please announce the new release in the public
Gitter_ channel, and wherever else you see fit.

.. _Gitter: https://gitter.im/flycheck/flycheck
