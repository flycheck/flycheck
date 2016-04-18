====================
 Maintainer’s Guide
====================

.. _flycheck-git-signatures:

Signatures for commits and tags
===============================

We sign all release tags as part of our :ref:`flycheck-release-process`.  Thus
you need a GPG key pair for Git.  Github provides a great guide which helps you
to `generate a key`_ and to `tell Git about your key`_.  Please also `add your
key`_ to your Github account.

We also recommend that you sign all your commits with your key.  Again, Github
provides a good guide to `sign commits`_.

.. seealso::

   `Signing Your Work`_
      For more information about signing commits and tags take a look at the
      section in the Git manual.

.. _Signing Your Work: https://git-scm.com/book/uz/v2/Git-Tools-Signing-Your-Work
.. _generate a key: https://help.github.com/articles/generating-a-gpg-key/
.. _tell Git about your key: https://help.github.com/articles/telling-git-about-your-gpg-key/
.. _add your key: https://help.github.com/articles/adding-a-new-gpg-key-to-your-github-account/
.. _sign commits: https://help.github.com/articles/signing-commits-using-gpg/

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

.. _flycheck-release-process:

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
pushes it to Github.

.. note::

    The tag is *signed*; you must configure Git for :ref:`signing commits and
    tags <flycheck-git-signatures>` before you make a release the first time.
    After pushing the new release to Github, the script bumps the version number
    again, to the next snapshot, and commits the changes again.

When the script has completed, please announce the new release in the public
Gitter_ channel, and wherever else you see fit.

.. _Gitter: https://gitter.im/flycheck/flycheck
