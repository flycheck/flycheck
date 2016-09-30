.. _flycheck-maintainers-guide:

====================
 Maintainer’s Guide
====================

Issue triage
============

Please label incoming tickets accordingly according to these rules:

- Add the “bug” label to things that you think **must be fixed urgently**.
  Please don’t use this label for bugs that do not severely impede Flycheck’s
  functionality.
- Add the “needs review” label to new bugs and pull requests that need to be
  reviewed.
- Add the “beginner friendly” label to really easy things.  If you add this
  label please also add a comment that outlines a possible solution.
- Add “blocked” to bugs that need further comment or help from the reporter, and
  to pull requests that need to be improved.
- Add “needs help” to anything that no contributor will work on, to mark the
  issue as available for external contributors and inform users that we will not
  work on the issue.
- Add “windows only” for bugs that appear to only affect Windows operating
  systems.

**If you’d like to review a bug or pull request please assign the corresponding
ticket to you.**

In issues for specific languages that Flycheck support please mention the
corresponding :ref:`language team <flycheck-language-teams>` if one exists.

Git workflow
============

Our Git workflow is simple:

* The ``master`` branch is always shippable.
* Every feature and every non-trivial change goes through a pull request.

GitHub calls this the “GitHub Flow” and has a very nice `visual guide`_ for this
model.

.. _visual guide: https://guides.github.com/introduction/flow/

.. _flycheck-branch-rules:

Branch rules
------------

Our workflow implies a couple of rules about which branches to push code to:

* Please commit new features, larger changes and refactorings and updates to
  documentation to separate branches and open a pull request for review and
  discussion.
* The ``master`` branch is protected.  Only :ref:`owners <flycheck-maintainers>`
  can push directly to it.  Everyone else needs to open a pull request.  Github
  requires maintainer approval and passing Travis CI tests before a pull request
  can be merged to master.

.. important::

   When creating a new branch please use a *descriptive name* to communicate the
   purpose of the branch to other developers and maintainers.  ``fix-bug-42`` is
   not a great name, but ``42-fix-void-function-error-in-error-list`` is.

.. _pull request: https://help.github.com/articles/using-pull-requests/

.. _flycheck-pull-requests:

Pull requests
-------------

Please review pull requests according to our :ref:`style guide
<flycheck-style-guide>`, and consider whether you personally think that the
change is a good addition to Flycheck.

**All pull requests require approval of a maintainer**.

To state your approval as a maintainer add a comment that contains ``LGTM``.
The LGTM.co will look for these comments and unlock the pull request once enough
maintainers approved it.  We require approvals from multiple maintainers, see
``.lgtm`` for the exact amount of approvals required to accept a pull request.

.. important::

   LGTM.co does not require repeated approval after changes to the pull
   request.  Hence you can “approve early”, i.e. approve before the pull request
   is polished.

   And it’s absolutely fine to do so.  If there are only minor changes left, if
   you trust the pull request author to address remaining issues, **feel free to
   approve early**, all the more if the pull request author is already a
   **contributor**.  In this case they’ll be able to directly merge their own
   pull request after making changes to it which decreases the turn-around time
   for pull requests.

Merge guidelines
~~~~~~~~~~~~~~~~

If a pull request was approved you may directly merge it.  For smaller pull
requests please “Squash and Merge” to keep a linear history, otherwise merge
normally.  What constitutes a "small" pull request is at your discretion.  Apply
common sense :)

You may also add the author of the pull request to the "Core developers" team to
give them commit access to the Flycheck repository and ask them merge the pull
request themselves.  That's a good way to gain new contributors.

.. _flycheck-git-signatures:

Signatures for commits and tags
-------------------------------

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

Tooling and Services
====================

In addition to Github_ where we host code and do code reviews we use a bit of
extra tooling and some 3rd party services for Flycheck:

* ReadTheDocs_ hosts http://www.flycheck.org and automatically rebuilds it on
  every change.  It works mostly automatically and requires little
  configuration.
* `Travis CI`_ runs our tests after every push and for every pull request.
  It's configured through ``.travis.yml``.
* LGTM_ handles the pull request approval process through ``LGTM`` comments.
  It's configured through ``.lgtm``, the list of maintainers that may approve
  pull requests is in the ``MAINTAINERS`` file.
* `CLA assistant`_ checks signatures to our CLA_ and allows contributors to sign
  the CLA through their Github account.

All :ref:`maintainers <flycheck-maintainers>` have administrative access to
these services so in case of an issue just contact them.

.. _Github: https://github.com/flycheck
.. _ReadTheDocs: https://readthedocs.org/projects/flycheck/
.. _Travis CI: https://travis-ci.org/flycheck/flycheck
.. _LGTM: https://lgtm.co/
.. _CLA assistant: https://cla-assistant.io
.. _CLA: https://gist.github.com/lunaryorn/c9c0d656fe7e704da2f734779242ec99

.. _flycheck-maintenance-scripts:

Maintenance scripts
===================

Administrative processes are tedious and time-consuming, so we try to automate
as much as possible.  The :file:`maint/` directory contains many scripts for
this purpose.  ``make -C maint/ help`` provides an overview over all
administrative tasks.

Most of these scripts require Python 3.5 and additional Python libraries.  On OS
X it is recommended that you use Homebrew_ to install the latest Python version
with ``brew install python3``.  On Linux you should be able to obtain Python 3.5
from the package manager of your distribution.

To install all required libraries run ``make -C maint init``.  We recommend that
you use virtualenv_ to avoid a global installation of Python modules.  ``make
init`` will warn you if you do not.

.. _Homebrew: http://brew.sh
.. _virtualenv: https://virtualenv.pypa.io/en/latest/

Versioning and releases
=======================

We use a single continuously increasing version number for Flycheck.

.. important::

   Breaking changes may occur **at any point**.

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

This runs the release script in :file:`maint/release.py`.  If any of the above
requirements isn't met the release script will signal an error and abort.

The release script bumps the version number, commits and tags a new release, and
pushes it to Github.

.. note::

    The tag is *signed*; you must configure Git for :ref:`signing commits and
    tags <flycheck-git-signatures>` before you make a release the first time.
    After pushing the new release to Github, the script bumps the version number
    again, to the next snapshot, and commits the changes again.

Once the script is completed please

1. Edit the `release information`_ on Github and add a short summary about the
   release.  Don’t forget to add a link to the complete changelog and upload the
   package TAR file.
2. Enable the new release on the ReadTheDocs `versions dashboard`_.
3. Announce the new release in our Gitter_ channel, on `emacs_flycheck`_ Twitter
   and wherever else you see fit.

.. _release information: https://github.com/flycheck/flycheck/releases
.. _versions dashboard: https://readthedocs.org/dashboard/flycheck/versions/
.. _Gitter: https://gitter.im/flycheck/flycheck
.. _emacs_flycheck: https://twitter.com/emacs_flycheck

New maintainers
===============

To propose a new maintainer open a pull request that adds the user to
``MAINTAINERS`` and ``doc/community/people.rst``.  The pull request is subject
to the :ref:`same rules <flycheck-pull-requests>` as all other pull requests.
Notably it goes through the same approval process.

Once merged please also

- add the new maintainer to the ``Maintainers`` team of the Github
  organisation.  This does not award additional privileges, it's just to support
  ``@flycheck/maintainers`` mentions for the sake of convenience,
- invite the new maintainer to the internal `Maintainers channel`_ on Gitter,
- and announce the new maintainer on Flycheck's Twitter account.

.. _Maintainers channel: https://gitter.im/flycheck/maintainers
