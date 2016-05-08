====================
 Maintainer’s Guide
====================

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

* Please do not commit directly to ``master`` unless it’s a trivial change,
  a safe refactoring, a small bug or spelling fix, etc.  If in doubt please use
  a separate branch and open a `pull request`_.
* Please commit new features, larger changes and refactorings and updates to
  documentation to separate branches and open a pull request for review and
  discussion.

.. important::

   When creating a new branch please use a *descriptive name* to communicate the
   purpose of the branch to other developers and maintainers.  ``fix-bug-42`` is
   not a great name, but ``42-fix-void-function-error-in-error-list`` is.

   If your branch addresses a specific Github issue please name your branch
   :samp:`{issue}-{description}`, where ``issue`` is the number of the Github
   issue *without* any prefix and ``description`` is the description of the
   branch.  This convention helps us to link branches to issues and has the
   added bonus of automatically moving issues into "In progress" on our `Waffle
   board`_.

We do not enforce these rules to give you the freedom to ignore them when need
be, like in the case of a very urgent but non-trivial bug fix.  But please do
try to follow these rules most of the time as they help us to maintain a high
code quality in ``master``.

For :ref:`maintainers <flycheck-maintainers>` these rules are relaxed: They may
commit to any branch at any time.  Nonetheless we also recommend that
maintainers open pull requests for discussion.

.. _pull request: https://help.github.com/articles/using-pull-requests/
.. _waffle board: https://waffle.io/flycheck/flycheck

.. _flycheck-pull-requests:

Pull requests
-------------

.. todo:: Explain how to review and merge pull requests

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

All :ref:`maintainers <flycheck-maintainers>` have administrative access to
these services so in case of an issue just contact them.

.. _Github: https://github.com/flycheck
.. _ReadTheDocs: https://readthedocs.org/projects/flycheck/
.. _Travis CI: https://travis-ci.org/flycheck/flycheck

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
3. Announce the new release in our Gitter_ channel and wherever else you see fit.

.. _release information: https://github.com/flycheck/flycheck/releases
.. _versions dashboard: https://readthedocs.org/dashboard/flycheck/versions/
.. _Gitter: https://gitter.im/flycheck/flycheck
