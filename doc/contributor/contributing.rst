.. _flycheck-contributors-guide:

=====================
 Contributor’s Guide
=====================

Thank you very much for your interest in contributing to Flycheck! We’d like to
warmly welcome you in the Flycheck community, and hope that you enjoy your time
with us!

There are many ways to contribute to Flycheck, and we appreciate all of them. We
hope that this document helps you to contribute. If you have questions, please
ask on our `issue tracker`_ or in our `Gitter chatroom`_.

For a gentle start please take a look at all the things we `need your help
with`_ and look for `beginner-friendly tasks`_.

Please note that all contributors are expected to follow our :ref:`Code of
Conduct <flycheck-conduct>`.

.. _issue tracker: https://github.com/flycheck/flycheck/issues
.. _Gitter chatroom: https://gitter.im/flycheck/flycheck
.. _need your help with: https://github.com/flycheck/flycheck/issues?q=is%3Aissue+is%3Aopen+label%3A%22needs+help%22
.. _beginner-friendly tasks: https://github.com/flycheck/flycheck/labels/beginner%20friendly

.. _flycheck-bug-reports:

Bug reports
===========

Bugs are a sad reality in software, but we strive to have as few as possible in
Flycheck. Please liberally report any bugs you find. If you are not sure whether
something is a bug or not, please report anyway.

If you have the chance and time please `search existing issues`_, as it’s
possible that someone else already reported your issue. Of course, this doesn’t
always work, and sometimes it’s very hard to know what to search for, so this is
absolutely optional. We definitely don’t mind duplicates, please report
liberally.

To open an issue simply fill out the `issue form`_. To help us fix the issue,
include as much information as possible. When in doubt, better include too much
than too little. Here’s a list of facts that are important:

* What you did, and what you expected to happen instead
* Whether and how you were able to `reproduce the issue in emacs -Q`_
* Your Flycheck setup from ``M-x flycheck-verify-setup``

.. _search existing issues: https://github.com/flycheck/flycheck/issues?q=is%3Aissue
.. _issue form: https://github.com/flycheck/flycheck/issues/new
.. _reproduce the issue in emacs -Q: https://emacs.stackexchange.com/questions/28429/how-do-i-troubleshoot-emacs-problems


.. _flycheck-windows-issues:

Windows-only issues
-------------------

As Flycheck does not support Windows officially we generally do *not* attempt to
fix issues that only occur on Windows. We will move all Windows-only issues to
the `list of open Windows issues`_, and leave them to Windows users and
developers.

We welcome anyone who wants to fix open Windows issues, and we will merge pull
requests for improved Windows compatibility. If you know Windows and Emacs,
please take a look at the list of open Windows issues and try to fix any of
these.

.. _list of open Windows issues: https://github.com/flycheck/flycheck/labels/arch%3A%20windows%20only

Feature requests
================

To request a new feature please open a new issue through our `issue form`_.
A feature request needs to find a core developer or maintainer who adopts and
implements it.

The build system
================

Flycheck provides a :file:`Makefile` with some convenient targets to compile and
test Flycheck.  The Makefile requires Cask_, the Emacs Lisp dependency manager.
Run ``make help`` to see a list of all available targets.  Some common ones are:

- ``make init`` initialises the project by installing local Emacs Lisp
  dependencies.
- ``make check`` checks all Emacs Lisp sources.  This target requires Emacs 25.
- ``make compile`` compiles Flycheck and its libraries to byte code.
- ``make format`` formats all Emacs Lisp sources.
- ``make specs`` runs all Buttercup_ specs for Flycheck.  Set :makevar:`PATTERN`
  to run only specs matching a specific regular expression, e.g. ``make
  PATTERN='^Mode Line' specs`` to run only tests for the mode line.
- ``make unit`` runs all ERT unit tests for Flycheck.  We are phasing ERT out in
  favour of Buttercup; no new ERT unit tests will be added and this target will
  eventually be removed.
- ``make integ`` runs all integration tests for Flycheck syntax checkers.  These
  tests are dependent on the checker programs and their versions; expect
  failures when running this target with bleeding-edge checkers.  Set
  :makevar:`SELECTOR` to run only tests matching a specific ERT selector,
  e.g. ``make SELECTOR='(language haskell)' integ`` to run only integration
  tests for Haskell.  ``make LANGUAGE=haskell integ`` is a shortcut for this.

  If you want to replicate the integration tests that are run on the CI,
  continue reading.

.. _Cask: http://cask.readthedocs.io/
.. _Buttercup: https://github.com/jorgenschaefer/emacs-buttercup

Running all the integration tests
=================================

To run all the integration tests, you need to have all the syntax checkers
installed.  As that can be tedious work, and since your locally installed tools
can have different versions than the tools used on the CI, we have created a
Docker image with most of the supported checkers.  To use the Docker image
locally and replicate the integration tests that are run on the CI, first you
need to build the image::

  cd flycheck
  docker pull flycheck/emacs-cask:26.2
  docker pull flycheck/all-tools:latest
  docker build --build-arg EMACS_VERSION=26.2 --tag tools-and-emacs:26.2 -f .travis/tools-and-emacs .

Replace ``26.2`` by the Emacs version you want to test.  See the available
versions on `docker hub`_.

Once the image is built, you can use it to run the integration tests::

  docker run --rm -it -v `pwd`:/flycheck --workdir /flycheck tools-and-emacs:26.2 /bin/bash -c "make integ"

Note that the ``all-tools`` image is rebuilt each month, so the versions of the
its syntax checkers will change accordingly.  You can check the version of each
installed tool by running the ``check-tools`` script in the image::

  docker run --rm -it -v `pwd`:/flycheck --workdir /flycheck tools-and-emacs:26.2 check-tools

.. _docker hub: https://hub.docker.com/r/flycheck/emacs-cask/tags

Pull requests
=============

Pull Requests are the primary mechanism to submit your own changes to
Flycheck. Github provides great documentation about `Pull Requests`_.

.. _Pull Requests: https://help.github.com/articles/using-pull-requests/

Please make your pull requests against the ``master`` branch.

Use ``make check specs unit`` to test your pull request locally. When making
changes to syntax checkers of a specific language, it’s also a good idea to run
:samp:`make LANGUAGE={language} integ` and check whether the tests for the
particular language still work.  A successful ``make integ`` is by no means
mandatory for pull requests, though, the continuous integration will test your
changes, too.

.. important::

   To contribute to Flycheck you must sign our CLA_ (Contributor License
   Agreement).  The CLA Assistant bot will automatically ask you to do this when
   you open a pull request, and will let you sign the CLA through your Github
   account.

   We require this process mostly to make you aware of the licensing
   implications of contributing to Flycheck and to obtain your explicit approval
   of our licenses for your contribution.

   .. _CLA: https://gist.github.com/lunaryorn/c9c0d656fe7e704da2f734779242ec99

All pull requests go through a two-stage review process:

* :ref:`Maintainer <flycheck-maintainers>` review the general idea and direction
  of the pull request and leave a ``LGTM`` comment if they believe that the
  change is a good addition to Flycheck.  We currently require at least one
  approval from a maintainer.
* :ref:`All contributors <flycheck-language-teams>`—language teams in
  particular—check the technical implementation of a pull request through `pull
  request reviews`_, and either approve it or request changes.  We currently
  require at least one approval and no requested changes.

.. important::

   We have a comprehensive :ref:`flycheck-style-guide` that explains what
   features we will accept, how our code should look likewise, what tests we
   require, how commit messages should look like, and so on.

   Take a look at it to see what we look for in a code review.

Additionally all pull requests go through automated tests on `Travis CI`_ which
check code style, run unit tests, etc

Feel free to mention individual contributors or entire teams
(e.g. ``@flycheck/maintainers`` or ``@flycheck/javascript``) to ask for help or
feedback or request a review.  Please mention the maintainers
(``@flycheck/maintainers``) if you think that your pull request has been waiting
for review too long.  You can expect a first response to any pull request in a
couple of days.

Once the pull request passed review and automated tests we will merge it.  We
may also ask you whether you'd like to join Flycheck and help us, thus giving
you commit access to our repository and let you merge your own pull request.

.. _pull request reviews: https://help.github.com/articles/about-pull-request-reviews/
.. _Travis CI: https://travis-ci.org/flycheck/flycheck/pull_requests

Writing documentation
=====================

Documentation improvements are very welcome.  Flycheck’s manual is written in
reStructuredText_ and built with Sphinx_.  The source of the manual resides in
the ``doc/`` directory.

You need Python 3.4 or newer to install Sphinx_ for Flycheck’s documentation.
On macOS it is recommended that you use Homebrew_ to install the latest Python
version with ``brew install python3``.  On Linux you should be able to obtain
Python 3.4 from the package manager of your distribution.

With Python 3 installed change into the ``doc/`` directory and run ``make init``
to install Sphinx and related tools required for Flycheck’s documentation.  We
recommend that you use virtualenv_ to avoid a global installation of Python
modules.  ``make init`` will warn you if you do not.

When editing documentation run ``make html-auto`` to view the results of your
edits.  This target runs a local webserver at http://localhost:8000 which serves
the HTML documentation and watches the documentation sources for changes to
rebuild automatically.  When you have finished your edits it is a good idea to
run ``make linkcheck`` to verify all links in the documentation.  Note that this
target can take a while especially when run on a clean build.

Run ``make help`` to see a list of all available Make targets for the
documentation.

Documentation pull requests work in the same way as other pull requests.  To
find documentation issues sort by the `documentation`_ label.

.. _ReStructuredText: http://docutils.sourceforge.net/rst.html
.. _Sphinx: http://www.sphinx-doc.org
.. _Homebrew: https://brew.sh
.. _virtualenv: https://virtualenv.pypa.io/en/latest/
.. _documentation: https://github.com/flycheck/flycheck/labels/documentation

Issue management
================

We use Github labels for basic issue management:

- **The red “bug” label denotes critical bugs in Flycheck that must be fixed
  urgently.**
- Violet labels describe the area of Flycheck the issue belongs to.
- The green “beginner friendly” label denotes easy tasks for newcomers to the
  project.
- Orange labels denote blockers.
- Grey labels indicate resolutions to issues.

Out of tree contributions
=========================

There are many ways that you can contribute to Flycheck that go beyond
this repository.

Answer questions in our `Gitter channel`_ or on StackExchange_.

Participate in Flycheck discussions in other Emacs communities and help
users with troubles.

Write :ref:`extensions for Flycheck <flycheck-extensions>`.

.. _Gitter channel: https://gitter.im/flycheck/flycheck
.. _StackExchange: https://emacs.stackexchange.com/questions/tagged/flycheck

--------------

This contributing guide is heavily inspired by `Rust’s excellent
contributing
information <https://github.com/rust-lang/rust/blob/master/CONTRIBUTING.md>`__.
