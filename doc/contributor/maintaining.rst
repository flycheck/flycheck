====================
 Maintainer’s Guide
====================

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

To publish a new release change into the ``maint/`` directory, run ``make
release`` and follow the instructions.  The script publishes a signed tag for
the new release, so you need to have Git and GPG configured for `signing tags`_.

.. _signing tags: https://git-scm.com/book/uz/v2/Git-Tools-Signing-Your-Work#Signing-Tags

.. todo:: Coming soon!
