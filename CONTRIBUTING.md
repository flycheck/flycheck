Contribution guidelines
=======================

If you discovered bugs and issues, have ideas for improvements or new features,
or want to contribute a new syntax checker, please report to the
[issue tracker][] the repository and send a pull request, but respect the
following guidelines.

[issue tracker]: https://github.com/flycheck/flycheck/issues

Reporting issues
================

Please report issues to the [issue tracker][].

- Check that the issue has not already been reported.
- Check that the issue has not already been fixed in the latest code.
- Check that your setup is fine with `M-x flycheck-verify-setup`.
- Be clear and precise (do not prose, but name functions and commands exactly).
- Include the version of Flycheck as shown by `M-x flycheck-version`.
- Open an issue with a clear title and description in grammatically correct,
  complete sentences.

Contributing code
=================

Contributions of code, either as pull requests or as patches, are *very*
welcome, but please respect the following guidelines.

General
-------

- Write good and *complete* code.
- Provide use cases and rationale for new features.

Code style
----------

- Generally, use the same coding style and spacing.
- Do not use tabs for indentation.
- Add docstrings for every declaration.
- Make sure your code compiles without warnings with `make compile`, and has no
  checkdoc issues with `M-x checkdoc-buffer` or `C-c ? d`.  If you are using
  Flycheck, just make sure that your code has no Flycheck warnings.

Commit messages
---------------

Write commit messages according to [Tim Pope's guidelines][]. In short:

- Start with a capitalized, short (50 characters or less) summary, followed by a
  blank line.
- If necessary, add one or more paragraphs with details, wrapped at 72
  characters.
- Use present tense and write in the imperative: “Fix bug”, not “fixed bug” or
  “fixes bug”.
- Separate paragraphs by blank lines.
- Do *not* use special markup (e.g. Markdown).  Commit messages are plain text.
  You may use `*emphasis*` or `_underline_` though, following conventions
  established on mailing lists.

This is a model commit message:

    Capitalized, short (50 chars or less) summary

    More detailed explanatory text, if necessary.  Wrap it to about 72
    characters or so.  In some contexts, the first line is treated as the
    subject of an email and the rest of the text as the body.  The blank
    line separating the summary from the body is critical (unless you omit
    the body entirely); tools like rebase can get confused if you run the
    two together.

    Write your commit message in the imperative: "Fix bug" and not "Fixed bug"
    or "Fixes bug."  This convention matches up with commit messages generated
    by commands like git merge and git revert.

    Further paragraphs come after blank lines.

    - Bullet points are okay, too

    - Typically a hyphen or asterisk is used for the bullet, followed by a
      single space, with blank lines in between, but conventions vary here

    - Use a hanging indent

[Git Commit][] and [Magit][] provide Emacs mode for Git commit messages, which
helps you to comply to these guidelines.

[Tim Pope's guidelines]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[Git Commit]: https://github.com/magit/magit/
[Magit]: https://github.com/magit/magit/

Contributing syntax checkers
----------------------------

For syntax checkers, some special guidelines apply in addition to the above:

- Provide a link to the website of the syntax checker tool in the comments of
  your pull request.
- Add a proper docstring to your syntax checker, including this URL.
- Add unit tests for your syntax checker, or provide example code that triggers
  errors for each error pattern of the syntax checker.
- Make sure that your new syntax checker passes all required tests, with
  `test/run.el (new-checker-for LANGUAGE)`, where `LANGUAGE` is the programming
  language the checker is for.
- Ideally, extend the [Flycheck testing VM][] with recipes to install the new
  syntax checker.

Unit tests that can run on Travis CI are **mandatory** for all syntax checkers
in Flycheck.

[Flycheck testing VM]: https://github.com/flycheck/flycheck-vm

Pull requests
-------------

- Use a **topic branch** to easily amend a pull request later, if necessary.
- Do **not** open new pull requests, when asked to improve your patch.  Instead,
  amend your commits with `git rebase -i`, and then update the pull request with
  `git push --force`
- Open a [pull request][] that relates to but one subject with a clear title and
  description in grammatically correct, complete sentences.

Pull requests **must** pass all tests on Travis CI before being merged.

[pull request]: https://help.github.com/articles/using-pull-requests
