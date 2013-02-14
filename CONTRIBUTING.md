Contributing
============

If you discovered bugs and issues, have ideas for improvements or new features,
or want to contribute a new syntax checker, please report to the
[issue tracker][1] the repository and send a pull request, but respect the
following guidelines.


Issue reporting
---------------

- Check that the issue has not already been reported.
- Check that the issue has not already been fixed in the latest code.
- Be clear and precise (do not prose, but name functions and commands exactly).
- Open an issue with a clear title and description in grammatically correct,
  complete sentences.


Pull requests
-------------

- Read [how to properly contribute to open source projects on Github][2].
- Use a topic branch to easily amend a pull request later, if necessary.
- Write [good commit messages][3].
- Use the same coding style and spacing.
- Verify your Emacs Lisp code with `checkdoc` (`C-c ? d`).
- Add unit tests for your new checkers.  Take a look at existing tests, and do
  not forget to update `vagrant/provision.py` to install your checker tool into
  the virtual testing environment.
- Open a [pull request][4] that relates to but one subject with a clear title
  and description in grammatically correct, complete sentences.


[1]: https://github.com/lunaryorn/flycheck/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
