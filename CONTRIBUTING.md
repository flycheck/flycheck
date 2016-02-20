# Contribution guidelines #

Thank you very much for your interest in contributing to Flycheck!  We’d like to
warmly welcome you in the Flycheck community, and hope that you enjoy your time
with us!

There are many ways to contribute to Flycheck, and we appreciate all of them.
We hope that this document helps you to contribute.  If you have questions,
please ask on our [issue tracker][] or in our [Gitter chatroom][gitter].

For a gentle start please take a look at all the things we
[need your help with][S-needs your love] and look for
[beginner-friendly tasks][E-beginner friendly].

Please note that all contributors are expected to follow our
[Code of Conduct][coc].

[issue tracker]: https://github.com/flycheck/flycheck/issues
[gitter]: https://gitter.im/flycheck/flycheck
[S-needs your love]: https://github.com/flycheck/flycheck/issues?q=is%3Aissue+is%3Aopen+label%3A%22S-needs+your+love%22
[E-beginner friendly]: https://github.com/flycheck/flycheck/labels/E-beginner%20friendly
[coc]: http://www.flycheck.org/conduct.html

## Bug reports ##

Bugs are a sad reality in software, but we strive to have as few as possible in
Flycheck.  Please liberally report any bugs you find.  If you are not sure
whether something is a bug or not, please report anyway.

If you have the chance and time please [search existing issues][issues], as it’s
possible that someone else already reported your issue.  Of course, this doesn’t
always work, and sometimes it’s very hard to know what to search for, so this is
absolutely optional.  We definitely don’t mind duplicates, please report
liberally.

To open an issue simply fill out the [issue form][].  To help us fix the issue,
include as much information as possible.  When in doubt, better include too much
than too little.  Here’s a list of facts that are important:

* What you did, and what you expected to happen instead
* Whether and how you were able to [reproduce the issue in emacs -Q][emacsQ]
* Your Flycheck setup from `M-x flycheck-verify-setup`
* Your operating system
* Your Emacs version from `M-x emacs-version`
* Your Flycheck version from `M-x flycheck-version`

[issues]: https://github.com/flycheck/flycheck/issues?utf8=✓&q=is%3Aissue
[issue form]: https://github.com/flycheck/flycheck/issues/new
[emacsQ]: http://www.lunaryorn.com/2015/11/29/reproduce-bugs-in-emacs-Q.html

### Windows-only issues ###

As Flycheck does not support Windows officially we generally do *not* attempt
to fix issues that only occur on Windows.  We will move all Windows-only issues
to the [list of open Windows issues][windows], and leave them to Windows
users and developers.

We welcome anyone who wants to fix open Windows issues, and we will merge pull
requests for improved Windows compatibility.  If you know Windows and Emacs,
please take a look at the list of open Windows issues and try to fix any of
these.

[windows]: https://github.com/flycheck/flycheck/labels/B-Windows%20only

## Feature requests ##

To request a new feature please open a new issue through our [issue form][].

A feature request needs to find a core developer or maintainer who adopts and
implements it.  Otherwise we will move the issue to the [S-needs your love][]
column of our [board][] where issues sit that wait for a pull request from the
community.

[board]: https://waffle.io/flycheck/flycheck

## The Build system ##

While Emacs Lisp per se doesn’t need to be compiled, Flycheck’s build system
provides many tasks to verify sources, run tests, generate documentation and
more.  It’s a good little helper when working on Flycheck, allowing you to
verify and test your changes before submitting them.

Our build system uses [Rake][] which is part of [Ruby][] and included in all
Ruby installations.  Any Ruby 2.x version will suffice.  On OS X, Rake is
preinstalled, on Linux systems it’s easy to install through the package manager,
e.g. `sudo apt-get install rake` on Ubuntu.

Additionally the build system requires [Cask][], the Emacs Lisp dependency
manager.

The build system is defined in `Rakefile`, with additional code in `admin/`.
Run `rake help` for a quick help about the Build System.

[rake]: https://github.com/ruby/rake
[Ruby]: https://www.ruby-lang.org/
[Cask]: http://cask.readthedocs.org/

### Optional tools ###

The build system makes use of additional tools for some tasks:

* `generate:logo`: [Inkscape][] and [ImageMagick][]
* `verify:travis`: [Travis Client][]
* `verify:ruby`: [Rubocop][]
* `verify:markdown`: [markdownlint][]
* `doc:*`: [Texinfo][] (version 5 or newer)
* `test:integration:html`: [html-proofer][]

We recommend that you install all tools for `verify:*` with `gem install travis
rubocop markdownlint` if you intend to work on Flycheck over a longer time.

[Rubocop]: https://github.com/bbatsov/rubocop
[markdownlint]: https://github.com/mivok/markdownlint
[Texinfo]: https://www.gnu.org/software/texinfo/
[html-proofer]: https://github.com/gjtorikian/html-proofer
[Travis Client]: https://github.com/travis-ci/travis.rb
[ImageMagick]: http://www.imagemagick.org/
[Inkscape]: https://inkscape.org/

### Useful tasks ###

Some common targets are:

* `rake init`: Initialise the project, by installing Emacs Lisp dependencies
  locally.
* `rake check:language[LANGUAGE]` runs all tests for the given `LANGUAGE`,
  e.g. `rake check:language[javascript]`.  Maintainers will use this task when
  testing pull requests that make changes to Flycheck’s support for a language,
  such as adding a new pull request.
* `rake check:fast` verifies all sources, byte-compiles Flycheck, and runs all
  unit tests.  You need the corresponding tools from the previous section.  This
  task runs on Travis CI on all contributions so it’s a good idea to run this
  task before submitting your contributions, albeit not a requirement, as Travis
  CI runs it anyway.
* `rake check:doc` generates and tests the documentation.  As before, you need
  the corresponding tools from the previous section.  This task also runs on
  Travis CI when building and deploying the manual.  If you made changes to the
  documentation it’s a good idea to run this task before submitting your
  changes.  You can also use the faster `rake doc:info` which generates the
  manual and checks syntactic correctness, but does not verify hyperlinks in the
  manual.

## Pull requests ##

Pull Requests are the primary mechanism to submit your own changes to Flycheck.
Github provides [great documentation][pull-requests] about Pull Requests.

Please make your pull requests against the `master` branch.

Use `rake check:fast` to test your pull request locally.  When making changes to
syntax checkers of a specific language, it’s also a good idea to run `'rake
check:language[LANGUAGE]'` to run all tests for the given `LANGUAGE`.

All pull requests are reviewed by a [maintainer][maintainers].  Feel free to
mention individual developers (e.g. `@lunaryorn`) to request a review from a
specific person, or `@flycheck/maintainers` if you have general questions or if
your pull request was waiting for review too long.

Additionally, all pull requests go through automated tests on
[Travis CI][travis-prs] which check code style, run unit tests, etc.  After the
pull request was reviewed and if all tests passed a maintainer will eventually
cherry-pick or merge your changes and close the pull request.

[pull-requests]: https://help.github.com/articles/using-pull-requests/
[travis-prs]: https://travis-ci.org/flycheck/flycheck/pull_requests
[maintainers]: http://www.flycheck.org/people.html#maintainers

### Commit guidelines ###

The art of writing good commit messages is a wide subject.  This model commit
message illustrates our style:

    Fix a foo bug

    The first line is the summary, 50 characters or less.  Write in the
    imperative and in present tense: “Fix bug”, not “fixed bug” or “fixes
    bug”.

    After the summary more paragraphs with detailed explanations may follow,
    wrapped at 72 characters.  Separate multiple paragraphs by blank lines.

    You may use simple formatting like *emphasis* or _underline_, but keep
    it to a minimum.  Commit messages are not in Markdown :)

    Commit messages may reference issues by number, like this: See GH-42.
    Please use `GH-` to prefix issue numbers.  You may also close issues
    like this: Fixes GH-42 and closes GH-42.

[Git Commit][] and [Magit][] provide Emacs mode for Git commit messages, which
helps you to comply to these guidelines.

[Git Commit]: https://github.com/magit/magit/
[Magit]: https://github.com/magit/magit/

## Writing documentation ##

Documentation improvements are very welcome.  The source of Flycheck’s manual is
in the `doc/` directory.  The manual is written in [Texinfo][] for best
integration in Emacs’ built-in manual viewer.  Some syntax and concepts of
Texinfo are archaic and unusual, but the [Texinfo manual][] provides a good
introduction and documentation for this format.

Documentation pull requests work in the same way as other pull requests.  To
find documentation issues sort by the [A-documentation][] label.

To preview the documentation run `rake doc:info` to generate the Info manual in
`doc/flycheck.info` and type `C-u C-h i /path/to/flycheck/doc/flycheck.info` in
Emacs.  Alternatively you may use `rake doc:html` to generate a single-page HTML
file in `doc/flycheck.html` which you can view in your browser.

[Texinfo manual]: https://www.gnu.org/software/texinfo/manual/texinfo/html_node/index.html
[A-documentation]: https://github.com/flycheck/flycheck/labels/A-documentation

## Issue management ##

We manage all issues and pull requests on our [Waffle board][board].  The board
has six columns which correspond to `S-` labels on Github:

* The *Backlog* (no `S` label) holds all incoming issues.  Pull requests waiting
  for review sit here, as well as bugs that were reported or stories and tasks
  that are not ready to work on yet.
* In *Ready* (`S-ready` label) we keep issues that we are ready to work on.
  This includes bugs which we can reproduce and fix, and pull requests that were
  reviewed and are ready to be merged now.  Look at this column to see what’s
  coming next to Flycheck.
* When we start to work on an issue it moves into *In Progress* (`S-in progress`
  label).
* *Blocked* (`S-blocked` label) issues are waiting for something, like a change
  in an upstream project or a feedback from another developer.  A `B-` label may
  provide additional clue why the issue is blocked.  Blocked issues may also
  appear in the backlog, but in this column we actively seek to remove the
  blockers and move the issue to *Ready*.
* *Community* (`S-needs your love` label) issues are those that we will not work
  on ourselves.  These issues need pull requests from the community to be
  solved.  Look at this column to find spots to contribute to.
* Eventually issues move into *Done* when they are closed.

In addition to these columns which reflect the basic issue workflow we also use
a variety of labels to group issues:

* Yellow, **A**-prefixed labels describes the area of Flycheck the issue belongs
  to.
* Orange, **B**-prefixed labels gives reasons why an issue is blocked.
* Green, **E**-prefixed labels denotes the level of experience necessary to
  address an issue.
* Blue, **K**-prefixed labels tells the kind of an issue, i.e. whether it’s a
  bug, a feature request, etc.
* Grey, **R**-prefixed labels inform about the resolution of an issue.

## Out of tree contributions ##

There are many ways that you can contribute to Flycheck that go beyond this
repository.

Answer questions in our [Gitter channel][gitter] or on [StackExchange][sx].

Participate in Flycheck discussions in other Emacs communities and help users
with troubles.

Write [extensions to Flycheck][extensions].

[sx]: https://emacs.stackexchange.com/questions/tagged/flycheck
[extensions]: http://www.flycheck.org/extensions.html

---

This contributing guide is heavily inspired by
[Rust’s excellent contributing information](https://github.com/rust-lang/rust/blob/master/CONTRIBUTING.md).
