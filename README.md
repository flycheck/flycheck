# [![Flycheck][logo]](https://www.flycheck.org)

[![License GPL 3](https://img.shields.io/github/license/flycheck/flycheck.svg)][COPYING]
[![Join the chat](https://img.shields.io/gitter/room/flycheck/flycheck.svg)](https://gitter.im/flycheck/flycheck)
[![MELPA](https://melpa.org/packages/flycheck-badge.svg)](https://melpa.org/#/flycheck)
[![MELPA stable version](http://stable.melpa.org/packages/flycheck-badge.svg)](https://stable.melpa.org/#/flycheck)
[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/flycheck.svg)](https://elpa.nongnu.org/nongnu/flycheck.html)
[![CI](https://github.com/flycheck/flycheck/actions/workflows/test.yml/badge.svg)](https://github.com/flycheck/flycheck/actions/workflows/test.yml)
[![Docs](https://github.com/flycheck/flycheck/actions/workflows/docs.yml/badge.svg)](https://github.com/flycheck/flycheck/actions/workflows/docs.yml)
[![Lint Python](https://github.com/flycheck/flycheck/actions/workflows/lint-python.yml/badge.svg)](https://github.com/flycheck/flycheck/actions/workflows/lint-python.yml)

<https://www.flycheck.org>

Modern on-the-fly syntax checking extension for GNU Emacs.  [Try it][]!

![](https://raw.githubusercontent.com/flycheck/flycheck/master/doc/images/flycheck-annotated.png)

## Getting Started

Flycheck is available for installation with `package.el` on [NonGNU
ELPA](https://elpa.nongnu.org/nongnu), [MELPA
Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

You can install Flycheck with the following command:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `flycheck` <kbd>[RET]</kbd>

Finally add this to your Emacs config:

```elisp
(global-flycheck-mode +1)
```

Alternatively, if you're into `use-package` you can do the following:

``` emacs-lisp
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
```

Now you can start using any of the [bundled checkers](https://www.flycheck.org/en/latest/languages.html) or install additional checkers.

For a more gentle introduction read the [Installation][] instructions and go
through [Quickstart][] guide.

## Getting Help

Please ask questions about Flycheck on [Stack Exchange][sx] or in our
[Gitter chat][gitter], and report bugs to our [issue tracker][].

## Contributing

We welcome all kinds of contributions, whether you write patches, open pull
requests, write documentation, help others with Flycheck issues, or just tell
other people about your experiences with Flycheck.  Please take a look at our
[Contributor’s Guide][contrib] for help and guidance about contributing to
Flycheck.

## Sponsoring

You can support financially the development of Flycheck and related packages
via:

- [Open Collective](https://opencollective.com/flycheck)
- [GitHub Sponsors](https://github.com/sponsors/bbatsov)
- [Patreon](https://www.patreon.com/bbatsov)
- [PayPal](https://www.paypal.me/bbatsov)

### Open Collective Backers

<a href="https://opencollective.com/flycheck/backer/0/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/0/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/1/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/1/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/2/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/2/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/3/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/3/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/4/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/4/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/5/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/5/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/6/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/6/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/7/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/7/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/8/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/8/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/9/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/9/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/10/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/10/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/11/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/11/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/12/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/12/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/13/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/13/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/14/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/14/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/15/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/15/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/16/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/16/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/17/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/17/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/18/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/18/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/19/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/19/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/20/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/20/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/21/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/21/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/22/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/22/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/23/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/23/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/24/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/24/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/25/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/25/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/26/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/26/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/27/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/27/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/28/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/28/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/backer/29/website" target="_blank"><img src="https://opencollective.com/flycheck/backer/29/avatar.svg"></a>

### Open Collective Sponsors

Become a sponsor and get your logo on our README on Github with a link to your
site. [[Become a sponsor](https://opencollective.com/flycheck#sponsor)]

<a href="https://opencollective.com/flycheck/sponsor/0/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/0/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/1/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/1/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/2/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/2/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/3/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/3/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/4/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/4/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/5/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/5/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/6/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/6/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/7/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/7/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/8/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/8/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/9/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/9/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/10/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/10/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/11/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/11/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/12/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/12/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/13/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/13/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/14/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/14/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/15/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/15/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/16/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/16/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/17/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/17/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/18/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/18/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/19/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/19/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/20/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/20/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/21/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/21/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/22/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/22/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/23/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/23/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/24/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/24/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/25/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/25/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/26/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/26/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/27/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/27/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/28/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/28/avatar.svg"></a>
<a href="https://opencollective.com/flycheck/sponsor/29/website" target="_blank"><img src="https://opencollective.com/flycheck/sponsor/29/avatar.svg"></a>

## Code of Conduct

We strive to create a safe, friendly and welcoming environment in the Flycheck
community and have a [Code of Conduct][coc] that defines acceptable and welcome
behaviour as well as sanctions for violations.  All contributors and all
participants are expected to follow it, on Github, Gitter, Emacs.SX or any other
place that’s part of Flycheck’s broader community.

## License

Flycheck is free software: you can redistribute it and/or modify it under the
terms of the [GNU General Public License][copying] as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Flycheck is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the [GNU General Public License][copying] for more
details.

[COPYING]: https://github.com/flycheck/flycheck/blob/master/COPYING
[logo]: https://raw.githubusercontent.com/flycheck/flycheck/master/doc/_static/logo.png
[try it]: https://www.flycheck.org/en/latest/#try-out
[Installation]: https://www.flycheck.org/en/latest/user/installation.html
[Quickstart]: https://www.flycheck.org/en/latest/user/quickstart.html
[sx]: https://emacs.stackexchange.com/questions/tagged/flycheck
[gitter]: https://gitter.im/flycheck/flycheck
[Issue Tracker]: https://github.com/flycheck/flycheck/issues
[contrib]: https://www.flycheck.org/en/latest/contributor/contributing.html
[coc]: https://www.flycheck.org/en/latest/community/conduct.html
