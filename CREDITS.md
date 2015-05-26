Credits
=======

The following people—listed in alphabetical order—contributed
substantial code to Flycheck:

- [Alain Kalker](https://github.com/ackalker) (fix quoting for flycheck-compile)
- [Atila Neves](https://github.com/atilaneves) (include paths for DMD)
- [Bozhidar Batsov](https://github.com/bbatsov) (improved Flycheck menu, RuboCop
  syntax checker, improvements to Ruby)
- [capdevc](https://github.com/capdevc) (enabled warnings in Rust)
- [Damon Haley](https://github.com/dhaley) (PHPCS syntax checker)
- [David Holm](https://github.com/dholm) (Clang, Cppcheck, lintr and Verilator
  syntax checkers)
- [Derek Harland](https://github.com/donkopotamus) (improved Crate support for
  Rust)
- [Doug MacEachern](https://github.com/dougm) (Go errcheck syntax checker)
- [Erik Hetzner](https://github.com/egh) (handlebars)
- [Fanael Linithien](https://github.com/Fanael) (GCC syntax checker, exception
  support for C/C++, inconclusive checks for cppcheck, literate Haskell support,
  bug fixes)
- [Gereon Frey](https://github.com/gfrey) (fixes to temporary file handling in
  Go and to the Go tests)
- [Gulshan Singh](https://github.com/gsingh93) (customizable Rust crate type)
- [Iain Beeston](https://github.com/iainbeeston) (SCSS-Lint syntax checker)
- [Jim Hester](https://github.com/jimhester) (lintr syntax checker)
- [Jimmy Yuen Ho Wong](https://github.com/wyuenho) (HTML tidy and JSHint syntax
  checkers, bug fixes to faces)
- [Krzysztof Witkowski](https://github.com/kwitek) (eval support)
- [Lee Adams](https://github.com/leeaustinadams) (scalastyle syntax checker)
- [Lorenzo Villani](https://github.com/lvillani) (`standard` syntax checker)
- [Łukasz Jędrzejewski](https://github.com/jedrz) (bug fixes for chktex syntax
  checker and error navigation)
- [Magnar Sveen](https://github.com/magnars) (customizable idle change delay)
- [Malyshev Artem](https://github.com/proofit404) (customizable keymap prefix)
- [Marian Schubert](https://github.com/maio) (Perl syntax checker)
- [Marcin Antczak](https://github.com/marcinant) (error highlighting in error
  list)
- [Marcus Majewski](https://github.com/hekto) (error columns in ESLint)
- [Mark Hellewell](https://github.com/markhellewell) (Puppet and puppetlint
  syntax checkers)
- [Matthias Dahl](https://github.com/BinaryKhaos) (performance improvements in
  temporary file handling)
- [Mitch Tishmack](https://github.com/mitchty) (Microsoft extensions in C/C++)
- [Moritz Bunkus](https://github.com/mbunkus) (Include path for Perl)
- [Omair Majid](https://github.com/omajid) (syntax checker for RPM spec)
- [papaeye](https://github.com/papaeye) (JSCS syntax checker)
- [Per Nordlöw](https://github.com/nordlow) (syntax checkers for Ada, Fortran
  and Python, syntax checker name in error list)
- [Peter Eisentraut](https://github.com/petere) (improvements to foodcritic)
- [Peter Vasil](https://github.com/ptrv) (Lua, Luacheck and Go build/test
  syntax checkers)
- [Robert Zaremba](https://github.com/robert-zaremba) (gofmt syntax checker)
- [Robert Dallas Gray](https://github.com/rdallasgray) (configurable error display)
- [Sean Gillespie](https://github.com/swgillespie) (help messages in Rust)
- [Sean Salmon](https://github.com/phatcabbage) (platform-independent directory
  handling)
- [Sebastian Wiesner](https://github.com/lunaryorn) (primary maintainer)
- [Syohei Yoshida](https://github.com/syohex) (customizable error display delay)
- [steckerhalter](https://github.com/steckerhalter) (PHPCS syntax checker)
- [Stephen Lewis](https://github.com/stephenjlewis) (pedantic flags for Clang
  and GCC)
- [Steve Purcell](https://github.com/purcell) (various bug fixes, syntax
  checkers for HAML, csslint, jslint, SASS, SCSS, jsonlint, LESS, GHC, hdevtools
  and hlint)
- [Sylvain Benner](https://github.com/syl20bnr) (Elixir and Erlang syntax
  checkers)
- [Sylvain Rousseau](https://github.com/thisirs) (Bash syntax checkers)
- [Ted Zlatanov](https://github.com/tzz) (CFEngine syntax checker, minimum level
  for error navigation)
- [Tom Jakubowski](https://github.com/tomjakubowski) (test support in Rust)
- [Tomoya Tanjo](https://github.com/tom-tan) (DMD syntax checker)
- [Victor Deryagin](https://github.com/vderyagin) (Rust syntax checker)
- [Yannick Roehlly](https://github.com/yannick1974) (PEP8 naming support in
  Python)
- [Yasuyuki Oka](https://github.com/yasuyk) (syntax checkers for phpmd, Slim,
  js-yaml, ruby-lint, Handlebars, Foodcritic, AsciiDoc, Racket, ESLint, erubis,
  POSIX Make, Golint and go vet)

The following people greatly supported Flycheck in other ways:

- [Simon Carter](https://github.com/bbbscarter) (valuable testing and feedback
  of automatic syntax checking)
- [Matthias Güdemann](https://github.com/mgudemann) (initial version of the
  Flycheck logo, valuable feedback for the final version)

The Flycheck developers would also like to thank the following people
and projects:

- [Bozhidar Batsov](https://github.com/bbatsov) for his valuable feedback and
  his constant support and endorsement of Flycheck from the very
  beginning. Notably, he added Flycheck to his popular
  [Prelude](https://github.com/bbatsov/prelude) project at a very early stage
  and thus brought Flycheck to many new users.
- [Magnar Sveen](https://github.com/magnars) for his
  [dash.el](https://github.com/magnars/dash.el) and
  [s.el](https://github.com/magnars/s.el) libraries, which support considerable
  parts of Flycheck internals, and greatly helped to overcome the author's
  initial aversion to Emacs Lisp.
- [Martin Grenfell](https://github.com/scrooloose) for the Vim syntax
  checking extension
  [Syntastic](https://github.com/scrooloose/syntastic), which saved
  the author's life back when he was using Vim, and served as
  inspiration for Flycheck and many of its syntax checkers.
- Pavel Kobyakov for his work on GNU Flymake, which is a great work on
  its own, despite its flaws and weaknesses
- [Steve Purcell](https://github.com/purcell) for his valuable feedback, the
  fruitful discussions and his important ideas about the shape and design of
  Flycheck, and his indispensible and dedicated work on MELPA, which drives the
  continuous distribution of Flycheck to its users.
