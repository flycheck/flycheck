=========
 Credits
=========

Flycheck is written and maintained by `Sebastian Wiesner`_, with contributions
from the following people.  Their help and ideas, their support and patches are
greatly appreciated.

- `Atila Neves`_ added support for include paths of DMD.
- `Bozhidar Batsov`_ provided valuable feedback and refinements, brought
  Flycheck to a larger user base by adding it to his Prelude_ project, and added
  a Ruby syntax checker using `rubocop`.
- `Damon Haley`_ helped to shape and test the PHP CodeSniffer checker.
- `David Holm`_ added C/C++ syntax and style checkers using `clang` and
  `cppcheck` respectively, and contributed a Verilog syntax checker using
  `verilator`.
- `Derek Harland`_ improved Rust syntax checking by adding support for info
  messages, and for crates with `flycheck-rust-crate-root`
- `Doug MacEachern`_ added a Go syntax checker using `errcheck`.
- `Fanael Linithien`_ greatly improved C/C++ support in Flycheck, by adding
  GCC-based syntax checker, and extending the C/C++ syntax checkers with new
  features and options.
- `Gereon Frey`_ fixed the `go-build` syntax checker and improved its tests.
- `Gulshan Singh`_ added `flycheck-rust-crate-type`
- `Jimmy Yuen Ho Wong`_ added the HTML syntax checker and the jshint Javascript
  checker, and did valuable testing and bug fixing.
- `Krzysztof Witkowski`_ implemented `eval` support in Flycheck commands.
- `Lee Adams`_ added a Scala style checker using scalastyle.
- `Magnar Sveen`_ developed the awesome `dash.el`_ and `s.el`_
  libraries, that drive considerable parts of Flycheck's internals.
- `Malyshev Artem`_ made Flycheck's keybindings customizable with
  `flycheck-keymap-prefix`.
- `Marian Schubert`_ added the Perl syntax checker.
- `Mark Hellewell`_ added the Puppet syntax and style checkers.
- `Martin Grenfell`_ created the awesome Vim library syntastic_ which inspired
  this project and many of its checkers.
- `Matthias Dahl`_ improved the performance of Flycheck's temp file handling.
- `Matthias Güdemann`_ contributed an initial version of the Flycheck logo, and
  provided very valuable input, ideas and feedback during the design of official
  logo.
- `Mitch Tishmack`_ added support for Microsoft extensions to the Clang syntax
  checker.
- `Per Nordlöw`_ contributed a syntax checker for Fortran using GFortran, and
  helped to shape an Ada syntax checker with GNAT.
- `Peter Vasil`_ contributed syntax checkers for XML, Lua and Go (using `go
  build` and `go test`), added unit tests and did valuable testing.
- `Robert Dallas Gray`_ made error display customizable with
  `flycheck-display-errors-function`.
- `Robert Zaremba`_ added Go syntax checker using `gofmt`.
- `Simon Carter`_ provided valuable feedback about automatic
  syntax checking, and took great effort to debug some very subtle issues in it.
- steckerhalter_ provided the PHP CodeSniffer checker.
- `Steve Purcell`_ implemented many checkers, contributed important ideas to the
  design of the checker API and engaged in worthwhile discussion to shape this
  project.
- `Sylvain Benner`_ added syntax checkers for Elixir and Erlang, and wrote the
  cool flycheck-color-mode-line_ extension.
- `Sylvain Rousseau`_ added a syntax checker for POSIX shell script using
  `bash`, and improved error parsing in the Bash script syntax checker.
- `Ted Zlatanov`_ added a syntax checker for CFEngine.
- `Tom Jakubowski`_ added `flycheck-rust-check-tests` to the Rust syntax
  checker.
- `tom tan`_ added a syntax checker for the D programming language using `dmd`,
  and wrote the cool flycheck-d-unittest_ extension.
- `Yannick Roehlly`_ added support for PEP8 naming errors to the Flake8 syntax checker.
- `Yasuyuki Oka`_ contributed a lot of syntax checkers for various languages,
  including AsciiDoc, Chef recipes, ERuby, GNU Make, Handlebars, Javascript,
  PHP, Racket, Ruby, Slim and YAML.
- `Victor Deryagin`_ added the Rust syntax checker.

Of course we also need to thank `GNU Flymake`_, the first, respectable, though
somewhat failed attempt at on-the-fly syntax checking.

.. _Prelude: https://github.com/bbatsov/prelude
.. _dash.el: https://github.com/magnars/dash.el
.. _s.el: https://github.com/magnars/s.el
.. _syntastic: https://github.com/scrooloose/syntastic
.. _flycheck-color-mode-line: https://github.com/flycheck/flycheck-color-mode-line
.. _flycheck-d-unittest: https://github.com/flycheck/flycheck-d-unittest
.. _GNU Flymake: http://www.gnu.org/software/emacs/manual/html_node/flymake/

.. _Atila Neves: https://github.com/atilaneves
.. _Bozhidar Batsov: https://github.com/bbatsov
.. _Damon Haley: https://github.com/dhaley
.. _David Holm: https://github.com/dholm
.. _Derek Harland: https://github.com/donkopotamus
.. _Doug MacEachern: https://github.com/dougm
.. _Fanael Linithien: https://github.com/Fanael
.. _Gereon Frey: https://github.com/gfrey
.. _Gulshan Singh: https://github.com/gsingh93
.. _Jimmy Yuen Ho Wong: https://github.com/wyuenho
.. _Krzysztof Witkowski: https://github.com/kwitek
.. _Lee Adams: https://github.com/leeaustinadams
.. _Magnar Sveen: https://github.com/magnars
.. _Malyshev Artem: https://github.com/proofit404
.. _Marian Schubert: https://github.com/maio
.. _Mark Hellewell: https://github.com/markhellewell
.. _Martin Grenfell: https://github.com/scrooloose
.. _Matthias Dahl: https://github.com/BinaryKhaos
.. _Matthias Güdemann: https://github.com/mgudemann
.. _Mitch Tishmack: https://github.com/mitchty
.. _Per Nordlöw: https://github.com/nordlow
.. _Peter Vasil: https://github.com/ptrv
.. _Robert Dallas Gray: https://github.com/rdallasgray
.. _Robert Zaremba: https://github.com/robert-zaremba
.. _Sebastian Wiesner: https://github.com/lunaryorn
.. _Simon Carter: https://github.com/bbbscarter
.. _steckerhalter: https://github.com/steckerhalter
.. _Steve Purcell: https://github.com/purcell
.. _Sylvain Benner: https://github.com/syl20bnr
.. _Sylvain Rousseau: https://github.com/thisirs
.. _Ted Zlatanov: https://github.com/tzz
.. _Tom Jakubowski: https://github.com/tomjakubowski
.. _tom tan: https://github.com/tom-tan
.. _Victor Deryagin: https://github.com/vderyagin
.. _Yannick Roehlly: https://github.com/yannick1974
.. _Yasuyuki Oka: https://github.com/yasuyk

.. Local Variables:
.. sort-fold-case: t
.. End:
