# The main manifest for test provisioning

include flycheck::base
include flycheck::emacs

# Do not install any checker tools on Travis
if !$::travis {
  include flycheck::sphinx

  include flycheck::checkers::cabal
  include flycheck::checkers::gems
  include flycheck::checkers::go
  include flycheck::checkers::misc
  include flycheck::checkers::npm
  include flycheck::checkers::packages
  include flycheck::checkers::perl
  include flycheck::checkers::php
  include flycheck::checkers::pip
}
else {
  notice('Skipping Sphinx and syntax checker tools on Travis CI')
}
