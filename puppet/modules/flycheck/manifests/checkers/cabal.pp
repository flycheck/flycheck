# Class: flycheck::checkers::cabal
#
# Install syntax checkers from Cabal
class flycheck::checkers::cabal {

  require flycheck::haskell

  $haskell_packages = ['shellcheck']
  flycheck::haskell::cabal { $haskell_packages: }
}
