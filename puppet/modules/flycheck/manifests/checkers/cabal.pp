# Class: flycheck::checkers::cabal
#
# Install syntax checkers from Cabal
class flycheck::checkers::cabal {

  $haskell_packages = ['shellcheck']
  flycheck::haskell::cabal { $haskell_packages: }
}
