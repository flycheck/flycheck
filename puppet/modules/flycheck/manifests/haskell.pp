# Class: flycheck::haskell
#
# Install a Haskell environment
class flycheck::haskell {
  require apt::update

  package { 'haskell-platform':
    ensure  => latest,
  }
}
