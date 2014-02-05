# Class: flycheck::haskell
#
# Install a Haskell environment
class flycheck::haskell {
  package { ['haskell-platform']: ensure => latest }
}
