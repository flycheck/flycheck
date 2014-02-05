# Definition: flycheck::haskell::cabal
#
# This class installs Cabal packages.
#
# Parameters:
# - The $package
define flycheck::haskell::cabal($package = $title) {

  include flycheck::haskell::update

  exec { "cabal install --global ${package}":
    command     => "cabal install --global ${package}",
    path        => ['/usr/local/bin', '/usr/bin', '/bin'],
    environment => 'HOME=/root',
    timeout     => 900,
    unless      => "ghc-pkg list --global | grep -E '${package}-[[:digit:]]+(\\.[[:digit:]]+)*$'",
    require     => Class['flycheck::haskell::update'],
  }
}
