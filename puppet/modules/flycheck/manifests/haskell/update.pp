# Class: flycheck::haskell::update
#
# Updates the Cabal package list
class flycheck::haskell::update {
  include flycheck::haskell

  exec { 'cabal update':
    environment => 'HOME=/root',
    path        => ['/usr/local/bin', '/usr/bin', '/bin'],
    creates     => '/root/.cabal/packages/hackage.haskell.org/00-index.tar.gz',
    require     => Class['flycheck::haskell']
  }
}
