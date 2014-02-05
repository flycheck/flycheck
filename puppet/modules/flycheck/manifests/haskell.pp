# Class: flycheck::haskell
#
# Install a Haskell environment
class flycheck::haskell {
  package { ['haskell-platform']: ensure => latest }

  exec { 'cabal update':
    environment => 'HOME=/root',
    path        => ['/usr/local/bin', '/usr/bin', '/bin'],
    creates     => '/root/.cabal/packages/hackage.haskell.org/00-index.tar.gz',
    require     => Package['haskell-platform'],
  }
}
