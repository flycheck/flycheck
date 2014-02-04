# Class: flycheck::haskell
#
# Install a Haskell environment
class flycheck::haskell {
  package { ['haskell-platform']: ensure => latest }

  exec { 'cabal update':
    environment => 'HOME=/root',
    path        => ['/usr/local/bin', '/usr/bin', '/bin'],
    require     => Package['haskell-platform'],
  }
}
