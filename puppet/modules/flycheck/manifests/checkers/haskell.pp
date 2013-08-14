# Provide Haskell syntax checkers

class flycheck::checkers::haskell {

  package { ['ghc', 'hlint']: ensure => latest }

  package { ['cabal-install']:
    ensure  => latest,
    require => Package['ghc'],
    notify  => Exec['cabal update'],
  }

  exec { 'cabal update':
    command     => '/usr/bin/cabal update',
    refreshonly => true,
    environment => 'HOME=/root',
    require     => Package['cabal-install']
  }

  exec { 'hdevtools':
    command     => '/usr/bin/cabal install --global hdevtools',
    creates     => '/usr/local/bin/hdevtools',
    environment => 'HOME=/root',
    require     => Exec['cabal update'],
  }
}
