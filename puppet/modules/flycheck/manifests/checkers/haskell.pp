# Provide Haskell syntax checkers

class flycheck::checkers::haskell {

  package { ['ghc', 'hlint']: ensure => latest }

  package { 'cabal-install':
    ensure  => latest,
    require => Package['ghc'],
  }

  exec { 'flycheck::checkers::haskell::cabal-update':
    command     => 'cabal update',
    environment => 'HOME=/root',
    unless      => 'cabal info hdevtools',
    path        => ['/usr/bin'],
    require     => Package['cabal-install']
  }

  exec { 'hdevtools':
    command     => 'cabal install --global hdevtools',
    creates     => '/usr/local/bin/hdevtools',
    environment => 'HOME=/root',
    require     => Exec['flycheck::checkers::haskell::cabal-update'],
    path        => ['/usr/bin']
  }
}
