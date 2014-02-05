# Class: flycheck::checkers::go
#
# Install checkers with the Go package system.
class flycheck::checkers::go {

  include flycheck::go
  include flycheck::git         # To install packages from Github
  include flycheck::hg          # To install packages from Google Code

  exec { 'golint':
    command     => 'go get github.com/golang/lint/golint',
    path        => ['/usr/local/bin', '/usr/bin', '/bin'],
    environment => ["GOPATH=${flycheck::go::gopath}"],
    require     => [Class['flycheck::go', 'flycheck::git']],
  }

  file { '/usr/local/bin/golint':
    ensure  => link,
    target  => "${flycheck::go::gopath}/bin/golint",
    require => Exec['golint']
  }

  exec { 'govet':
    command     => 'go get code.google.com/p/go.tools/cmd/vet',
    path        => ['/usr/local/bin', '/usr/bin', '/bin'],
    environment => ["GOPATH=${flycheck::go::gopath}"],
    require     => [Class['flycheck::go', 'flycheck::hg']],
  }
}
