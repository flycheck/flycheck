# Class: flycheck::checkers::go
#
# Install checkers with the Go package system.
class flycheck::checkers::go {

  include flycheck::go
  include flycheck::git         # To install packages from Github

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

}
