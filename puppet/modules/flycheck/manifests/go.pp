# Class: flycheck::go
#
# Install Go
class flycheck::go {
  apt::ppa { 'ppa:juju/golang': }

  package { 'golang':
    ensure  => latest,
    require => [Class['apt::update'], Apt::Ppa['ppa:juju/golang']],
  }

  # Go path for global packages
  $gopath = '/usr/local/lib/go'

  file { $gopath:
    ensure => directory,
  }
}
