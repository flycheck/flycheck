# Class: flycheck::python
#
# Install Python and bootstrap Pip
class flycheck::python {
  require apt::update

  package { ['python', 'python-dev']:
    ensure => latest
  }

  package { 'python-setuptools' :
    ensure  => latest,
    notify  => Exec['flycheck::python::bootstrap-pip'],
    require => Package['python'],
  }

  exec { 'flycheck::python::bootstrap-pip':
    command     => '/usr/bin/easy_install -U setuptools pip',
    refreshonly => true,
    require     => Package['python-setuptools'],
  }
}
