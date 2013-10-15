# Syntax checkers for Python

class flycheck::checkers::python {
  package { 'python': ensure => latest}

  package { 'python-setuptools' :
    ensure  => latest,
    notify  => Exec['flycheck::checkers::python::bootstrap-pip'],
    require => Package['python'],
  }

  exec { 'flycheck::checkers::python::bootstrap-pip':
    command     => '/usr/bin/easy_install -U setuptools pip',
    refreshonly => true,
    require     => Package['python-setuptools'],
  }

  $python_packages = ['setuptools',
                      'pip',
                      'flake8',      # python-flake8
                      'pep8-naming', # Plugin for flake8
                      'pylint',      # python-pylint
                      'docutils',    # rst
                      ]

  package { $python_packages:
    ensure   => latest,
    provider => pip,
    require  => Exec['flycheck::checkers::python::bootstrap-pip'],
  }
}
