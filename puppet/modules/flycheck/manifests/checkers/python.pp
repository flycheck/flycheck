# Syntax checkers for Python

class flycheck::checkers::python {
  package { ['python', 'python-setuptools'] : ensure => latest }

  # Bootstrap a decent Pip release
  $pip = 'pip-1.4.1'
  $pip_archive = "${pip}.tar.gz"

  archive { $pip_archive:
    ensure        => present,
    url           => "https://pypi.python.org/packages/source/p/pip/${pip_archive}",
    digest_string => '6afbb46aeb48abac658d4df742bff714',
    target        => '/usr/src',
  }

  exec { 'flycheck::checkers::python::bootstrap-pip':
    command => '/usr/bin/python setup.py install',
    cwd     => "/usr/src/${pip}",
    creates => '/usr/local/bin/pip',
    require => [Package['python-setuptools'], Archive[$pip_archive]],
  }

  # Now upgrade to the latest version of setuptools and pip
  package { 'setuptools':
    ensure   => latest,
    provider => pip,
    require  => Exec['flycheck::checkers::python::bootstrap-pip'],
  }

  package { 'pip':
    ensure   => latest,
    provider => pip,
    require  => Package['setuptools'],
  }

  $python_packages = ['flake8',      # python-flake8
                      'pep8-naming', # Plugin for flake8
                      'pylint',      # python-pylint
                      'docutils',    # rst
                      ]

  package { $python_packages:
    ensure   => latest,
    provider => pip,
    require  => Package['pip'],
  }
}
