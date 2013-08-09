# Syntax checkers for Python

class flycheck::checkers::python {
  package { ['python', 'python-pip']: ensure => latest }

  $python_packages = ['flake8',      # python-flake8
                      'pep8-naming', # Plugin for flake8
                      'pylint',      # python-pylint
                      'docutils',    # rst
                      ]

  package { $python_packages:
    ensure   => present,
    provider => pip,
    require  => Package['python-pip'],
  }
}
