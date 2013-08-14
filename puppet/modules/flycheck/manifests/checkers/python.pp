# Syntax checkers for Python

class flycheck::checkers::python {
  package { ['python', 'python-pip']: ensure => latest }

  # Get the latest distribute and pip
  package { 'setuptools':
    ensure   => latest,
    provider => pip,
    require  => Package['python-pip'],
  }

  $python_packages = ['flake8',      # python-flake8
                      'pep8-naming', # Plugin for flake8
                      'pylint',      # python-pylint
                      'docutils',    # rst
                      ]

  package { $python_packages:
    ensure   => latest,
    provider => pip,
    require  => [Package['setuptools'], Package['python-pip']],
  }
}
