# Class: flycheck::checkers::pip
#
# Install syntax checkers from Pip
class flycheck::checkers::pip {
  include flycheck::python

  # Sphinx also comes from Pip
  include flycheck::sphinx

  $python_packages = ['flake8',      # python-flake8
                      'pep8-naming', # Plugin for flake8
                      'pylint',      # python-pylint
                      'docutils',    # rst
                      ]

  package { $python_packages:
    ensure   => latest,
    provider => pip,
    # We must have pip available
    require  => Class['flycheck::python'],
  }

  package { 'closure-linter':
    ensure   => installed,
    provider => pip,
    source   => 'http://closure-linter.googlecode.com/files/closure_linter-latest.tar.gz',
    require  => Class['flycheck::python'],
  }
}
