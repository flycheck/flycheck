# Class: flycheck::checkers::pip
#
# Install syntax checkers from Pip
class flycheck::checkers::pip {
  require flycheck::python

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
  }

  package { 'closure-linter':
    ensure   => installed,
    provider => pip,
    source   => 'http://closure-linter.googlecode.com/files/closure_linter-latest.tar.gz',
  }
}
