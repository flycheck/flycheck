# Class: flycheck::sphinx
#
# Install Sphinx and related packages to build documentation
class flycheck::sphinx {
  require flycheck::python

  package { [ 'Sphinx',
              'Pillow'           # For image support
              ]:
    ensure   => latest,
    provider => pip,
  }

  # For Texinfo building
  package { ['texinfo', 'install-info']: ensure => latest }
}
