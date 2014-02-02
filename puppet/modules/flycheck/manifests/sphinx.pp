# Class: flycheck::sphinx
#
# Install Sphinx and related packages to build documentation
class flycheck::sphinx {
  include flycheck::python

  package { [ 'Sphinx',
              'Pillow'           # For image support
              ]:
    ensure   => latest,
    provider => pip,
    require  => Class['flycheck::python']
  }

  # For Texinfo building
  package { ['texinfo', 'install-info']: ensure => latest }
}
