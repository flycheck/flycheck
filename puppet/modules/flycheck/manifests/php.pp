# Class: flycheck::php
#
# Install PHP and Pear
class flycheck::php {
  include php::params

  apt::ppa { 'ppa:ondrej/php5': }

  package { 'php5-cli':
    ensure  => latest,
    require => Apt::Ppa['ppa:ondrej/php5']
  }

  class { 'php::pear':
    ensure  => latest,
    require => Package['php5-cli'],
  }
}
