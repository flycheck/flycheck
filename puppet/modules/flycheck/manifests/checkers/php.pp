# Install PHP and PHP syntax checkers

class flycheck::checkers::php {
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

  Class['php::pear'] -> Package<| provider==pear |>

  package { 'PHP_CodeSniffer': # php-phpcs
    ensure   => latest,
    provider => pear,
    require  => Exec['php::pear::auto_discover'],
  }

  package{ 'pear.phpmd.org/PHP_PMD':
    ensure   => latest,
    provider => pear,
    require  => Exec['php::pear::auto_discover'],
  }
}
