# Install PHP and PHP syntax checkers

class flycheck::checkers::php {
  apt::ppa { 'ppa:ondrej/php5': }

  package { 'php5-cli':
    ensure  => latest,
    require => Apt::Ppa['ppa:ondrej/php5']
  }

  package { 'php-pear':
    ensure  => latest,
    require => Package['php5-cli'],
    notify  => Exec['pear update']
  }

  exec { 'pear update':
    command     => '/usr/bin/pear update-channels',
    refreshonly => true,
    require     => Package['php-pear'],
  }

  package { 'PHP_CodeSniffer': # php-phpcs
    ensure   => latest,
    provider => pear,
    require  => Exec['pear update'],
  }
}
