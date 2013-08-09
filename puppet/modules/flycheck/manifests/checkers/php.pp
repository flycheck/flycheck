# Install PHP and PHP syntax checkers

class flycheck::checkers::php {
  apt::ppa { 'ppa:ondrej/php5': }

  package { 'php5-cli':
    ensure  => present,
    require => Apt::Ppa['ppa:ondrej/php5']
  }

  package { 'php-pear':
    ensure  => present,
    require => Package['php5-cli'],
    notify  => Exec['pear update']
  }

  exec { 'pear update':
    command     => '/usr/bin/pear update-channels',
    refreshonly => true,
    require     => Package['php-pear'],
  }

  package { 'PHP_CodeSniffer': # php-phpcs
    ensure   => present,
    provider => pear,
    require  => Exec['pear update'],
  }
}
