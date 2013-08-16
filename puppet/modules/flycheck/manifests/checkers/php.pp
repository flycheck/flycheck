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
  }

  package { 'PHP_CodeSniffer': # php-phpcs
    ensure   => latest,
    provider => pear,
    require  => Package['php-pear'],
  }
}
