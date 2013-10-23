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

 if ! defined(Exec['php-pear-discover-phpmd']) {
    exec { 'php-pear-discover-phpmd':
      command => 'pear channel-discover pear.phpmd.org',
      require => Package['php-pear'],
      path    => ['/usr/bin', '/bin'],
    }
  }

 if ! defined(Exec['php-pear-discover-pdepend']) {
    exec { 'php-pear-discover-pdepend':
      command => 'pear channel-discover pear.pdepend.org',
      require => Package['php-pear'],
      path    => ['/usr/bin', '/bin'],
    }
  }

  package { 'phpmd/PHP_PMD': # php-phpmd
    ensure   => latest,
    provider => pear,
    install_options => ['--alldeps'],
    require  => Exec['php-pear-discover-phpmd', 'php-pear-discover-pdepend']
  }
}
