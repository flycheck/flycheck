# Class: flycheck::checkers::php
#
# Installs syntax checkers from PHP package managers
class flycheck::checkers::php {

  require flycheck::php

  $php_packages = [ 'pear.phpmd.org/PHP_PMD', # php-phpmd
                    'PHP_CodeSniffer',        # php-phpcs
                    ]

  package { $php_packages:
    ensure   => latest,
    provider => pear,
    require  => Exec['php::pear::auto_discover'],
  }
}
