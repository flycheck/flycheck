# -*- mode: yaml; -*-

# Syntax checkers for PHP

# Provide a repository for the latest PHP releases, and install the PEAR package
# manager
php5-repo:
  pkgrepo.managed:
    - ppa: ondrej/php5
    - require:
        - pkg: python-software-properties
    - require_in:
        - pkg: php-pear
        - pkg: php

php-pear:
  pkg.installed: []
  cmd.wait:
    - name: pear update-channels
    - watch:
        - pkg: php-pear

php:
  pkg.installed:
    - name: php5-cli

php-phpcs:
  cmd.run:
    - name: pear -q install PHP_CodeSniffer
    - unless: phpcs --version
    - require:
        - cmd: php-pear
