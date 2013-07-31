# -*- mode: yaml; -*-

# Syntax checkers for PHP
include:
  - base.tools

# Install the Pear package manager
php-pear:
  pkg.installed:
    - require:
        - pkg: php
  cmd.wait:
    - name: pear update-channels
    - watch:
        - pkg: php-pear

php:
  # Use a PPA for the latest PHP release
  pkg.installed:
    - name: php5-cli
  pkgrepo.managed:
    - ppa: ondrej/php5
    - require:
        - pkg: ppa
    - require_in:
        - pkg: php-pear
        - pkg: php

php-phpcs:
  cmd.run:
    - name: pear -q install PHP_CodeSniffer
    - unless: phpcs --version
    - require:
        - cmd: php-pear
