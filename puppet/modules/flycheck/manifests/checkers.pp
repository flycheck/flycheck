# Class: flycheck::checkers
#
# Install all syntax checkers
class flycheck::checkers {

  include flycheck::python
  include flycheck::ruby

  include flycheck::checkers::erlang
  include flycheck::checkers::go
  include flycheck::checkers::php

  # Do not install Gems on Travis
  if $::travis {
    notice('Skipping Ruby Gems on Travis CI')
  }
  else {
    include flycheck::checkers::gems
  }

  # Various other syntax checkers
  Package {
    require => Class['apt::update'],
  }

  # Cfengine
  apt::source { 'cfengine-community':
    location    => 'http://cfengine.com/pub/apt',
    repos       => 'main',
    key         => '89107B44',
    key_source  => 'http://cfengine.com/pub/gpg.key',
    include_src => false,
  }

  package { 'cfengine-community':
    ensure  => latest,
    require => Apt::Source['cfengine-community'],
  }

  # This PPA provides Clang 3.2 for Ubuntu 12.04
  apt::ppa { 'ppa:kxstudio-team/builds': }

  package { 'clang':
    ensure  => latest,
    require => Apt::Ppa['ppa:kxstudio-team/builds'],
  }

  apt::ppa { 'ppa:plt/racket': }

  package { 'racket':
    ensure  => latest,
    require => Apt::Ppa['ppa:plt/racket'],
  }

  apt::ppa { 'ppa:kevincantu/rust': }

  package { 'rust':
    ensure  => latest,
    require => Apt::Ppa['ppa:kevincantu/rust']
  }

  class { 'nodejs':
    manage_repo => true,
    version     => latest
  }

  $node_packages = ['coffee-script', # coffee
                    'coffeelint',    # coffee-coffeelint
                    'csslint',       # css-csslint
                    'eslint',        # javascript-eslint
                    'handlebars',    # handlebars
                    'jshint',        # javascript-jshint
                    'jsonlint',      # json-jsonlint
                    'js-yaml',       # yaml-jsyaml
                    'less',          # less
                    ]
  package { $node_packages:
    # We can't use latest here, thanks to
    # https://github.com/puppetlabs/puppetlabs-nodejs/issues/43
    ensure   => present,
    provider => npm,
    require  => Class['nodejs'],
  }

  $python_packages = ['flake8',      # python-flake8
                      'pep8-naming', # Plugin for flake8
                      'pylint',      # python-pylint
                      'docutils',    # rst
                      'sphinx',      # rst-sphinx
                      ]

  package { $python_packages:
    ensure   => latest,
    provider => pip,
    # We must have pip available
    require  => Class['flycheck::python'],
  }

  package { 'closure-linter':
    ensure   => installed,
    provider => pip,
    source   => 'http://closure-linter.googlecode.com/files/closure_linter-latest.tar.gz',
    require  => Class['flycheck::python'],
  }

  $packages = [ 'asciidoc',        # asciidoc
                'bash',            # bash/sh-bash
                'cppcheck',        # c/c++-cpppcheck
                'ghc',             # haskell-ghc
                'hlint',           # haskell-lint
                'tidy',            # html-tidy
                'lua5.2',          # lua
                'perl',            # perl
                'puppet',          # puppet-parser
                'scala',           # scala
                'dash',            # sh-dash
                'verilator',       # verilog-verilator
                'chktex',          # tex-chktex
                'lacheck',         # tex-lacheck
                'xmlstarlet',      # xml-xmlstarlet
                'libxml2-utils',   # xml-xmllint
                'zsh',             # zsh
                ]
  package { $packages: ensure => latest }

  $dmd_version = '2.063.2'
  $dmd_deb     = "dmd_${dmd_version}-0_amd64.deb"

  archive::download { $dmd_deb:
    url           => "http://downloads.dlang.org/releases/2013/${dmd_deb}",
    digest_string => 'fa2c04994df432156903fc66a4c73727',
  }

  # DMD dependencies need to be installed explicitly, as DPKG does not resolve
  # them
  package { ['gcc-multilib', 'xdg-utils']:
    ensure => latest,
  }

  package { 'dmd':
    ensure   => present,
    provider => dpkg,
    source   => "/usr/src/${dmd_deb}",
    require  => [ Archive::Download[$dmd_deb],
                  Package['gcc-multilib'], Package['xdg-utils'] ]
  }
}
