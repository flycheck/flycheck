# Install all syntax checkers

class flycheck::checkers {

  include flycheck::checkers::erlang
  include flycheck::checkers::go
  include flycheck::checkers::haskell
  include flycheck::checkers::php
  include flycheck::checkers::python
  include flycheck::checkers::ruby

  # Various other syntax checkers

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
                    'jshint',        # javascript-jshint
                    'jsonlint',      # json-jsonlint
                    'less',          # less
                    ]
  package { $node_packages:
    ensure   => present,
    provider => npm,
    require  => Class['nodejs'],
  }

  $packages = [ 'bash',            # bash/sh-bash
                'clang',           # c/c++-clang
                'cppcheck',        # c/c++-cpppcheck
                'tidy',            # html-tidy
                'lua5.2',          # lua
                'perl',            # perl
                'puppet',          # puppet-parser
                'scala',           # scala
                'dash',            # sh-dash
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
