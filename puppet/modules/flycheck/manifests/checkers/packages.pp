# Class: flycheck::checkers::packages
#
# Install syntax checkers from system packages
class flycheck::checkers::packages {

  require flycheck::checkers::repositories
  require apt::update

  # These language environments also provide syntax checkers
  include flycheck::erlang
  include flycheck::ruby
  include flycheck::php
  include flycheck::haskell
  include flycheck::go

  $apt_packages = [ 'asciidoc',           # asciidoc
                    'bash',               # bash/sh-bash
                    'clang',              # c/c++-clang
                    'cppcheck',           # c/c++-cpppcheck
                    'cfengine-community', # cfengine
                    'hlint',              # haskell-lint
                    'tidy',               # html-tidy
                    'lua5.2',             # lua
                    'pmake',              # make (NetBSD Make)
                    'puppet',             # puppet-parser
                    'racket',             # racket
                    'rust-0.9',           # rust
                    'scala',              # scala
                    'dash',               # sh-dash
                    'verilator',          # verilog-verilator
                    'chktex',             # tex-chktex
                    'lacheck',            # tex-lacheck
                    'xmlstarlet',         # xml-xmlstarlet
                    'libxml2-utils',      # xml-xmllint
                    'zsh',                # zsh
                    ]

  package { $apt_packages:
    ensure  => latest,
  }

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
