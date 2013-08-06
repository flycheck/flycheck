# Install all syntax checkers

class flycheck::checkers {

  include flycheck::checkers::erlang
  include flycheck::checkers::go
  include flycheck::checkers::haskell
  include flycheck::checkers::nodejs
  include flycheck::checkers::php
  include flycheck::checkers::python
  include flycheck::checkers::ruby

  # Various other syntax checkers

  apt::ppa { 'ppa:kevincantu/rust': }

  package { 'rust':
    ensure  => present,
    require => Apt::Ppa['ppa:kevincantu/rust']
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
                'zsh',             # zsh
                ]
  package { $packages: ensure => latest }
}
