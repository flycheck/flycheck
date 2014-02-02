# Class: flycheck::checkers
#
# Install all syntax checkers
class flycheck::checkers {

  include flycheck::php
  include flycheck::python
  include flycheck::ruby

  include flycheck::checkers::repositories

  # Nodejs
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

  include perl

  $perl_packages = ['Perl::Critic' # perl-perlcritic
                    ]

  perl::module { $perl_packages: }

  $php_packages = [ 'pear.phpmd.org/PHP_PMD', # php-phpmd
                    'PHP_CodeSniffer',        # php-phpcs
                    ]

  package { $php_packages:
    ensure   => latest,
    provider => pear,
    require  => [Class['flycheck::php'], Exec['php::pear::auto_discover']],
  }

  $python_packages = ['flake8',      # python-flake8
                      'pep8-naming', # Plugin for flake8
                      'pylint',      # python-pylint
                      'docutils',    # rst
                      ]

  package { $python_packages:
    ensure   => latest,
    provider => pip,
    # We must have pip available
    require  => Class['flycheck::python'],
  }

  $gem_packages = [ 'erubis',      # eruby-erubis
                    'foodcritic',  # chef-foodcritic
                    'haml',
                    'puppet-lint',
                    'rubocop',     # ruby-rubocop
                    'sass',        # sass/scss
                    'slim',        # slim
                    'ruby-lint',
                    ]

  package { $gem_packages:
    ensure   => latest,
    provider => gem,
    # We need Ruby and Rubygems
    require  => Class['flycheck::ruby'],
  }

  package { 'closure-linter':
    ensure   => installed,
    provider => pip,
    source   => 'http://closure-linter.googlecode.com/files/closure_linter-latest.tar.gz',
    require  => Class['flycheck::python'],
  }

  $apt_packages = [ 'asciidoc',           # asciidoc
                    'bash',               # bash/sh-bash
                    'clang',              # c/c++-clang
                    'cppcheck',           # c/c++-cpppcheck
                    'cfengine-community', # cfengine
                    'esl-erlang',         # erlang
                    'golang-stable',      # go-*
                    'ghc',                # haskell-ghc
                    'hlint',              # haskell-lint
                    'tidy',               # html-tidy
                    'lua5.2',             # lua
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
    require => [Class['flycheck::checkers::repositories'], Class['apt::update']]
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

  # elixir
  $elixir_version = '0.12.0'

  archive { "elixir-${elixir_version}":
    ensure        => present,
    url           => "https://github.com/elixir-lang/elixir/releases/download/v${elixir_version}/v${elixir_version}.zip",
    extension     => 'zip',
    digest_string => '62fc9173158ba919b2d0f792b827eca7',
    target        => "/opt/elixir-${elixir_version}",
    root_dir      => '.',
    require       => Package['esl-erlang'],
  }

  file { '/usr/local/bin/elixirc':
    ensure  => link,
    target  => "/opt/elixir-${elixir_version}/bin/elixirc",
    require => Archive["elixir-${elixir_version}"],
  }
}
