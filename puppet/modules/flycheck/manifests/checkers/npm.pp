# Class: flycheck::checkers::node
#
# Installs syntax checkers from NPM
class flycheck::checkers::npm {
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
}
