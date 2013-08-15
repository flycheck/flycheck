# Node.js and Javascript checkers

class flycheck::checkers::nodejs {

  # Install Node.js.  We do not use the nodejs class, because it's broken on
  # Precise.  It doesn't enable the PPA, and attempts to install NPM.
  apt::ppa { 'ppa:chris-lea/node.js': }

  package { 'nodejs':
    ensure  => installed,
    require => Apt::Ppa['ppa:chris-lea/node.js'],
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
    require  => Package['nodejs'],
  }
}
