# Class: flycheck::checkers::gems
#
# Installs syntax checkers from Ruby Gems
class flycheck::checkers::gems {

  include flycheck::ruby

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
}
