# Class: flycheck::checkers::gems
#
# Installs syntax checkers from Ruby Gems
class flycheck::checkers::gems {

  require flycheck::ruby

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
  }
}
