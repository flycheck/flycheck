# All our Rubygems

class flycheck::checkers::gems {

  # We keep all gems in a separate class to install them separately on Travis
  # CI, as normal user, and not as root.  If we install Gems as root on Travis,
  # RVM screws up, and won't find our gems.

  if defined(Class['flycheck::ruby']) {
    # If Ruby is installed, do so before installing our Gems
    $gem_requires = [Class['flycheck::ruby']]
  }

  $gem_packages = [ 'cucumber',    # cucumber
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
    require  => $gem_requires
  }
}
