# All our Rubygems

class flycheck::checkers::gems {

  # We keep all gems in a separate class to install them separately on Travis
  # CI, as normal user, and not as root.  If we install Gems as root on Travis,
  # RVM screws up, and won't find our gems.

  $gem_packages = [ 'haml',
                    'puppet-lint',
                    'rubocop',     # ruby-rubocop
                    'sass',        # sass/scss
                    ]

  package { $gem_packages:
    ensure   => installed,
    provider => gem,
  }
}
