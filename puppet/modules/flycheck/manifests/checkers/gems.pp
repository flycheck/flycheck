# All our Rubygems

class flycheck::checkers::gems {

  # We keep all gems in a separate class to install them separately on Travis
  # CI, as normal user, and not as root.  If we install Gems as root on Travis,
  # RVM screws up, and won't find our gems.

  $gem_requires = $::travis ? {
    # We know that Ruby is available on Travis CI, so we do not need Ruby
    # provisioning from flycheck::ruby.  Thus, this manifest is independent on
    # Travis CI, so we can easily call it as non-root
    undef   => [Class['flycheck::ruby']],
    default => [],
  }

  $gem_packages = [ 'haml',
                    'puppet-lint',
                    'rubocop',     # ruby-rubocop
                    'sass',        # sass/scss
                    'slim',        # slim
                    ]

  package { $gem_packages:
    ensure   => latest,
    provider => gem,
    require  => $gem_requires
  }
}
