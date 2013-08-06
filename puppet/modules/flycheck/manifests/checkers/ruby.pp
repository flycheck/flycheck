# Ruby syntax checkers

class flycheck::checkers::ruby {
  package { ['ruby1.9.1', 'jruby']:
    ensure => latest,
  }

  # Make Ruby 1.9 the default Ruby
  alternatives { 'ruby':
    path    => '/usr/bin/ruby1.9.1',
    require => Package['ruby1.9.1'],
  }

  # Make sure, Ruby is installed and configured before installing any Gems
  Alternatives['ruby'] -> Package<| provider == gem |>

  # Do not install Gems on Travis
  if $::travis {
    notice('Skipping Ruby Gems on Travis CI')
  }
  else {
    include flycheck::checkers::gems
  }
}
