# Class: flycheck::ruby
#
# Install Ruby, and select Ruby 1.9 as default
class flycheck::ruby {
  require apt::update

  package { ['ruby1.9.1', 'ruby1.9.1-dev', 'jruby']:
    ensure  => latest,
  }

  # Make Ruby 1.9 the default Ruby
  alternatives { 'ruby':
    path    => '/usr/bin/ruby1.9.1',
    require => Package['ruby1.9.1'],
  }

  # To compile Nokogiri, which is a dependency of foodcritic
  $xml_headers = ['libxslt1-dev', 'libxml2-dev']
  package { $xml_headers:
    ensure  => latest,
  }
}
