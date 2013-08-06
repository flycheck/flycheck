# Basic system setup

class flycheck::base {
  include apt

  package { 'make': ensure => latest }

  # Required to add PPAs
  package { 'python-software-properties': ensure => latest }

  # Archive tools to extract Carton and syntax checker archives
  package { ['tar', 'unzip']: ensure => latest }

  # Texinfo building
  package { ['texinfo', 'install-info']: ensure => latest }

  # Emacs packages
  apt::ppa { 'ppa:cassou/emacs': }

  package { ['emacs24-nox', 'emacs-snapshot-nox']:
    ensure  => latest,
    require => Apt::Ppa['ppa:cassou/emacs']
  }

  $carton_version = '0.3.0'

  archive { "carton-${carton_version}":
    ensure        => present,
    url           => "https://github.com/rejeep/carton/archive/v${carton_version}.tar.gz",
    digest_string => '60390687d4f4c3815d27b557b65a036c',
    target        => '/opt/'
  }

  file { '/usr/local/bin/carton':
    ensure  => link,
    target  => "/opt/carton-${carton_version}/bin/carton",
    require => Archive["carton-${carton_version}"],
  }
}
