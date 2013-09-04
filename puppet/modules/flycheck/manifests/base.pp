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

  $cask_version = '0.4.6'
  $cask_archive = "cask-${cask_version}"

  archive { $cask_archive:
    ensure        => present,
    url           => "https://github.com/cask/cask/archive/v${cask_version}.tar.gz",
    digest_string => 'fecf7ca5db66d3084970a03de5d2f789',
    target        => '/opt/'
  }

  file { '/usr/local/bin/cask':
    ensure  => link,
    target  => "/opt/${cask_archive}/bin/cask",
    require => Archive[$cask_archive],
  }
}
