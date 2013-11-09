# Basic system setup

class flycheck::base {
  class { 'apt':
    # When not on Travis, construct our own sources.list to pick good mirrors
    purge_sources_list => true,
  }

  # Use Ubuntu mirrors to get fast package downloads
  apt::source { 'ubuntu':
    location => 'mirror://mirrors.ubuntu.com/mirrors.txt',
    release  => 'precise',
    repos    => 'main restricted universe multiverse',
  }

  apt::source { 'ubuntu-security':
    location => 'http://security.ubuntu.com/ubuntu',
    release  => 'precise-security',
    repos    => 'main restricted universe multiverse',
  }

  apt::source { 'ubuntu-updates':
    location => 'mirror://mirrors.ubuntu.com/mirrors.txt',
    release  => 'precise-updates',
    repos    => 'main restricted universe multiverse',
  }

  apt::source { 'ubuntu-backports':
    location => 'mirror://mirrors.ubuntu.com/mirrors.txt',
    release  => 'precise-backports',
    repos    => 'main restricted universe multiverse',
  }

  # Required to add PPAs.  Install first, before re-configuring Apt, to avoid a
  # dependency circle around apt::update
  package { 'python-software-properties':
    ensure  => latest,
    require => [],
    before  => Class['apt'],
  }

  # Update Apt before installing packages
  Package {
    require => Class['apt::update']
  }

  package { 'make': ensure => latest }

  # Archive tools to extract Carton and syntax checker archives
  package { ['tar', 'unzip']: ensure => latest }

  # Texinfo building
  package { ['texinfo', 'install-info']: ensure => latest }

  # Decrypt encrypted files in unit tests
  package { 'gnupg': ensure => latest }

  # Emacs packages
  apt::ppa { 'ppa:cassou/emacs': }

  package { ['emacs24-nox', 'emacs-snapshot-nox']:
    ensure  => latest,
    require => Apt::Ppa['ppa:cassou/emacs']
  }

  $cask_archive = 'cask-master'

  archive { $cask_archive:
    ensure    => present,
    # Don't verify the checksum, since master changes frequently
    checksum  => false,
    url       => 'https://github.com/cask/cask/archive/master.tar.gz',
    target    => '/opt/',
  }

  file { '/usr/local/bin/cask':
    ensure  => link,
    target  => "/opt/${cask_archive}/bin/cask",
    require => Archive[$cask_archive],
  }

  # PIP, to install various checker utilities
  package { 'python': ensure => latest}

  package { 'python-setuptools' :
    ensure  => latest,
    notify  => Exec['base::bootstrap-pip'],
    require => Package['python'],
  }

  exec { 'base::bootstrap-pip':
    command     => '/usr/bin/easy_install -U setuptools pip',
    refreshonly => true,
    require     => Package['python-setuptools'],
  }

  Exec['base::bootstrap-pip'] -> Package <| provider == pip |>
}
