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
