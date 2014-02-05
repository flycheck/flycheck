# Basic system setup

class flycheck::base {

  $should_use_own_sources_list = !$::travis

  class { 'apt':
    # When not on Travis, construct our own sources.list to pick good mirrors
    purge_sources_list => $should_use_own_sources_list,
  }

  if $should_use_own_sources_list {
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

  # Decrypt encrypted files in unit tests
  package { 'gnupg': ensure => latest }
}
