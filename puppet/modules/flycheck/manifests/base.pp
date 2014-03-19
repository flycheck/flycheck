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

  # Update Apt before installing packages
  Package {
    require => Class['apt::update']
  }

  package { 'python-software-properties':
    ensure  => latest,
    # Drop apt::update from Package default, to avoid a cyclic dependency on
    # apt::update, because apt::update in turn will depend on this package,
    # because it has the tools to add PPAs
    require => [],
    before  => Class['apt'],
  }

  # Important utilities
  package { ['curl', 'make']:
    ensure => latest
  }

  # Archive tools to extract Cask and syntax checker archives
  package { ['tar', 'unzip']: ensure => latest }

  # Decrypt encrypted files in unit tests
  package { 'gnupg': ensure => latest }
}
