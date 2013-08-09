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

  $cask_version = '0.4.4'
  $cask_archive = "cask.el-${cask_version}"

  archive { $cask_archive:
    ensure        => present,
    url           => "https://github.com/rejeep/cask.el/archive/v${cask_version}.tar.gz",
    digest_string => 'df9ea7b05a260980ae42e8b8626e73ac',
    target        => '/opt/'
  }

  file { '/usr/local/bin/cask':
    ensure  => link,
    target  => "/opt/${cask_archive}/bin/cask",
    require => Archive["cask.el-${cask_version}"],
  }
}
