# Class: flycheck::emacs
#
# Install Emacs

class flycheck::emacs {

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

}
