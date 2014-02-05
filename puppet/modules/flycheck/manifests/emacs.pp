# Class: flycheck::emacs
#
# Install Emacs

class flycheck::emacs {

  apt::ppa { 'ppa:cassou/emacs': }
  apt::ppa { 'ppa:ubuntu-elisp/ppa': } # For up to date snapshot builds

  package { ['emacs24-common', 'emacs24-bin-common', 'emacs24-nox']:
    ensure  => latest,
    require => Apt::Ppa['ppa:cassou/emacs']
  }

  package { ['emacs-snapshot-common', 'emacs-snapshot-nox']:
    ensure  => latest,
    require => Apt::Ppa['ppa:ubuntu-elisp/ppa'],
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
