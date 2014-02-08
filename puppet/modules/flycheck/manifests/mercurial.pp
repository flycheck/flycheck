# Class: flycheck::mercurial
#
# Install Mercurial
class flycheck::mercurial {
  package { 'mercurial':
    ensure  => latest,
    require => Class['apt::update'],
  }
}
