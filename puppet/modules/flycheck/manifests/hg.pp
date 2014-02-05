# Class: flycheck::hg
#
# Install Mercurial
class flycheck::hg {
  package { 'mercurial':
    ensure  => latest,
    require => Class['apt::update'],
    alias   => 'hg'
  }
}
