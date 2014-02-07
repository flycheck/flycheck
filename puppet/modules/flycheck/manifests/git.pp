# Class: flycheck::git
#
# Install Git
class flycheck::git {
  package { 'git-core':
    ensure  => latest,
    require => Class['apt::update'],
    alias   => 'git'
  }
}
